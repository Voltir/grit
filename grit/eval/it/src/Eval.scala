package grit.eval

import java.time.Instant

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.assembly.retrieval.RetrievalAssembler
import grit.core.context.{AssemblyNote, AssemblyRequest, ContextAssembler, Window}
import grit.core.id.{
  CloseRef,
  ConversationId,
  EntryId,
  PeriodRef,
  PeriodSeq,
  PrincipalId,
  TurnRef,
  TurnSeq
}
import grit.core.message.{AssistantBlock, Cost, Message, StopReason, Tokens, Usage}
import grit.core.period.{Balance, CloseReason, Closing, Edit, Flows, Ground, LifecycleSettings}
import grit.core.place.{Directory, Locality, Namespace, Place, Scope, Weight}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.store.{Entry, Origin, Payload}
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.models.{ModelRole, OpenRouterConfig, OpenRouterProvider}

/** The assembly eval (roadmap/mechanisms/assembly-eval.md): every case in [[Cases]], as
  * written and buried in filler, under every strategy, scored on how many of its `[must]`
  * entries the window holds and what the window costs in estimated tokens. It runs in a
  * throwaway Postgres (`TestPostgres`), so retrieval ranks with the real pg_textsearch. A
  * report, not a gate: the tests pin the harness, never the scores.
  *
  * {{{./mill grit.eval.it.runMain grit.eval.Eval [--live]}}}
  *
  * Retrieval uses each case's handwritten `query:`; with `--live` it also runs with the
  * query written by the Query role's model (OpenRouter, `OPENROUTER_API_KEY`), once per
  * case and variant, and reports what those calls cost.
  */
object Eval {

  /** What builds a window. `Oracle` is exactly the labelled entries: the cheapest complete
    * window, the floor the others are measured against.
    */
  enum Strategy {
    case Linear(budget: Tokens)

    /** Retrieval, its own conversation's hits weighted by `weight` against those elsewhere. */
    case Retrieval(budget: Tokens, live: Boolean, weight: Double = 2.0)
    case Oracle

    def label: String = this match {
      case Linear(b) => s"linear@${Tokens.value(b)}"
      case Retrieval(b, live, w) =>
        val weighted = if (w == 2.0) "" else s" w${w.toString.stripSuffix(".0")}"
        s"retrieval${if (live) "+live" else ""}@${Tokens.value(b)}$weighted"
      case Oracle => "oracle"
    }
  }

  val Budgets: Vector[Tokens] = Vector(1_000L, 2_000L, 4_000L, 24_000L).map(Tokens(_))

  /** The weights a case with other conversations is also run at, beside the default 2, at
    * the smallest budget, where its own turns and those elsewhere compete for room.
    */
  val Weights: Vector[Double] = Vector(1.0, 4.0)

  def strategies(live: Boolean, elsewhere: Boolean = false): Vector[Strategy] =
    Budgets.map(Strategy.Linear(_)) ++
      Budgets.map(Strategy.Retrieval(_, live = false)) ++
      (if (elsewhere) Weights.map(Strategy.Retrieval(Tokens(1_000), live = false, _))
       else Vector.empty) ++
      (if (live) Budgets.map(Strategy.Retrieval(_, live = true)) else Vector.empty) :+
      Strategy.Oracle

  /** A case as written, or with [[FillerPerGap]] filler turns after each of its turns. */
  enum Variant {
    case Plain, Buried

    def label: String = this match {
      case Plain => "plain"
      case Buried => "buried"
    }
  }

  val FillerPerGap = 40

  /** One window, scored: labelled entries it holds, of how many; its estimated size; the
    * labelled entries it missed; the assembler's notes; and how many `[never]` entries it
    * holds (`strays`).
    */
  final case class Score(
      got: Int,
      of: Int,
      tokens: Tokens,
      missed: Vector[EntryId],
      notes: Vector[AssemblyNote],
      strays: Int = 0
  )

  /** A case written into the eval's database, one entry per line, ids
    * `{case}/{variant}/t{turn}:{seq}`; each other conversation's `{case}/{variant}/p{i}/t{turn}:{seq}`,
    * its periods opened (and closed) as written, its place under the case's own root
    * (`fs:/eval/{case}/{variant}`, `task:eval/{case}/{variant}`), and the case's scope (or
    * the whole root) within it.
    */
  final case class Loaded(
      c: Case,
      variant: Variant,
      turn: TurnRef,
      must: Vector[EntryId],
      messages: Map[EntryId, Message],
      never: Vector[EntryId] = Vector.empty,
      scope: Scope = Scope.Off
  )

  def load(engine: Engine^, config: DbConfig, c: Case, variant: Variant): Loaded = {
    val conversation: ConversationId =
      engine
        .conversation(Origin.Task("eval", s"${c.name}/${variant.label}"), PrincipalId.Local)
        .fold(e => sys.error(s"eval: $e"), identity)
    val body = variant match {
      case Variant.Plain => c.turns
      case Variant.Buried =>
        c.turns.zipWithIndex.flatMap((turn: Vector[Case.Line], i: Int) =>
          turn +: Filler.turns(c.name.hashCode.toLong * 31 + i, FillerPerGap)
        )
    }
    val turns = body :+ Vector(Case.Line(you = true, c.ask, must = false))
    val lines = turns.zipWithIndex
      .flatMap((turn: Vector[Case.Line], t: Int) => turn.map(line => (t, line)))
      .zipWithIndex
      .map { case ((t, line), seq) =>
        (EntryId(s"${c.name}/${variant.label}/t$t:$seq"), t, seq, line)
      }
    LiveDb.transaction(config) {
      lines.foreach { (id, t, seq, line) =>
        engine.entries
          .insert(
            Entry(
              id,
              conversation,
              TurnSeq(t.toLong),
              None,
              seq.toLong,
              Payload.Message(message(line)),
              Instant.EPOCH
            )
          )
          .fold(e => sys.error(s"eval: $e"), identity)
      }
    }
    val others = c.elsewhere.zipWithIndex.map((e: Case.Elsewhere, i: Int) =>
      elsewhere(engine, config, c, variant, e, i)
    )
    val theirs = others.flatten
    val root = Vector("eval", c.name, variant.label)
    val scope = c.scope.fold(Scope(Vector(rooted(root, "fs:/"), rooted(root, "task:"))))(written =>
      Scope(written.split("\\s+").toVector.map(rooted(root, _)))
    )
    Loaded(
      c,
      variant,
      TurnRef(conversation, TurnSeq((turns.size - 1).toLong)),
      lines.collect { case (id, _, _, line) if line.must => id } ++
        theirs.collect { case (id, line) if line.must => id },
      lines.map((id, _, _, line) => id -> message(line)).toMap ++
        theirs.map((id, line) => id -> message(line)),
      (lines.collect { case (id, _, _, line) if line.never => id } ++
        theirs.collect { case (id, line) if line.never => id }),
      scope
    )
  }

  /** `written` (a place as a case writes it) under the case's `root`: `fs:/a` as
    * `fs:/eval/{case}/{variant}/a`, `task:a` as `task:eval/{case}/{variant}/a`.
    */
  private def rooted(root: Vector[String], written: String): Place =
    Place.read(written) match {
      case Right(p) =>
        p.segments.headOption.flatMap(Namespace.of) match {
          case Some(ns) => Place.under(ns, root ++ p.segments.drop(1))
          case None => sys.error(s"eval: a case's place must name a namespace: $written")
        }
      case Left(why) => sys.error(s"eval: $why")
    }

  /** The origin whose place is `place`: a TUI session for `fs`, a task's run for `task`. */
  private def originAt(place: Place): Origin =
    place.segments.headOption.flatMap(Namespace.of) match {
      case Some(Namespace.Fs) =>
        Origin.Tui(
          Directory
            .of("/" + place.segments.drop(1).mkString("/"))
            .fold(e => sys.error(s"eval: $e"), identity),
          "eval"
        )
      case Some(Namespace.Task) =>
        Origin.Task(
          place.segments.drop(1).headOption.getOrElse("eval"),
          place.segments.drop(2).mkString("/")
        )
      case _ => sys.error(s"eval: a case's place is fs: or task:, not ${place.written}")
    }

  /** The `i`-th other conversation of `c`, written into the database: its first period's
    * turns, open or closed with its carried lines, then any turns of its second; each
    * entry's id and line.
    */
  private def elsewhere(
      engine: Engine^,
      config: DbConfig,
      c: Case,
      variant: Variant,
      e: Case.Elsewhere,
      i: Int
  ): Vector[(EntryId, Case.Line)] = {
    val root = Vector("eval", c.name, variant.label)
    val conversation = engine
      .conversation(originAt(rooted(root, e.place)), PrincipalId.Local)
      .fold(x => sys.error(s"eval: $x"), identity)
    def body(turns: Vector[Vector[Case.Line]], seed: Long) = variant match {
      case Variant.Plain => turns
      case Variant.Buried =>
        turns.zipWithIndex.flatMap((turn: Vector[Case.Line], t: Int) =>
          turn +: Filler.turns(seed + t, FillerPerGap / 4)
        )
    }
    def write(
        turns: Vector[Vector[Case.Line]],
        from: Int,
        seqFrom: Int
    ): Vector[(EntryId, Case.Line, Int, Int)] =
      turns.zipWithIndex
        .flatMap((turn: Vector[Case.Line], t: Int) => turn.map(line => (from + t, line)))
        .zipWithIndex
        .map { case ((t, line), k) =>
          (EntryId(s"${c.name}/${variant.label}/p$i/t$t:${seqFrom + k}"), line, t, seqFrom + k)
        }
    val first = write(body(e.turns, c.name.hashCode.toLong * 17 + i), 0, 0)
    val firstTurns = first.map(_._3).maxOption.fold(0)(_ + 1)
    val second =
      write(body(e.reopened, c.name.hashCode.toLong * 19 + i), firstTurns, first.size + 1)
    def insert(rows: Vector[(EntryId, Case.Line, Int, Int)]): Unit =
      LiveDb.transaction(config) {
        rows.headOption.foreach(r =>
          engine.periods.openFor(conversation, TurnSeq(r._3.toLong), Instant.EPOCH)
        )
        rows.foreach { (id, line, t, seq) =>
          engine.entries
            .insert(
              Entry(
                id,
                conversation,
                TurnSeq(t.toLong),
                None,
                seq.toLong,
                Payload.Message(message(line)),
                Instant.EPOCH
              )
            )
            .fold(x => sys.error(s"eval: $x"), identity)
        }
      }
    insert(first)
    if (e.closed) {
      val p1 = PeriodRef(conversation, PeriodSeq.First)
      val carried =
        Balance.empty.edit(e.carried.map(Edit.Stand(_, Ground.Person)), PeriodSeq.First).balance
      val closing = Closing(
        Flows.of(s"${e.place} closed.", None, Vector.empty).getOrElse(sys.error("eval: flows")),
        carried
      )
      LiveDb
        .transaction(config)(
          engine.periods.seal(
            CloseRef(p1, TurnSeq((firstTurns - 1).toLong), Instant.EPOCH),
            CloseReason.Lapsed,
            closing,
            Instant.EPOCH
          )
        )
        .fold(x => sys.error(s"eval: $x"), identity)
    }
    insert(second)
    (first ++ second).map(r => r._1 -> r._2)
  }

  private def message(line: Case.Line): Message =
    if (line.you) Message.User(line.text)
    else
      Message.Assistant(
        Vector(AssistantBlock.Text(line.text)),
        StopReason.EndTurn,
        Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
        "eval"
      )

  /** Answers every request with the case's handwritten query, at no cost. */
  private final class Handwritten(query: String) extends Provider {
    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
      Right(
        Message.Assistant(
          Vector(AssistantBlock.Text(query)),
          StopReason.EndTurn,
          Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0))),
          "handwritten"
        )
      )
  }

  /** `underlying`, asked each distinct request once: the budgets of one case share a query. */
  final class Once(underlying: Provider^) extends Provider {
    @caps.unsafe.untrackedCaptures
    private var answered = Map.empty[ModelRequest, Either[ProviderError, Message.Assistant]]

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
      answered.getOrElse(
        request, {
          val reply = underlying.complete(request)
          answered = answered.updated(request, reply)
          reply
        }
      )

    /** What the calls made so far cost, as their providers reported it. */
    def spent: Cost = Cost.total(answered.values.collect { case Right(m) => m.usage })
  }

  /** Answers nothing: the query writer when the eval runs without `--live`. */
  object NoLive extends Provider {
    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
      Left(ProviderError.Unavailable("no query model: run with --live"))
  }

  /** The window `strategy` builds for `loaded`, scored against its labels. `live` writes
    * the query for `Retrieval(_, live = true)`.
    */
  def run(
      engine: Engine^,
      loaded: Loaded,
      strategy: Strategy,
      live: Provider^
  ): Either[String, Score] = {
    val request = AssemblyRequest(loaded.turn)
    // The settings are one row for the whole database: each run sets the case's scope, so
    // no case sees another's conversations, and its strategy's weight.
    val weight = strategy match {
      case Strategy.Retrieval(_, _, w) => Weight.of(w).fold(e => sys.error(s"eval: $e"), identity)
      case _ => Weight.Default
    }
    val d = LifecycleSettings.Default
    engine.jot
      .write(
        engine.lifecycle.set(
          LifecycleSettings
            .of(d.windows, d.balance, d.settle, d.resolveAt, d.asks, Locality(loaded.scope, weight))
            .fold(e => sys.error(s"eval: $e"), identity)
        )
      )
      .fold(e => sys.error(s"eval: $e"), identity)
    def assemble(assembler: ContextAssembler^): Either[String, Window] =
      assembler.assemble(request)(using engine.db).left.map(e => s"${strategy.label}: $e")
    val window: Either[String, Window] = strategy match {
      case Strategy.Linear(budget) =>
        assemble(
          new LinearAssembler(
            engine.entries,
            engine.periods,
            engine.principals,
            CharEstimate,
            budget
          )
        )
      case Strategy.Retrieval(budget, false, _) =>
        loaded.c.query.toRight(s"${loaded.c.name} has no query:").flatMap { q =>
          assemble(retrieval(engine, new Handwritten(q), budget))
        }
      case Strategy.Retrieval(budget, true, _) => assemble(retrieval(engine, live, budget))
      case Strategy.Oracle => Right(Window(loaded.must))
    }
    window.map(w => score(loaded, w))
  }

  private def retrieval(
      engine: Engine^,
      writer: Provider^,
      budget: Tokens
  ): ContextAssembler^{writer} =
    new RetrievalAssembler(
      engine.entries,
      engine.periods,
      engine.principals,
      engine.lifecycle,
      engine.search,
      writer,
      CharEstimate,
      budget,
      Tokens(Tokens.value(budget) / 3)
    )

  /** `window` scored against `loaded`'s labels: its own entries and its nearby sections'
    * alike count as held; its size is each held message's estimate.
    */
  def score(loaded: Loaded, window: Window): Score = {
    val all = window.entries ++ window.nearby.flatMap(_.entries)
    val held = all.toSet
    Score(
      loaded.must.count(held),
      loaded.must.size,
      all
        .flatMap(loaded.messages.get)
        .map(CharEstimate.message)
        .foldLeft(Tokens.Zero)(_ + _),
      loaded.must.filterNot(held),
      window.notes,
      loaded.never.count(held)
    )
  }

  def main(args: Array[String]): Unit = {
    val live: Option[OpenRouterConfig] =
      if (!args.contains("--live")) None
      else
        OpenRouterConfig.forRole(sys.env, ModelRole.Query) match {
          case Right(c) => Some(c)
          case Left(invalid) =>
            System.err.println(s"--live: ${invalid}")
            sys.exit(2)
        }
    val cases = (Cases.all ++ Cases.crossPlace).map((name, text) => Case.parse(name, text))
    cases.collect { case Left(e) => e }.foreach(e => println(s"unreadable case: $e"))
    val model: Provider^ = live match {
      case Some(c) => new OpenRouterProvider(c)
      case None => NoLive
    }
    val writer = new Once(model)
    val config = TestPostgres.freshDatabase("eval")
    val engine = LiveEngine.open(config, "eval")
    try {
      val results = for {
        c <- cases.collect { case Right(c) => c }
        variant <- Variant.values.toVector
      } yield {
        val loaded = load(engine, config, c, variant)
        val scores =
          strategies(live.nonEmpty, c.elsewhere.nonEmpty).map(s =>
            s -> run(engine, loaded, s, writer)
          )
        report(loaded, scores)
        (c.elsewhere.nonEmpty, variant, scores)
      }
      // The single-conversation cases' totals stay comparable with every earlier report.
      totals(
        "Totals: labelled entries held, over every case with one conversation",
        results.collect { case (false, v, s) =>
          (v, s)
        }
      )
      totals(
        "Cross-place totals: labelled entries held, and [never] entries held (strays)",
        results.collect { case (true, v, s) =>
          (v, s)
        }
      )
      live.foreach { c =>
        val spent = writer.spent match {
          case Cost.Exact(usd) => s"$$${usd.bigDecimal.toPlainString}"
          case Cost.AtLeast(usd) => s"at least $$${usd.bigDecimal.toPlainString}"
        }
        println(s"Query model ${c.model}: $spent in all")
      }
    } finally engine.close()
  }

  private def report(loaded: Loaded, scores: Vector[(Strategy, Either[String, Score])]): Unit = {
    val c = loaded.c
    val size = loaded.messages.size
    println(s"${c.name} (${loaded.variant.label}, $size entries): ${c.about}")
    scores.foreach {
      case (s, Left(e)) => println(f"  ${s.label}%-22s failed: $e")
      case (s, Right(r)) =>
        val missed =
          if (r.missed.isEmpty) ""
          else r.missed.map(id => gist(loaded, id)).mkString("  missed: ", "; ", "")
        val strays = if (loaded.never.isEmpty) "" else s"  ${r.strays}/${loaded.never.size} strays"
        println(
          f"  ${s.label}%-22s ${r.got}/${r.of} must  ${Tokens.value(r.tokens)}%5d tokens$strays$missed${noted(r.notes)}"
        )
    }
    println()
  }

  private def noted(notes: Vector[AssemblyNote]): String =
    notes.map {
      case AssemblyNote.Queried(q, "handwritten", _, _) => ""
      case AssemblyNote.Queried(q, _, _, _) => s"  query: \"${q.take(70)}\""
      case AssemblyNote.FellBack(why) => s"  fell back: $why"
      case AssemblyNote.Recalled(turns) => s"  recalled ${turns.size} turn(s)"
    }.mkString

  private def totals(
      title: String,
      results: Vector[(Variant, Vector[(Strategy, Either[String, Score])])]
  ): Unit = {
    println(title)
    Variant.values.foreach { v =>
      val mine = results.filter(_._1 == v)
      val byStrategy = mine.flatMap(_._2).groupBy(_._1.label)
      println(s"  ${v.label}")
      mine.headOption.toVector.flatMap(_._2.map(_._1)).foreach { s =>
        val rs = byStrategy.getOrElse(s.label, Vector.empty).collect { case (_, Right(r)) => r }
        println(
          f"    ${s.label}%-22s ${rs.map(_.got).sum}/${rs.map(_.of).sum}  strays ${rs.map(_.strays).sum}"
        )
      }
    }
  }

  private def gist(loaded: Loaded, id: EntryId): String =
    loaded.messages.get(id).fold(EntryId.value(id)) {
      case Message.User(text) => s"\"${text.take(40)}\""
      case Message.Assistant(blocks, _, _, _, _) =>
        blocks.collect { case AssistantBlock.Text(t) => s"\"${t.take(40)}…\"" }.mkString
      case Message.ToolResult(_, content, _) => s"\"${content.take(40)}\""
    }
}
