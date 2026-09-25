package grit.eval

import java.time.Instant

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.assembly.retrieval.RetrievalAssembler
import grit.core.context.{AssemblyNote, AssemblyRequest, ContextAssembler, Window}
import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Cost, Message, StopReason, Tokens, Usage}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.store.{Entry, Origin, Payload}
import grit.dbos.engine.Engine
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.models.{ModelRole, OpenRouterConfig, OpenRouterProvider}

/** The assembly eval (roadmap/mechanisms/assembly-eval.md): every case in [[Cases]], as
  * written and buried in filler, under every strategy, scored on how many of its `[must]`
  * entries the window holds and what the window costs in estimated tokens. It runs in a
  * throwaway Postgres (Testcontainers), so retrieval ranks with the real pg_textsearch. A
  * report, not a gate: the tests pin the harness, never the scores.
  *
  * {{{./mill grit.eval.test.runMain grit.eval.Eval [--live]}}}
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
    case Retrieval(budget: Tokens, live: Boolean)
    case Oracle

    def label: String = this match {
      case Linear(b) => s"linear@${Tokens.value(b)}"
      case Retrieval(b, false) => s"retrieval@${Tokens.value(b)}"
      case Retrieval(b, true) => s"retrieval+live@${Tokens.value(b)}"
      case Oracle => "oracle"
    }
  }

  val Budgets: Vector[Tokens] = Vector(1_000L, 2_000L, 4_000L, 24_000L).map(Tokens(_))

  def strategies(live: Boolean): Vector[Strategy] =
    Budgets.map(Strategy.Linear(_)) ++
      Budgets.map(Strategy.Retrieval(_, live = false)) ++
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
    * labelled entries it missed; and the assembler's notes.
    */
  final case class Score(
      got: Int,
      of: Int,
      tokens: Tokens,
      missed: Vector[EntryId],
      notes: Vector[AssemblyNote]
  )

  /** A case written into the eval's database, one entry per line, ids
    * `{case}/{variant}/t{turn}:{seq}`.
    */
  final case class Loaded(
      c: Case,
      variant: Variant,
      turn: TurnRef,
      must: Vector[EntryId],
      messages: Map[EntryId, Message]
  )

  def load(engine: Engine^, config: DbConfig, c: Case, variant: Variant): Loaded = {
    val conversation: ConversationId =
      engine
        .conversation(Origin.Task("eval", s"${c.name}/${variant.label}"))
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
    Loaded(
      c,
      variant,
      TurnRef(conversation, TurnSeq((turns.size - 1).toLong)),
      lines.collect { case (id, _, _, line) if line.must => id },
      lines.map((id, _, _, line) => id -> message(line)).toMap
    )
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
    def assemble(assembler: ContextAssembler^): Either[String, Window] =
      assembler.assemble(request)(using engine.db).left.map(e => s"${strategy.label}: $e")
    val window: Either[String, Window] = strategy match {
      case Strategy.Linear(budget) =>
        assemble(new LinearAssembler(engine.entries, CharEstimate, budget))
      case Strategy.Retrieval(budget, false) =>
        loaded.c.query.toRight(s"${loaded.c.name} has no query:").flatMap { q =>
          assemble(retrieval(engine, new Handwritten(q), budget))
        }
      case Strategy.Retrieval(budget, true) => assemble(retrieval(engine, live, budget))
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
      engine.search,
      writer,
      CharEstimate,
      budget,
      Tokens(Tokens.value(budget) / 3)
    )

  def score(loaded: Loaded, window: Window): Score = {
    val held = window.entries.toSet
    Score(
      loaded.must.count(held),
      loaded.must.size,
      window.entries
        .flatMap(loaded.messages.get)
        .map(CharEstimate.message)
        .foldLeft(Tokens.Zero)(_ + _),
      loaded.must.filterNot(held),
      window.notes
    )
  }

  def main(args: Array[String]): Unit = {
    val live: Option[OpenRouterConfig] =
      if (!args.contains("--live")) None
      else
        OpenRouterConfig.fromEnv(sys.env, ModelRole.Query) match {
          case Right(c) => Some(c)
          case Left(invalid) =>
            System.err.println(s"--live: ${invalid.message}")
            sys.exit(2)
        }
    val cases = Cases.all.map((name, text) => Case.parse(name, text))
    cases.collect { case Left(e) => e }.foreach(e => println(s"unreadable case: $e"))
    val model: Provider^ = live match {
      case Some(c) => new OpenRouterProvider(c)
      case None => NoLive
    }
    val writer = new Once(model)
    val config = TestPostgres.freshDatabase("eval")
    val engine = Engine.open(config, "eval")
    try {
      val results = for {
        c <- cases.collect { case Right(c) => c }
        variant <- Variant.values.toVector
      } yield {
        val loaded = load(engine, config, c, variant)
        val scores = strategies(live.nonEmpty).map(s => s -> run(engine, loaded, s, writer))
        report(loaded, scores)
        (variant, scores)
      }
      totals(results)
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
        println(
          f"  ${s.label}%-22s ${r.got}/${r.of} must  ${Tokens.value(r.tokens)}%5d tokens$missed${noted(r.notes)}"
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
      results: Vector[(Variant, Vector[(Strategy, Either[String, Score])])]
  ): Unit = {
    println("Totals: labelled entries held, over every case")
    Variant.values.foreach { v =>
      val mine = results.filter(_._1 == v)
      val byStrategy = mine.flatMap(_._2).groupBy(_._1.label)
      println(s"  ${v.label}")
      mine.headOption.toVector.flatMap(_._2.map(_._1)).foreach { s =>
        val rs = byStrategy.getOrElse(s.label, Vector.empty).collect { case (_, Right(r)) => r }
        println(f"    ${s.label}%-22s ${rs.map(_.got).sum}/${rs.map(_.of).sum}")
      }
    }
  }

  private def gist(loaded: Loaded, id: EntryId): String =
    loaded.messages.get(id).fold(EntryId.value(id)) {
      case Message.User(text) => s"\"${text.take(40)}\""
      case Message.Assistant(blocks, _, _, _) =>
        blocks.collect { case AssistantBlock.Text(t) => s"\"${t.take(40)}…\"" }.mkString
      case Message.ToolResult(_, content, _) => s"\"${content.take(40)}\""
    }
}
