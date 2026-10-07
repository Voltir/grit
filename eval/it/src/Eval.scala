package grit.eval

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.assembly.retrieval.RetrievalAssembler
import grit.core.context.{AssemblyNote, AssemblyRequest, ContextAssembler, Shown, Window}
import grit.core.document.DocWeight
import grit.core.id.{DocumentVersion, EntryId, TurnRef}
import grit.core.message.{AssistantBlock, Cost, Message, StopReason, Tokens, Usage}
import grit.core.period.LifecycleSettings
import grit.core.place.{Locality, Scope, Weight}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.store.Nearby
import grit.core.visibility.Subject
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.TestPostgres
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

    /** Retrieval, its own conversation's hits weighted by `weight` against those elsewhere, and
      * a document's score scaled by `documents`.
      */
    case Retrieval(budget: Tokens, live: Boolean, weight: Double = 2.0, documents: Double = 1.0)
    case Oracle

    def label: String = this match {
      case Linear(b) => s"linear@${Tokens.value(b)}"
      case Retrieval(b, live, w, d) =>
        val weighted = if (w == 2.0) "" else s" w${w.toString.stripSuffix(".0")}"
        val scaled = if (d == 1.0) "" else s" d$d"
        s"retrieval${if (live) "+live" else ""}@${Tokens.value(b)}$weighted$scaled"
      case Oracle => "oracle"
    }
  }

  val Budgets: Vector[Tokens] = Vector(1_000L, 2_000L, 4_000L, 24_000L).map(Tokens(_))

  /** The weights a case with other conversations is also run at, beside the default 2, at
    * the smallest budget, where its own turns and those elsewhere compete for room.
    */
  val Weights: Vector[Double] = Vector(1.0, 4.0)

  /** The document weights a case with documents is also run at, beside 1, at the smallest
    * budget: a tiny one. A weight decides only which candidates a full budget passes over, so
    * where the budget holds every candidate a document is drawn at any weight.
    */
  val DocumentWeights: Vector[Double] = Vector(0.001)

  def strategies(
      live: Boolean,
      elsewhere: Boolean = false,
      documents: Boolean = false
  ): Vector[Strategy] =
    Budgets.map(Strategy.Linear(_)) ++
      Budgets.map(Strategy.Retrieval(_, live = false)) ++
      (if (elsewhere) Weights.map(Strategy.Retrieval(Tokens(1_000), live = false, _))
       else Vector.empty) ++
      (if (documents)
         DocumentWeights.map(d => Strategy.Retrieval(Tokens(1_000), live = false, documents = d))
       else Vector.empty) ++
      (if (live) Budgets.map(Strategy.Retrieval(_, live = true)) else Vector.empty) :+
      Strategy.Oracle

  /** One window, scored: labelled entries and documents it holds, of how many; its estimated
    * size; the labelled entries it missed; the assembler's notes; how many `[never]` entries it
    * holds (`strays`); and the labelled documents it missed.
    */
  final case class Score(
      got: Int,
      of: Int,
      tokens: Tokens,
      missed: Vector[EntryId],
      notes: Vector[AssemblyNote],
      strays: Int = 0,
      missedDocuments: Vector[DocumentVersion] = Vector.empty
  )

  /** A case written into the eval's database as its [[Layout]] lays it out: the turn it asks
    * in, its `[must]` and `[never]` entries by id, every entry's message, the scope a window
    * for it is drawn within, its `[must]` documents by version, and each of its documents as a
    * window shows it.
    */
  final case class Loaded(
      c: Case,
      variant: Variant,
      turn: TurnRef,
      must: Vector[EntryId],
      messages: Map[EntryId, Message],
      never: Vector[EntryId] = Vector.empty,
      scope: Scope = Scope.Off,
      mustDocuments: Vector[DocumentVersion] = Vector.empty,
      documents: Map[DocumentVersion, Message] = Map.empty
  )

  /** `c` as `variant` lays it out, written into `engine`'s database, its documents through
    * [[Load.Plugin]]'s keeper under its unscaled terms.
    */
  def load(engine: Engine^, c: Case, variant: Variant): Loaded = {
    val terms = Load.terms(DocWeight.Unscaled)
    (for {
      layout <- Layout.of(c, variant)
      written <- Load.into(
        engine.jot,
        engine.conversations,
        engine.entries,
        engine.periods,
        engine.keeper(Load.Plugin, terms)
      )(layout)
      kept <- engine.db
        .read(Subject.Public)(engine.documents.read(written.documents))
        .left
        .map(e => s"${c.name}: $e")
    } yield {
      val rows = layout.entries
      Loaded(
        c,
        variant,
        written.turn,
        rows.filter(_.line.must).map(_.id),
        rows.map(r => r.id -> Layout.message(r.line)).toMap,
        rows.filter(_.line.never).map(_.id),
        layout.scope,
        layout.documents.zip(written.documents).collect { case (d, v) if d.must => v },
        kept.map(d => d.version -> Shown.document(d, terms.label)).toMap
      )
    }).fold(e => sys.error(s"eval: $e"), identity)
  }

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
      case Strategy.Retrieval(_, _, w, _) =>
        Weight.of(w).fold(e => sys.error(s"eval: $e"), identity)
      case _ => Weight.Default
    }
    // The documents' terms in force, as an engine's start declares them: at the strategy's
    // weight.
    val documents = strategy match {
      case Strategy.Retrieval(_, _, _, d) =>
        DocWeight.of(d).fold(e => sys.error(s"eval: $e"), identity)
      case _ => DocWeight.Unscaled
    }
    engine.jot
      .write(Subject.Public)(engine.documents.declare(Vector(Load.Plugin -> Load.terms(documents))))
      .fold(e => sys.error(s"eval: $e"), identity)
    val d = LifecycleSettings.Default
    engine.jot
      .write(Subject.Public)(
        engine.lifecycle.set(
          LifecycleSettings
            .of(d.windows, d.balance, d.settle, d.resolveAt, d.asks, Locality(loaded.scope, weight))
            .fold(e => sys.error(s"eval: $e"), identity)
        )
      )
      .fold(e => sys.error(s"eval: $e"), identity)
    def assemble(assembler: ContextAssembler^): Either[String, Score] =
      assembler
        .assemble(request)(using engine.db)
        .left
        .map(e => s"${strategy.label}: $e")
        .flatMap(w => held(engine, loaded, w).map(score(loaded, _, w.notes, w.documents)))
    strategy match {
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
      case Strategy.Retrieval(budget, false, _, _) =>
        loaded.c.query.toRight(s"${loaded.c.name} has no query:").flatMap { q =>
          assemble(retrieval(engine, new Handwritten(q), budget))
        }
      case Strategy.Retrieval(budget, true, _, _) => assemble(retrieval(engine, live, budget))
      case Strategy.Oracle =>
        Right(score(loaded, loaded.must, documents = loaded.mustDocuments))
    }
  }

  private def retrieval(
      engine: Engine^,
      writer: Provider^,
      budget: Tokens
  ): ContextAssembler^{writer} =
    new RetrievalAssembler(
      engine.entries,
      engine.conversations,
      engine.periods,
      engine.principals,
      engine.lifecycle,
      engine.search,
      engine.documents,
      engine.stitches,
      writer,
      CharEstimate,
      budget,
      Tokens(Tokens.value(budget) / 3),
      grit.core.stitch.Tuning.Default
    )

  /** The ids of the entries `window` names, read from `engine`'s store: its own, in
    * `loaded`'s conversation, then its nearby sections'. One gone is left out.
    */
  private def held(
      engine: Engine^,
      loaded: Loaded,
      window: Window
  ): Either[String, Vector[EntryId]] =
    engine.db
      .read(Subject.Public)(
        for {
          own <- engine.entries.at(loaded.turn.conversationId, window.entries)
          near <- Nearby.read(window.nearby, engine.entries)
        } yield (own ++ near).map(_.id)
      )
      .left
      .map(e => s"eval: $e")

  /** A window holding the entries `shown` and the documents `documents`, scored against
    * `loaded`'s labels, with the notes its assembler wrote: its size is each held message's
    * estimate, a document's as the window shows it.
    */
  def score(
      loaded: Loaded,
      shown: Vector[EntryId],
      notes: Vector[AssemblyNote] = Vector.empty,
      documents: Vector[DocumentVersion] = Vector.empty
  ): Score = {
    val held = shown.toSet
    val kept = documents.toSet
    Score(
      loaded.must.count(held) + loaded.mustDocuments.count(kept),
      loaded.must.size + loaded.mustDocuments.size,
      (shown.flatMap(loaded.messages.get) ++ documents.flatMap(loaded.documents.get))
        .map(CharEstimate.message)
        .foldLeft(Tokens.Zero)(_ + _),
      loaded.must.filterNot(held),
      notes,
      loaded.never.count(held),
      loaded.mustDocuments.filterNot(kept)
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
    val cases =
      (Cases.all ++ Cases.crossPlace ++ Cases.documented).map((name, text) =>
        Case.parse(name, text)
      )
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
        val loaded = load(engine, c, variant)
        val scores =
          strategies(live.nonEmpty, c.elsewhere.nonEmpty, c.documents.nonEmpty).map(s =>
            s -> run(engine, loaded, s, writer)
          )
        report(loaded, scores)
        (Group.of(c), variant, scores)
      }
      // The single-conversation cases' totals stay comparable with every earlier report.
      totals(
        "Totals: labelled entries held, over every case with one conversation",
        results.collect { case (Group.One, v, s) =>
          (v, s)
        }
      )
      totals(
        "Cross-place totals: labelled entries held, and [never] entries held (strays)",
        results.collect { case (Group.CrossPlace, v, s) =>
          (v, s)
        }
      )
      totals(
        "Document totals: labelled entries and documents held",
        results.collect { case (Group.Documented, v, s) =>
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

  /** Which totals a case is counted in: one conversation's, cross-place, or with documents. */
  private enum Group {
    case One, CrossPlace, Documented
  }

  private object Group {
    def of(c: Case): Group =
      if (c.documents.nonEmpty) Documented else if (c.elsewhere.nonEmpty) CrossPlace else One
  }

  private def report(loaded: Loaded, scores: Vector[(Strategy, Either[String, Score])]): Unit = {
    val c = loaded.c
    val size = loaded.messages.size
    println(s"${c.name} (${loaded.variant.label}, $size entries): ${c.about}")
    scores.foreach {
      case (s, Left(e)) => println(f"  ${s.label}%-22s failed: $e")
      case (s, Right(r)) =>
        val gists = r.missed.map(id => gist(loaded, id)) ++
          r.missedDocuments.map(v => s"document ${DocumentVersion.value(v)}")
        val missed = if (gists.isEmpty) "" else gists.mkString("  missed: ", "; ", "")
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
