package grit.eval.harness.reply

import java.nio.file.Files
import java.time.{Duration, Instant}

import scala.concurrent.duration.*

import grit.assembly.estimate.CharEstimate
import grit.assembly.retrieval.RetrievalAssembler
import grit.core.clock.{Clock, Fresh}
import grit.core.context.{AssemblyNote, Retrieved, Width}
import grit.core.document.{DocLabel, DocText, DocWeight, DocumentTerms, Written}
import grit.core.durable.Durable
import grit.core.id.{
  CloseRef,
  ConversationId,
  DocKey,
  DocumentVersionId,
  PluginName,
  QuestionName,
  SourceId,
  TurnRef,
  TurnSeq,
  WorkflowId
}
import grit.core.identity.Account
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Catalog, Pinned}
import grit.core.period.{CloseReason, TestClosings}
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError}
import grit.core.speech.{Reach, Speaking}
import grit.core.stitch.{StitchReads, Tuning}
import grit.core.store.{Nearby, Origin}
import grit.core.tool.Toolbox
import grit.core.triage.Corpora
import grit.core.visibility.Integrity
import grit.core.visibility.{Label, Level, Subject}
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.internal.Reader
import grit.dbos.sql.TestPostgres
import grit.eval.harness.capture.{Digest, Dump, KnowledgeJson, TurnCapture}
import grit.eval.harness.jev.{Asking, Budget}
import grit.eval.harness.log.Cache
import grit.lifecycle.stitch.{Stitch, StitchEnv}
import grit.lifecycle.triage.{Triage, TriageEnv, TriageRecords, TriageSpeech}
import grit.models.{OpenRouterConfig, StubClassifier, StubModels}
import grit.turn.{
  Turn,
  TurnEnv,
  TurnHosting,
  TurnLoop,
  TurnRecord,
  TurnRecords,
  TurnRelevance,
  TurnSpeech,
  TurnStitching,
  TurnTooling
}

import Queries.given
import TurnRelevance.Verdict
import utest.*

/** Windows rebuilt over a database a live engine ran turns on, each turn's window drawn by
  * the shipped retrieval assembler, its query writer answering "Falcon budget"; and heard
  * messages triaged by the stub classifier, never drafted.
  */
object RebuildTests extends TestSuite {

  private val nothing = (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id)

  private def right[E, A](e: Either[E, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why.toString), identity)

  /** The system clock's now. */
  private def now(): Instant = Clock.system().now()

  private def eventually(done: => Boolean): Boolean = {
    val clock = Clock.system()
    val until = clock.millis() + 30.seconds.toMillis
    var held = done
    while (!held && clock.millis() < until) { clock.sleep(50.millis); held = done }
    held
  }

  private def answer(text: String): Message.Assistant = Message.Assistant(
    Vector(AssistantBlock.Text(text)),
    StopReason.EndTurn,
    Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.0001"))),
    "test/query"
  )

  /** The stub's models, but every query written by a [[Writer]] of its own: the query its own
    * pair's, so the turn's query call is told from its summary's.
    */
  private object Writing extends Models {
    private val query =
      grit.core.model.Assignment(StubModels.Ref, 1000, None)
    private val inForce: Catalog = Catalog.of(
      StubModels.Catalog.policy.copy(query = grit.core.model.Ladder.one(query)),
      Vector.empty
    )
    private val stub = new StubModels()
    def catalog(): Either[String, Catalog] = Right(inForce)
    def provider(pinned: Pinned): Provider^ =
      if (pinned.assignment == query) new Writer else stub.provider(pinned)
  }

  /** A query writer answering "Falcon budget", counting what it is asked. */
  private final class Writer extends Provider {
    // Untracked: a Writer is either made by [[Writing]] for one call, its count read by no
    // one, or made by the one test that reads it, handed to the two answers that test runs
    // one after the other on its own thread, and read once both return. No other code holds
    // either, so no write to it is seen elsewhere.
    @caps.unsafe.untrackedCaptures
    var asked: Int = 0
    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      asked += 1
      Right(answer("Falcon budget"))
    }
  }

  /** A turn's judge that rejects, at 0.1, each candidate whose text names a rumour, and passes
    * the rest at 0.9.
    */
  private final class Sceptic extends grit.core.classify.Classifier {
    protected def answer(
        state: ujson.Value,
        questions: Vector[grit.core.classify.Question]
    ): Either[grit.core.classify.ClassifierError, grit.core.classify.Answers] = {
      val candidates = state.objOpt
        .flatMap(_.get("candidates"))
        .flatMap(_.arrOpt)
        .fold(Vector.empty[String])(_.toVector.flatMap(_.strOpt))
      Right(
        grit.core.classify.Answers(
          questions.indices.toVector.map(i =>
            grit.core.classify.Answer
              .YesNo(if (candidates.lift(i).exists(_.contains("rumour"))) 0.1 else 0.9)
          ),
          Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0))),
          "test/sceptic"
        )
      )
    }
  }

  private def launch(engine: Engine^): Unit =
    launch(engine, grit.core.classify.ClearedClassifier.none("no classifier"))

  /** The turn, triage and stitch workflows launched on `engine`, each turn judging what its
    * search found by `judge`.
    */
  private def launch(engine: Engine^, judge: grit.core.classify.ClearedClassifier^): Unit = {
    val classifier = new StubClassifier
    val tooling = TurnTooling[{}](
      Toolbox.Empty,
      Toolbox.Empty,
      Vector.empty,
      engine.jot,
      right(TurnLoop.Rounds.of(2))
    )
    // One Mentions weighs a said root before its offer and asks its task questions in route.
    val mentions =
      new grit.lifecycle.triage.Mentions(
        grit.core.stitch.StitchReads(
          engine.entries,
          engine.conversations,
          engine.lifecycle,
          engine.stitches,
          engine.search,
          engine.principals
        ),
        engine.rooms,
        Corpora.Empty,
        grit.lifecycle.triage.TriageQuestions.shipped(grit.core.persona.Persona.Grit),
        grit.core.classify.ClearedClassifier.none("no classifier"),
        engine.placements,
        engine.db,
        Clock.system(),
        CharEstimate,
        Tuning.Default
      )
    engine.launch(
      Turn.body(
        TurnEnv(
          TurnRecords(
            engine.entries,
            engine.ledger,
            CharEstimate,
            engine.profiles,
            engine.principals,
            engine.documents
          ),
          TurnHosting(
            engine.conversations,
            engine.prompts,
            engine.toolSets,
            engine.requests,
            engine.edgeDirectory,
            engine.voices,
            engine.askers
          ),
          grit.kit.run.Launch.moveRecords(engine),
          new RetrievalAssembler(
            engine.entries,
            engine.conversations,
            engine.periods,
            engine.principals,
            engine.lifecycle,
            engine.search,
            engine.documents,
            engine.stitches,
            CharEstimate,
            Assembled.Shipped.window,
            Assembled.Shipped.tail,
            Tuning.Default
          ),
          judge,
          Writing,
          engine.db,
          Clock.system(),
          Fresh.random(),
          TurnSpeech(Speaking.Off, engine.speech, engine.deliveries),
          TurnStitching(
            engine.stitches,
            engine.search,
            engine.lifecycle,
            Tuning.Default,
            grit.act.phase.Placing
              .Env(
                engine.entries,
                engine.conversations,
                engine.db,
                engine.placements,
                grit.core.clock.Clock.system()
              )
          ),
          grit.turn.TurnWeighing(engine.triage, mentions),
          grit.turn.TurnRouting(mentions, engine.budget, engine.spending)
        ),
        tooling
      ),
      nothing,
      nothing,
      nothing,
      Triage.body(
        TriageEnv(
          TriageRecords(
            engine.entries,
            engine.triage,
            engine.principals,
            engine.conversations,
            engine.speech,
            engine.spending,
            engine.acknowledgements,
            engine.deliveries,
            engine.stitches,
            engine.search,
            engine.lifecycle,
            engine.rooms,
            engine.periods,
            engine.documents
          ),
          grit.kit.run.Launch.moves(
            engine,
            engine.db,
            new grit.models.StubModels,
            grit.core.classify.ClearedClassifier.of(grit.models.StubClassifier.Service, classifier),
            Clock.system()
          ),
          engine.budget,
          engine.db,
          Clock.system(),
          TriageSpeech(
            Speaking.Off,
            engine.budget,
            t => engine.inbox.startTurn(t).left.map(_.toString)
          ),
          Tuning.Default,
          grit.act.phase.Placing
            .Env(
              engine.entries,
              engine.conversations,
              engine.db,
              engine.placements,
              grit.core.clock.Clock.system()
            ),
          Corpora.Empty,
          grit.lifecycle.triage.TriageQuestions.shipped(grit.core.persona.Persona.Grit)
        )
      ),
      Stitch.body(
        StitchEnv(
          StitchReads(
            engine.entries,
            engine.conversations,
            engine.lifecycle,
            engine.stitches,
            engine.search,
            engine.principals
          ),
          grit.kit.run.Launch.moves(
            engine,
            engine.db,
            new grit.models.StubModels,
            grit.core.classify.ClearedClassifier.of(grit.models.StubClassifier.Service, classifier),
            Clock.system()
          ),
          engine.budget,
          engine.db,
          Clock.system(),
          Tuning.Default
        )
      ),
      Vector.empty
    )
  }

  /** `text` said to grit in task `name`, and its turn run to its end. */
  private def ask(engine: Engine^, name: String, text: String): TurnRef = {
    val turn = right(
      engine.inbox
        .ingest(Origin.Task("rebuild", name), SourceId(name), Message.User(text), Account.Local)
    )
    right(engine.inbox.startTurn(turn))
    val _ = engine.awaitTurn(turn)
    turn
  }

  private def hear(engine: Engine^, ts: String, text: String): Unit =
    engine.inbox.hear(
      Origin.Slack("T1", "C1", ts),
      SourceId(ts),
      text,
      Account.Local,
      now(),
      Reach(Some(s"C1/$ts/$ts"), Set.empty)
    ) ==> Right(())

  /** The confidential task conversation [[world]] holds, with nothing said in it. */
  private val Secret: Origin = Origin.Task("rebuild", "secret")

  /** Three task conversations, each asked one thing about the Falcon budget in turn, the first
    * then closed, then two messages heard in Slack: one asking, one not; a confidential one
    * ([[Secret]]); and the three turns, in order.
    */
  private lazy val world: (Reader, Vector[TurnRef]) = {
    val config = TestPostgres.freshDatabase("harness_rebuild")
    val engine = LiveEngine.open(config, Turn.Epoch)
    try {
      launch(engine)
      val a = ask(engine, "a", "the Falcon budget is 4200")
      val b = ask(engine, "b", "what is the Falcon budget?")
      // Said after b's turn assembled its window.
      val c = ask(engine, "c", "the Falcon budget moved to 9000")
      // a's period closes after both turns: open when b assembled, a record since.
      right(engine.jot.write(Subject.Public)(engine.periods.of(a))).foreach { p =>
        right(
          engine.jot.write(Subject.Public)(
            engine.periods.seal(
              CloseRef(p.ref, a.turnSeq, now()),
              CloseReason.Lapsed,
              TestClosings.prose("the Falcon budget is 4200, agreed."),
              now()
            )
          )
        )
      }
      // A confidential task conversation: a turn there may write its query only to a pin
      // cleared for confidential, which a reader clearing no party has none of.
      val _ = right(
        engine.jot.write(Subject.Public)(
          engine.conversations
            .findOrCreate(Secret, Account.Local, Label.at(Level.Confidential))
        )
      )
      // Asking, and not put to grit (~0.1): never answered, under Off.
      hear(engine, "3000.1", "Falcon budget, anyone? ~0.1 ~back:asks")
      hear(engine, "4000.1", "thanks all ~back:nothing")
      val tagged = eventually(
        right(
          engine.db.read(Subject.Public)(engine.triage.tagged(Instant.EPOCH, now().plusSeconds(60)))
        ).size == 2
      )
      assert(tagged)
      (Reader.open(config), Vector(a, b, c))
    } finally engine.close()
  }

  /** A document kept at task "rebuild", the Falcon budget, before a turn there asks about it;
    * that turn's window rebuilt while the document is kept, then once it is forgotten (as its
    * retention would collect it); and the document's version.
    */
  private lazy val documented: (Reader, RebuiltWindow, RebuiltWindow, DocumentVersionId) = {
    val config = TestPostgres.freshDatabase("harness_rebuild_documents")
    val engine = LiveEngine.open(config, Turn.Epoch)
    try {
      launch(engine)
      val notes = right(PluginName.of("notes"))
      val terms =
        right(DocumentTerms.of(right(DocLabel.of("Notes")), DocWeight.Unscaled, 30.days, 10))
      right(engine.jot.write(Subject.Public)(engine.documents.declare(Vector(notes -> terms))))
      val version = right(
        engine.jot.write(Subject.Public)(
          engine
            .keeper(notes, terms)
            .write(
              right(DocKey.of("falcon")),
              grit.core.visibility.Label.Public,
              Integrity.Member,
              Origin.Task("rebuild", "d").room,
              right(DocText.of("the Falcon budget is 4200")),
              ujson.Obj(),
              None,
              now()
            )
        )
      ) match {
        case Written.Versioned(v, _, _) => v
        case other => throw new java.lang.AssertionError(s"not written: $other")
      }
      val d = ask(engine, "d", "what is the Falcon budget?")
      val reader = Reader.open(config)
      val kept = right(
        Rebuild.recorded(reader, d.workflowId, Assembled.Shipped, Admission.Shipped, Width.Deployed)
      )
      right(engine.jot.write(Subject.Public)(engine.documents.forget(version)))
      val gone = right(
        Rebuild.recorded(reader, d.workflowId, Assembled.Shipped, Admission.Shipped, Width.Deployed)
      )
      (reader, kept, gone, version)
    } finally engine.close()
  }

  /** A turn at task "late" asking the Falcon budget, after another task's turn so it writes a
    * query, then a document kept there that tells it; a moment between the two; the document's
    * version; and the asking turn.
    */
  private lazy val late: (Reader, Instant, DocumentVersionId, TurnRef) = {
    val config = TestPostgres.freshDatabase("harness_refound_late")
    val engine = LiveEngine.open(config, Turn.Epoch)
    try {
      launch(engine)
      val notes = right(PluginName.of("notes"))
      val terms =
        right(DocumentTerms.of(right(DocLabel.of("Notes")), DocWeight.Unscaled, 30.days, 10))
      right(engine.jot.write(Subject.Public)(engine.documents.declare(Vector(notes -> terms))))
      val _ = ask(engine, "early", "the Falcon plan is drafted")
      val asking = ask(engine, "late", "what is the Falcon budget?")
      val between = now()
      val version = right(
        engine.jot.write(Subject.Public)(
          engine
            .keeper(notes, terms)
            .write(
              right(DocKey.of("falcon")),
              grit.core.visibility.Label.Public,
              Integrity.Member,
              Origin.Task("rebuild", "late").room,
              right(DocText.of("the Falcon budget is 4200")),
              ujson.Obj(),
              None,
              now()
            )
        )
      ) match {
        case Written.Versioned(v, _, _) => v
        case other => throw new java.lang.AssertionError(s"not written: $other")
      }
      (Reader.open(config), between, version, asking)
    } finally engine.close()
  }

  /** Two task conversations told the Falcon budget, the second as a rumour, then a third asking
    * it, its turn judged by a [[Sceptic]]; the two told, that turn's window rebuilt, what its
    * search recorded, and how many searches the rebuild made.
    */
  private lazy val judged: (Reader, TurnRef, TurnRef, RebuiltWindow, TurnRecord.Retrieval, Int) = {
    val config = TestPostgres.freshDatabase("harness_rebuild_judged")
    val engine = LiveEngine.open(config, Turn.Epoch)
    try {
      launch(
        engine,
        grit.core.classify.ClearedClassifier.of(StubClassifier.Service, new Sceptic)
      )
      val told = ask(engine, "e", "the Falcon budget is 4200")
      val rumoured = ask(engine, "f", "a rumour says the Falcon budget is 1")
      val g = ask(engine, "g", "what is the Falcon budget?")
      val reader = Reader.open(config)
      val counted = new Searches(reader)
      val rebuilt =
        right(
          Rebuild.recorded(
            counted,
            g.workflowId,
            Assembled.Shipped,
            Admission.Shipped,
            Width.Deployed
          )
        )
      val retrieval = right(right(reader.steps(g.workflowId).map(TurnRecord.retrieval)))
      (
        reader,
        told,
        rumoured,
        rebuilt,
        right(retrieval.toRight("no search recorded")),
        counted.made
      )
    } finally engine.close()
  }

  /** `inner`, counting each search made of it: `made`. */
  private final class CountedSearch(inner: grit.core.store.EntrySearch)
      extends grit.core.store.EntrySearch {
    // Untracked: a CountedSearch is made by one [[Searches]], used by the one rebuild `judged`
    // runs on its own thread, and its count read once that rebuild returns. No other code
    // holds it.
    @caps.unsafe.untrackedCaptures
    var made: Int = 0
    def search(
        conversation: ConversationId,
        from: TurnSeq,
        before: TurnSeq,
        query: String,
        limit: Int
    )(using grit.core.store.Tx^) = {
      made += 1
      inner.search(conversation, from, before, query, limit)
    }
    def nearby(open: Vector[grit.core.store.OpenPeriod], query: String, limit: Int)(using
        grit.core.store.Tx^
    ) = {
      made += 1
      inner.nearby(open, query, limit)
    }
    def closings(conversations: Vector[ConversationId], query: String, limit: Int)(using
        grit.core.store.Tx^
    ) = {
      made += 1
      inner.closings(conversations, query, limit)
    }
    def room(room: grit.core.place.Place, from: Instant, until: Instant, query: String, limit: Int)(
        using grit.core.store.Tx^
    ) = {
      made += 1
      inner.room(room, from, until, query, limit)
    }
  }

  /** `reader`, counting each search made of its entries: `made`. */
  private final class Searches(reader: Reader^) extends Reader {
    private val counted = new CountedSearch(reader.search)
    def made: Int = counted.made
    val db: grit.core.store.Db = reader.db
    val all: grit.core.store.Reads = reader.all
    val entries: grit.core.store.EntryStore = reader.entries
    val conversations: grit.core.store.ConversationStore = reader.conversations
    val periods: grit.core.store.PeriodStore = reader.periods
    val lifecycle: grit.core.store.LifecycleStore = reader.lifecycle
    val stitches: grit.core.stitch.StitchStore = reader.stitches
    val rooms: grit.core.recipe.RoomReads = reader.rooms
    val search: grit.core.store.EntrySearch = counted
    val documents: grit.core.document.DocumentSearch = reader.documents
    def current()(using grit.core.store.Tx^) = reader.current()
    val principals: grit.core.store.Principals = reader.principals
    val triage: grit.core.triage.TriageStore = reader.triage
    val shadows: grit.core.triage.TriageShadows = reader.shadows
    val reviews: grit.core.review.ReviewStore = reader.reviews
    val speech: grit.core.speech.SpeechStore = reader.speech
    def cues(turn: grit.core.id.TurnRef)(using grit.core.store.Tx^) = reader.cues(turn)
    val ledger: grit.core.store.UsageLedger = reader.ledger
    val prompts: grit.core.store.PromptStore = reader.prompts
    val toolSets: grit.core.tool.ToolSets = reader.toolSets
    val profiles: grit.core.store.ModelProfileStore = reader.profiles
    def turns(until: Instant) = reader.turns(until)
    def steps(id: WorkflowId) = reader.steps(id)
    def workflow(id: WorkflowId) = reader.workflow(id)
    def starts() = reader.starts()
    def close(): Unit = ()
  }

  private def conversations(n: Vector[Nearby]): Vector[ConversationId] = n.map(_.conversation)

  override def utestAfterAll(): Unit = {
    world._1.close()
    documented._1.close()
    judged._1.close()
    late._1.close()
  }

  val tests = Tests {
    test(
      "a turn whose window held a document is rebuilt holding it; once it is forgotten, gone"
    ) {
      val (_, kept, gone, version) = documented
      (kept.recorded.map(_.documents), kept.rebuilt.documents, kept.drift) ==>
        (Some(Vector(version)), Vector(version), Some(Drift.Same))
      (gone.rebuilt.documents, gone.drift) ==> (Vector(), Some(Drift.Gone))
    }

    test("a turn rebuilt as of its assembly is the window it recorded, its query replayed") {
      val (reader, Vector(a, b, _)) = world: @unchecked
      val r = right(
        Rebuild.recorded(reader, b.workflowId, Assembled.Shipped, Admission.Shipped, Width.Deployed)
      )
      // What b's turn recorded: a's turn, found by its query, from elsewhere, open then.
      r.recorded.map(w => conversations(w.nearby)) ==> Some(Vector(a.conversationId))
      r.drift ==> Some(Drift.Same)
      r.rebuilt.notes.collect { case q: AssemblyNote.Queried => q.query } ==> Vector(
        "Falcon budget"
      )
    }

    test("a turn rebuilt from its recorded verdicts shows none it rejected") {
      val (_, told, rumoured, r, retrieval, _) = judged
      // The rumour was found, and its judge rejected it; the budget it passed.
      retrieval.verdicts.collect {
        case (Retrieved.Near(c, _), Verdict.Passed(_)) => c -> "passed"
        case (Retrieved.Near(c, _), Verdict.Rejected(_)) => c -> "rejected"
      }.toMap ==> Map(told.conversationId -> "passed", rumoured.conversationId -> "rejected")
      r.recorded.map(w => conversations(w.nearby)) ==> Some(Vector(told.conversationId))
      (conversations(r.rebuilt.nearby), r.drift) ==>
        (Vector(told.conversationId), Some(Drift.Same))
    }

    test("a turn rebuilt from its recorded search searches nothing, its query the one recorded") {
      val (_, _, _, r, retrieval, searches) = judged
      (searches, r.rebuilt.notes.collect { case q: AssemblyNote.Queried => q.query }) ==>
        (0, Vector(retrieval.written.text))
    }
    test(
      "documents read at the end find one kept after the moment a read asks for; as of that moment, none"
    ) {
      val (reader, between, version, asking) = late
      val end = now()
      def found(view: AsOf) = right(reader.all.read {
        for {
          plugins <- view.documents.declared()
          shelves <- view.documents.shelved(plugins.map(_._1), between)
          hits <- view.documents.search(shelves, "Falcon budget", 10, between)
        } yield hits.map(_.document.version)
      })
      (found(AsOf(reader, end).documentsAtEnd), found(AsOf(reader, end))) ==>
        (Vector(version), Vector.empty)
    }
    test("documents read as of a later moment find one kept after the stores' own moment") {
      val (reader, between, version, asking) = late
      val end = now()
      def found(view: AsOf) = right(reader.all.read {
        for {
          plugins <- view.documents.declared()
          shelves <- view.documents.shelved(plugins.map(_._1), between)
          hits <- view.documents.search(shelves, "Falcon budget", 10, between)
          read <- view.documents.read(Vector(version))
        } yield (hits.map(_.document.version), read.map(_.version))
      })
      (
        found(AsOf(reader, between).documentsAsOf(end)),
        found(AsOf(reader, between).documentsAtEnd)
      ) ==>
        ((Vector(version), Vector(version)), (Vector.empty, Vector.empty))
    }
    test("a turn drawn again with documents as of now holds one kept after it ran") {
      val (reader, between, version, asking) = late
      def drawn(documents: Instant) = right(
        Rebuild.redrawn(reader, asking.workflowId, Assembled.Shipped, Width.Deployed, documents)
      ).documents
      (drawn(now()), drawn(between)) ==> (Vector(version), Vector.empty)
    }
    test("a turn that followed no document is left out of the recall replay") {
      val (reader, _, _, _, _, _) = judged
      val turns = right(reader.turns(now()))
      val current = Refound.Current.of(right(reader.all.read(reader.current())))
      val searched =
        turns.count((w, _) => right(reader.steps(w).map(TurnRecord.retrieval)).exists(_.isDefined))
      // The asking turn searched; whether a turn telling the budget did depends on what was
      // said before it, so only "some turn searched" is pinned.
      (
        turns.size,
        searched > 0,
        turns.map((w, _) => Refound.of(reader, w, Assembled.Shipped, now(), current))
      ) ==> (3, true, Vector.fill(3)(Right(None)))
    }

    test("a message said elsewhere after a turn's assembly is absent from its rebuild") {
      val (reader, Vector(a, b, c)) = world: @unchecked
      val r = right(
        Rebuild.recorded(reader, b.workflowId, Assembled.Shipped, Admission.Shipped, Width.Deployed)
      )
      conversations(r.rebuilt.nearby).filter(Set(a, c).map(_.conversationId)) ==>
        Vector(a.conversationId)
    }

    test(
      "a message said to grit is put triage's set as heard, a question per source at its place"
    ) {
      val (reader, Vector(_, b, _)) = world: @unchecked
      val turns = right(TurnCapture(reader, Dump(Digest.text(""), now().plusSeconds(60))))
      val knowledge = right(
        KnowledgeJson.read(
          """{"sources": [
            |  {"name": "tasks", "line": "the tasks' records", "within": "task:"},
            |  {"name": "chat", "line": "the chat", "within": "slack:"}
            |]}""".stripMargin
        )
      )
      // b's message, said in a task: asked as heard, with a question for the task source only.
      turns.cases
        .find(_.workflow == b.workflowId)
        .toRight("b not captured")
        .flatMap(
          TurnTriage.ask(
            reader,
            _,
            knowledge,
            grit.lifecycle.triage.TriageQuestions.shipped(grit.core.persona.Persona.Grit),
            Tuning.Default
          )
        )
        .map(a =>
          (
            a.questions.keys.map(QuestionName.value).toVector,
            a.posed.asking match {
              case Asking.Questions(_, state, _) => state.message
              case _ => "not a question set's"
            }
          )
        ) ==> Right(
        (
          Vector(
            "gap",
            "open",
            "to",
            "to-grit",
            "durable",
            "anchor-record",
            "source:tasks"
          ),
          "what is the Falcon budget?"
        )
      )
    }

    test(
      "a window-only case in a conversation no query pin is cleared for has its query withheld; its window is drawn without one"
    ) {
      val (reader, _) = world
      val secret = right(reader.all.read(reader.conversations.find(Secret)))
        .getOrElse(throw new java.lang.AssertionError("no confidential conversation"))
      val w = WindowOnly(TurnRef(secret.id, TurnSeq.First), now())
      (
        right(WindowOnly.asked(reader, w, Assembled.Shipped, Width.Deployed)),
        WindowOnly
          .window(reader, w, Assembled.Shipped, Map.empty, Width.Deployed)
          .map(w => (w.entries, w.notes))
      ) ==> (
        WindowOnly.Query.Withheld,
        Right((Vector.empty, Vector(AssemblyNote.FellBack(RetrievalAssembler.NoPin))))
      )
    }

    test(
      "a heard message triage read as asking, that no turn answered, gets a window; its query asked once"
    ) {
      val (reader, Vector(a, b, c)) = world: @unchecked
      val only = right(WindowOnly.all(reader, now().plusSeconds(60)))
      // The asking message's thread alone: the other was not read as asking.
      only.map(w =>
        right(reader.all.read(reader.conversations.get(w.turn.conversationId))).map(_.origin)
      ) ==> Vector(Some(Origin.Slack("T1", "C1", "3000.1")))
      val requests =
        only.flatMap(w =>
          right(WindowOnly.asked(reader, w, Assembled.Shipped, Width.Deployed)) match {
            case WindowOnly.Query.Asked(request) => request
            case WindowOnly.Query.Withheld => None
          }
        )
      requests.size ==> 1
      val config =
        OpenRouterConfig(
          "unused",
          "test/query",
          64,
          OpenRouterConfig.Endpoint,
          Duration.ofSeconds(1)
        )
      val cache = Cache.at[String](Files.createTempDirectory("rebuild-cache"))
      val writer = new Writer
      val budget = right(Budget.of(BigDecimal("0.01"), Queries.estimate(requests, config, cache)))
      val first = Queries.answer(requests, config, cache, writer, budget, Clock.system())
      val again = Queries.answer(requests, config, cache, writer, budget, Clock.system())
      ((first.asked, first.cached), (again.asked, again.cached), writer.asked) ==> (
        (1, 0),
        (0, 1),
        1
      )
      Queries.estimate(requests, config, cache) ==> BigDecimal(0)
      // Heard after a's period closed: a is its record, b and c their open turns.
      def named(t: TurnRef) = ConversationId.value(t.conversationId)
      only.map(o =>
        right(
          WindowOnly.window(reader, o, Assembled.Shipped, again.answers, Width.Deployed)
        ).nearby.collect {
          case Nearby.Open(k, _, _) => s"open ${ConversationId.value(k)}"
          case Nearby.Closed(k, _, _) => s"closed ${ConversationId.value(k)}"
        }.toSet
      ) ==> Vector(Set(s"closed ${named(a)}", s"open ${named(b)}", s"open ${named(c)}"))
    }
  }
}
