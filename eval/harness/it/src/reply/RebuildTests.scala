package grit.eval.harness.reply

import java.nio.file.Files
import java.time.{Duration, Instant}

import scala.concurrent.duration.*

import grit.assembly.estimate.CharEstimate
import grit.assembly.retrieval.RetrievalAssembler
import grit.core.classify.Classifier
import grit.core.clock.{Clock, Fresh}
import grit.core.context.{AssemblyNote, Width}
import grit.core.durable.Durable
import grit.core.id.{
  CloseRef,
  ConversationId,
  PrincipalId,
  QuestionName,
  SourceId,
  TurnRef,
  WorkflowId
}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.{CloseReason, TestClosings}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.speech.{Reach, Speaking}
import grit.core.stitch.{StitchReads, Tuning}
import grit.core.store.{Nearby, Origin}
import grit.core.tool.Toolbox
import grit.core.triage.KnowledgeSources
import grit.dbos.engine.{Engine, LiveEngine, Reader}
import grit.dbos.sql.TestPostgres
import grit.eval.harness.corpus.{Digest, Dump, KnowledgeJson, TurnCapture}
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
  TurnRecords,
  TurnSpeech,
  TurnStitching,
  TurnTooling
}

import Queries.given
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

  /** A query writer answering "Falcon budget", counting what it is asked. */
  private final class Writer extends Provider {
    // Untracked: a Writer is made by the one test that reads it, handed to the two answers it
    // runs one after the other on its own thread, and read once both return; no other code
    // holds it, so no write to it is seen elsewhere.
    @caps.unsafe.untrackedCaptures
    var asked: Int = 0
    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      asked += 1
      Right(answer("Falcon budget"))
    }
  }

  private def launch(engine: Engine^): Unit = {
    val classifier = new StubClassifier
    val tooling = TurnTooling[{}](
      Toolbox.Empty,
      Toolbox.Empty,
      Vector.empty,
      engine.jot,
      right(TurnLoop.Budget.of(2))
    )
    engine.launch(
      Turn.body(
        TurnEnv(
          TurnRecords(
            engine.entries,
            engine.ledger,
            CharEstimate,
            engine.profiles,
            engine.principals
          ),
          TurnHosting(
            engine.conversations,
            engine.prompts,
            engine.toolSets,
            engine.requests,
            engine.edgeDirectory,
            engine.voices
          ),
          new RetrievalAssembler(
            engine.entries,
            engine.conversations,
            engine.periods,
            engine.principals,
            engine.lifecycle,
            engine.search,
            engine.stitches,
            new Writer,
            CharEstimate,
            Assembled.Shipped.window,
            Assembled.Shipped.tail,
            Tuning.Default
          ),
          Classifier.none("no classifier"),
          new StubModels(),
          engine.db,
          Clock.system(),
          Fresh.random(),
          TurnSpeech(Speaking.Off, engine.speech, engine.deliveries),
          TurnStitching(
            engine.stitches,
            engine.search,
            engine.lifecycle,
            Tuning.Default,
            engine.placements
          ),
          grit.turn.TurnWeighing(
            engine.triage,
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
              KnowledgeSources.Empty,
              grit.lifecycle.triage.TriageQuestions.shipped(grit.core.persona.Persona.Grit),
              Classifier.none("no classifier"),
              engine.placements,
              engine.db,
              Clock.system(),
              CharEstimate,
              Tuning.Default
            )
          )
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
            engine.stitches,
            engine.search,
            engine.lifecycle,
            engine.rooms
          ),
          classifier,
          engine.db,
          Clock.system(),
          TriageSpeech(
            Speaking.Off,
            engine.budget,
            t => engine.inbox.startTurn(t).left.map(_.toString)
          ),
          Tuning.Default,
          engine.placements,
          KnowledgeSources.Empty,
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
          classifier,
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
        .ingest(Origin.Task("rebuild", name), SourceId(name), Message.User(text), PrincipalId.Local)
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
      PrincipalId.Local,
      now(),
      Reach(Some(s"C1/$ts/$ts"), Set.empty)
    ) ==> Right(())

  /** Three task conversations, each asked one thing about the Falcon budget in turn, the first
    * then closed, then two messages heard in Slack: one asking, one not; and the turns, in
    * order.
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
      right(engine.jot.write(engine.periods.of(a))).foreach { p =>
        right(
          engine.jot.write(
            engine.periods.seal(
              CloseRef(p.ref, a.turnSeq, now()),
              CloseReason.Lapsed,
              TestClosings.prose("the Falcon budget is 4200, agreed."),
              now()
            )
          )
        )
      }
      hear(engine, "3000.1", "Falcon budget, anyone? ~back:asks")
      hear(engine, "4000.1", "thanks all ~back:nothing")
      val tagged = eventually(
        right(
          engine.db.read(engine.triage.tagged(Instant.EPOCH, now().plusSeconds(60)))
        ).size == 2
      )
      assert(tagged)
      (Reader.open(config), Vector(a, b, c))
    } finally engine.close()
  }

  private def conversations(n: Vector[Nearby]): Vector[ConversationId] = n.map(_.conversation)

  override def utestAfterAll(): Unit = world._1.close()

  val tests = Tests {
    test("a turn rebuilt as of its assembly is the window it recorded, its query replayed") {
      val (reader, Vector(a, b, _)) = world: @unchecked
      val r = right(Rebuild.recorded(reader, b.workflowId, Assembled.Shipped, Width.Deployed))
      // What b's turn recorded: a's turn, found by its query, from elsewhere, open then.
      r.recorded.map(w => conversations(w.nearby)) ==> Some(Vector(a.conversationId))
      r.drift ==> Some(Drift.Same)
      r.rebuilt.notes.collect { case q: AssemblyNote.Queried => q.query } ==> Vector(
        "Falcon budget"
      )
    }

    test("a message said elsewhere after a turn's assembly is absent from its rebuild") {
      val (reader, Vector(a, b, c)) = world: @unchecked
      val r = right(Rebuild.recorded(reader, b.workflowId, Assembled.Shipped, Width.Deployed))
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
            "anchor",
            "anchor-record",
            "source:tasks"
          ),
          "what is the Falcon budget?"
        )
      )
    }

    test(
      "a heard message triage read as asking, that no turn answered, gets a window; its query asked once"
    ) {
      val (reader, Vector(a, b, c)) = world: @unchecked
      val only = right(WindowOnly.all(reader, now().plusSeconds(60)))
      // The asking message's thread alone: the other was not read as asking.
      only.map(w =>
        right(reader.db.read(reader.conversations.get(w.turn.conversationId))).map(_.origin)
      ) ==> Vector(Some(Origin.Slack("T1", "C1", "3000.1")))
      val requests =
        only.flatMap(w => right(WindowOnly.asked(reader, w, Assembled.Shipped, Width.Deployed)))
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
