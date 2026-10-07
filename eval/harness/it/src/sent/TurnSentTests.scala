package grit.eval.harness.sent

import java.util.concurrent.ConcurrentLinkedQueue

import scala.jdk.CollectionConverters.*

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.core.classify.Classifier
import grit.core.clock.{Clock, Fresh}
import grit.core.durable.Durable
import grit.core.id.{PrincipalId, SourceId, TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Catalog, Pinned}
import grit.core.place.Directory
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError, ToolUse}
import grit.core.speech.{Limits, Speaking}
import grit.core.spend.DailyCap
import grit.core.stitch.Tuning
import grit.core.store.Origin
import grit.core.tool.{Args, Field, Gate, Outcome, Tool, ToolName, ToolSpec, Toolbox}
import grit.core.triage.Weighing
import grit.dbos.engine.{Engine, LiveEngine, Reader}
import grit.dbos.sql.TestPostgres
import grit.models.{StubModels, StubProvider}
import grit.turn.{
  Turn,
  TurnEnv,
  TurnHosting,
  TurnLoop,
  TurnRecords,
  TurnSpeech,
  TurnStitching,
  TurnTooling,
  TurnWeighing
}

import utest.*

/** A turn a live engine ran, read back through [[TurnSent.read]]: its requests, rebuilt
  * after the fact from the database, against those its model was sent.
  */
object TurnSentTests extends TestSuite {

  private val nothing = (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id)

  private def right[E, A](e: Either[E, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why.toString), identity)

  /** `echo`: a free tool run in the turn's own step. */
  private val echo: Tool[String] =
    Tool(
      ToolSpec(
        ToolName("echo"),
        "Echoes.",
        Args.of((text = Field.text("What to echo."))).map(_.text)
      ),
      Gate.Free,
      t => t,
      t => Outcome.Done(s"echoed $t")
    )

  /** Calls `echo` until two of its results are in, then replies; keeps every request. */
  private final class Looping extends Provider {
    val sent = new ConcurrentLinkedQueue[ModelRequest]()

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      sent.add(request)
      val results = request.messages.count(_.isInstanceOf[Message.ToolResult])
      val blocks =
        if (results < 2 && request.use == ToolUse.Auto)
          Vector(
            AssistantBlock.Text("calling"),
            AssistantBlock.ToolCall(
              grit.core.id.ToolCallId(s"call-$results"),
              "echo",
              ujson.Obj("text" -> s"round $results")
            )
          )
        else Vector(AssistantBlock.Text("two echoes"))
      Right(
        Message.Assistant(
          blocks,
          if (blocks.exists(_.isInstanceOf[AssistantBlock.ToolCall])) StopReason.ToolUse
          else StopReason.EndTurn,
          Usage(Tokens(100), Tokens(10), Tokens.Zero, Some(BigDecimal("0.001"))),
          "test/looping"
        )
      )
    }
  }

  /** The stub's catalog: the turn's calls to `turn`, every other role's to the stub. */
  private final class LoopingModels(turn: Provider^) extends Models {
    private val rest = new StubProvider()
    def catalog(): Either[String, Catalog] = Right(StubModels.Catalog)
    def provider(pinned: Pinned): Provider^ =
      if (pinned.assignment == StubModels.Catalog.policy.turn) turn else rest
  }

  /** Weighs nothing: a TUI turn's recipe reads no answers. */
  private object Unweighed extends Weighing {
    def weigh(turn: TurnRef): Either[Weighing.Unweighed, Weighing.Weighed] =
      Left(Weighing.Unweighed.Unread)
  }

  private def launch(engine: Engine^, model: Provider^): Unit = {
    val speaking = Speaking.Within(
      Limits.suggested(
        right(DailyCap.of("0.25")),
        grit.core.triage.Tags.V2.asks(
          grit.core.period.Probability.clamped(0.5)
        )
      )
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
            engine.voices
          ),
          new LinearAssembler(
            engine.entries,
            engine.periods,
            engine.principals,
            CharEstimate,
            LinearAssembler.DefaultBudget
          ),
          Classifier.none("no classifier"),
          new LoopingModels(model),
          engine.db,
          Clock.system(),
          Fresh.random(),
          TurnSpeech(speaking, engine.speech, engine.deliveries),
          TurnStitching(
            engine.stitches,
            engine.search,
            engine.lifecycle,
            Tuning.Default,
            engine.placements
          ),
          TurnWeighing(engine.triage, Unweighed)
        ),
        TurnTooling[{}](
          right(Toolbox.of[{}](echo)),
          Toolbox.Empty,
          Vector.empty,
          engine.jot,
          right(TurnLoop.Budget.of(4))
        )
      ),
      nothing,
      nothing,
      nothing,
      nothing,
      nothing,
      Vector.empty
    )
  }

  val tests = Tests {
    test(
      "a live turn read back is rebuilt as its model was sent it, each call agreeing with its ledger row"
    ) {
      val config = TestPostgres.freshDatabase("harness_sent")
      val engine = LiveEngine.open(config, Turn.Epoch)
      val model = new Looping
      try {
        launch(engine, model)
        val origin = Origin.Tui(right(Directory.of("/tmp/harness-sent")), "s")
        val turn = right(
          engine.inbox.ingest(origin, SourceId("t0"), Message.User("echo twice"), PrincipalId.Local)
        )
        right(engine.inbox.startTurn(turn))
        val _ = engine.awaitTurn(turn)
        val reader = Reader.open(config)
        val sent =
          try right(TurnSent.read(reader, turn.workflowId))
          finally reader.close()
        val received = model.sent.asScala.toVector
        // The summary goes to the stub, not this model: these are the reply's three calls.
        received.size ==> 3
        sent.requests.map(_._3) ==> received
        sent.requests.map((_, reply, request) => sent.agrees(reply, request)) ==>
          Vector.fill(3)(Some(true))
        sent.picked ==> Some(Picked.On)
        sent.epoch ==> Turn.Epoch
      } finally engine.close()
    }
  }
}
