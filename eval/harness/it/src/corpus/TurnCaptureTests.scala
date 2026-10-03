package grit.eval.harness.corpus

import java.time.Instant

import scala.concurrent.duration.*

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.core.classify.Classifier
import grit.core.clock.{Clock, Fresh}
import grit.core.durable.Durable
import grit.core.id.{CloseRef, PrincipalId, SourceId, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Catalog, Pinned}
import grit.core.period.{CloseReason, Probability, TestClosings}
import grit.core.place.Directory
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError, ToolUse}
import grit.core.speech.{Limits, Reach, Speaking}
import grit.core.spend.DailyCap
import grit.core.stitch.{StitchReads, Tuning}
import grit.core.store.{Focus, Origin}
import grit.core.tool.{Args, Field, Gate, Outcome, Tool, ToolName, ToolSpec, Toolbox}
import grit.core.triage.KnowledgeSources
import grit.dbos.engine.{Engine, LiveEngine, Reader}
import grit.dbos.sql.TestPostgres
import grit.eval.harness.label.Verdicts
import grit.lifecycle.stitch.{Stitch, StitchEnv}
import grit.lifecycle.triage.{Triage, TriageEnv, TriageRecords, TriageSpeech}
import grit.models.{StubClassifier, StubModels, StubProvider}
import grit.turn.{
  Turn,
  TurnEnv,
  TurnHosting,
  TurnLoop,
  TurnOffer,
  TurnRecord,
  TurnRecords,
  TurnSpeech,
  TurnStitching,
  TurnTooling
}

import utest.*

/** Turns captured from a database a live engine ran them on: the real turn, a scripted model,
  * the stub classifier. Every message, draft, tool result, closing and model error holds
  * [[Marker]], which no captured file may.
  */
object TurnCaptureTests extends TestSuite {

  private val Marker = "zqxmarker"

  private val nothing = (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id)

  /** The system clock's now. */
  private def now(): Instant = Clock.system().now()

  private def eventually(done: => Boolean): Boolean = {
    val clock = Clock.system()
    val until = clock.millis() + 30.seconds.toMillis
    var held = done
    while (!held && clock.millis() < until) { clock.sleep(50.millis); held = done }
    held
  }

  private def right[E, A](e: Either[E, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why.toString), identity)

  /** The result `echo` gives every call. */
  private val Echoed = s"$Marker echoed"

  /** `echo`: a free tool run in the turn's own step, answering [[Echoed]]. */
  private val echo: Tool[String] =
    new Tool(
      ToolSpec(
        ToolName("echo"),
        "Echoes.",
        Args.of((text = Field.text("What to echo."))).map(_.text)
      ),
      Gate.Free,
      t => t,
      _ => Outcome.Done(Echoed)
    )

  /** A model scripted by the turn's own message: `~fail` refuses; `~loop` calls `echo` until
    * two of its results are in, then replies; `~pass` passes; "what was it" answers from the
    * record; anything else replies.
    */
  private final class Scripted extends Provider {
    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      val said = request.messages.collect { case Message.User(t) => t }.lastOption.getOrElse("")
      val results = request.messages.count(_.isInstanceOf[Message.ToolResult])
      def reply(blocks: AssistantBlock*): Either[ProviderError, Message.Assistant] = Right(
        Message.Assistant(
          blocks.toVector,
          if (blocks.exists(_.isInstanceOf[AssistantBlock.ToolCall])) StopReason.ToolUse
          else StopReason.EndTurn,
          Usage(Tokens(100), Tokens(10), Tokens.Zero, Some(BigDecimal("0.001"))),
          "test/scripted"
        )
      )
      if (said.contains("~fail")) Left(ProviderError.Refused(s"$Marker refused"))
      else if (said.contains("~loop") && results < 2 && request.use == ToolUse.Auto)
        reply(
          AssistantBlock.Text(s"$Marker calling"),
          AssistantBlock.ToolCall(
            grit.core.id.ToolCallId(s"call-$results"),
            "echo",
            ujson.Obj("text" -> s"$Marker argument")
          )
        )
      else if (said.contains("~pass")) reply(AssistantBlock.Text("pass"))
      else if (said.contains("what was it")) reply(AssistantBlock.Text(s"$Marker Falcon 4200"))
      else reply(AssistantBlock.Text(s"$Marker replied"))
    }
  }

  /** The stub's catalog: the turn's calls to `turn`, every other role's to the stub. */
  private final class ScriptedModels(turn: Provider^) extends Models {
    private val rest = new StubProvider()
    def catalog(): Either[String, Catalog] = Right(StubModels.Catalog)
    def provider(pinned: Pinned): Provider^ =
      if (pinned.assignment == StubModels.Catalog.policy.turn) turn else rest
  }

  /** V2's `gap` choosing `asks`, which the stub classifier answers from `~back:asks`. */
  private val asks = grit.core.triage.Tags.V2.asks(Probability.clamped(0.5))

  private def launch(engine: Engine^): Unit = {
    val speaking = Speaking.Within(Limits.suggested(right(DailyCap.of("0.25")), asks))
    val classifier = new StubClassifier
    val tooling = TurnTooling[{}](
      right(Toolbox.of[{}](echo)),
      Toolbox.Empty,
      Vector.empty,
      engine.jot,
      right(TurnLoop.Budget.of(4))
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
            engine.voices,
            engine.principals
          ),
          new LinearAssembler(
            engine.entries,
            engine.periods,
            engine.principals,
            CharEstimate,
            LinearAssembler.DefaultBudget
          ),
          Classifier.none("no classifier"),
          new ScriptedModels(new Scripted),
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
            engine.stitches,
            engine.search,
            engine.lifecycle,
            engine.rooms
          ),
          classifier,
          engine.db,
          Clock.system(),
          TriageSpeech(
            speaking,
            engine.budget,
            t => engine.inbox.startTurn(t).left.map(_.toString)
          ),
          Tuning.Default,
          engine.placements,
          KnowledgeSources.Empty
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

  /** `text` said to grit at `origin` as `source`, and its turn run to its end. */
  private def ask(engine: Engine^, origin: Origin, source: String, text: String): TurnRef = {
    val turn = right(
      engine.inbox.ingest(origin, SourceId(source), Message.User(text), PrincipalId.Local)
    )
    right(engine.inbox.startTurn(turn))
    val _ = engine.awaitTurn(turn)
    turn
  }

  private val dump = Dump(Digest.text("synthetic dump"), Instant.parse("2100-01-01T00:00:00Z"))

  val tests = Tests {
    test(
      "every turn is captured, TUI and Slack, addressed and heard, as it ran, the same on recapture, and no text reaches its files"
    ) {
      val config = TestPostgres.freshDatabase("harness_turns")
      val engine = LiveEngine.open(config, Turn.Epoch)
      try {
        launch(engine)
        val tui = Origin.Tui(right(Directory.of("/tmp/harness-turns")), "s")
        // A TUI session: a remark, its period closed with a record, then a question it answers.
        val remark = ask(engine, tui, "t0", s"$Marker remember the Falcon budget is 4200")
        right(engine.jot.write(engine.periods.of(remark))).foreach { p =>
          right(
            engine.jot.write(
              engine.periods.seal(
                CloseRef(p.ref, remark.turnSeq, now()),
                CloseReason.Lapsed,
                TestClosings.prose(s"$Marker the Falcon budget is 4200, agreed."),
                now()
              )
            )
          )
        }
        val question = ask(engine, tui, "t1", s"$Marker what was it?")
        val failed = ask(engine, tui, "t2", s"$Marker ~fail")
        // A Slack thread asked directly, the model looping through two rounds of tools.
        val looped = ask(engine, Origin.Slack("T1", "C1", "2000.1"), "2000.1", s"$Marker ~loop")
        // A Slack message heard, drafted by triage's gate, and passed by the model.
        engine.inbox.hear(
          Origin.Slack("T1", "C1", "3000.1"),
          SourceId("3000.1"),
          s"$Marker ~pass is the deploy on friday? ~back:asks",
          PrincipalId.Local,
          now(),
          Reach(Some("C1/3000.1/3000.1"), Set.empty)
        ) ==> Right(())
        val thread = eventually(
          right(
            engine.db.read(engine.conversations.find(Origin.Slack("T1", "C1", "3000.1")))
          ).nonEmpty
        )
        assert(thread)
        val heard = TurnRef(
          right(engine.db.read(engine.conversations.find(Origin.Slack("T1", "C1", "3000.1"))))
            .fold(sys.error("heard"))(_.id),
          TurnSeq.First
        )
        assert(eventually(right(engine.db.read(engine.entries.get(heard.draftId))).nonEmpty))
        val _ = engine.awaitTurn(heard)

        def captured(): (Turns, Verdicts) = {
          val reader = Reader.open(config)
          try {
            (
              right(TurnCapture(reader, dump)),
              right(grit.eval.harness.pull.Pull.verdicts(reader, Instant.EPOCH)).verdicts
            )
          } finally reader.close()
        }
        val (turns, verdicts) = captured()
        val byWorkflow = turns.cases.map(t => t.workflow -> t).toMap
        def at(t: TurnRef): TurnCase =
          byWorkflow.getOrElse(t.workflowId, throw new java.lang.AssertionError(s"no case for $t"))

        turns.cases.map(_.workflow) ==>
          Vector(remark, question, failed, looped, heard).map(_.workflowId)
        turns.skipped ==> 0

        // Where each was said, its root and focus, and how it ended.
        turns.cases.map(t =>
          (
            t.said match {
              case Said.Slack(id) => s"slack ${id.written}"
              case Said.Tui(_) => "tui"
              case Said.Task(_) => "task"
            },
            t.root,
            t.focus,
            t.ended
          )
        ) ==> Vector(
          (
            "tui",
            TurnOffer.Root.Addressed,
            Focus.Focused,
            Ended.Replied(s"$Marker replied".length, false)
          ),
          (
            "tui",
            TurnOffer.Root.Addressed,
            Focus.Focused,
            Ended.Replied(s"$Marker Falcon 4200".length, false)
          ),
          (
            "tui",
            TurnOffer.Root.Addressed,
            Focus.Focused,
            Ended.Failed(Turn.Step.CallModel, Ended.Why.Model)
          ),
          (
            "slack C1/2000.1",
            TurnOffer.Root.Addressed,
            Focus.Open,
            Ended.Replied(s"$Marker replied".length, false)
          ),
          ("slack C1/3000.1", TurnOffer.Root.Heard, Focus.Open, Ended.Replied("pass".length, true))
        )

        // The offer: echo alone, its schema costed; the TUI session's directory as workspace.
        turns.cases.map(
          _.offered.map(o => (o.tools.map(ToolName.value), Tokens.value(o.schema) > 0))
        ) ==>
          Vector.fill(5)(Some((Vector("echo"), true)))
        at(question).offered.flatMap(_.workspace) ==> Some(tui.place)
        at(looped).offered.flatMap(_.workspace) ==> None

        // The loop: two rounds of one echo each, both ran.
        at(looped).rounds ==> Vector.fill(2)(
          Round(Vector(Call(Called.Tool(ToolName("echo")), Settled.Ok(Echoed.length))))
        )
        Vector(remark, question, failed, heard).map(at(_).rounds) ==> Vector.fill(4)(Vector.empty)

        // What each call was for.
        turns.cases.map(_.spend.map(_.role)) ==> Vector(
          Vector(Some(TurnRecord.Role.Reply), Some(TurnRecord.Role.Summary)),
          Vector(Some(TurnRecord.Role.Reply), Some(TurnRecord.Role.Summary)),
          Vector.empty,
          Vector(
            Some(TurnRecord.Role.Round(0)),
            Some(TurnRecord.Role.Round(1)),
            Some(TurnRecord.Role.Reply),
            Some(TurnRecord.Role.Summary)
          ),
          Vector(Some(TurnRecord.Role.Reply))
        )
        at(looped).spend.filter(_.model == "test/scripted").map(_.usage.input) ==> Vector.fill(3)(
          Tokens(100)
        )

        // The question's window is the record alone, which carries the whole reply.
        at(question).window.map(_.parts.map(p => (p.kind, p.support.map(_.value)))) ==>
          Some(Vector((Part.Kind.Record, Some(1.0))))
        at(question).window.exists(w =>
          Tokens.value(w.parts.foldLeft(Tokens.Zero)(_ + _.tokens)) > 0 &&
            Tokens.value(w.own) > 0
        ) ==> true
        // A passed draft supports nothing, nor does a turn that failed after its window: the
        // record, then the question's turn, shown by recency.
        at(heard).window.map(_.parts.flatMap(_.support)) ==> Some(Vector.empty)
        at(failed).window.map(_.parts.map(p => (p.kind, p.support))) ==>
          Some(Vector((Part.Kind.Record, None), (Part.Kind.Recent, None)))
        at(failed).window.flatMap(_.parts.lift(1)).map(_.seqs.size) ==> Some(2)

        // Live triage's answers on the heard root alone, and its draft passed.
        turns.cases.map(_.triage.isDefined) ==> Vector(false, false, false, false, true)
        turns.cases.map(_.speech.map(_.outcome)) ==> Vector(
          None,
          None,
          None,
          None,
          Some(Drafted.Kind.Passed)
        )
        verdicts ==> Verdicts.Empty

        // Text-free, and the same bytes again.
        def lines(t: Turns, v: Verdicts) =
          t.cases.map(TurnJson.write(_).render()).mkString("\n") + Verdicts.written(v)
        val written = lines(turns, verdicts)
        assert(!written.contains(Marker))
        assert(!written.contains("Falcon"))
        val (again, verdictsAgain) = captured()
        lines(again, verdictsAgain) ==> written
      } finally engine.close()
    }
  }
}
