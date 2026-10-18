package grit.kit.rehearse

import grit.assembly.estimate.CharEstimate
import grit.core.classify.{Ask, Question, StateJson}
import grit.core.durable.{Durable, StepRecord}
import grit.core.id.{ConversationId, TurnRef, TurnSeq, WorkflowId}
import grit.core.inbox.InboxError
import grit.core.message.Message
import grit.core.provider.ModelRequest
import grit.core.route.Routed
import grit.core.store.{Db, InMemoryModelSettingStore, StoreError, Tx}
import grit.core.visibility.{Label, Subject}
import grit.dbos.engine.Starts
import grit.dbos.sql.TestTx
import grit.kit.deployment.Topics
import grit.models.{StubClassifier, StubModels}

import utest.*
import Rehearsal.Event
import RehearsalRunner.Came
import Rehearsed.{Answered, Part}
import Tape.{Call, Entry}

/** [[Rehearsed]]: what a dry rehearsal shows of the calls its workflows made. */
object RehearsedTests extends TestSuite {

  private object FakeDb extends Db {
    def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  private given StateJson[String] = StateJson.instance(s => ujson.Str(s))

  private val turn = TurnRef(ConversationId("c"), TurnSeq.First)
  private val request = ModelRequest("Be brief.", Vector(Message.User("lunch?")))
  private val pinned = StubModels.Catalog.pin.turn.first
  private val asked = Ask.yesNo[String]("Is it about lunch?", None, None)

  /** The route record of a turn whose reads were routed for `label`. */
  private def route(label: Label): String =
    s"""{"ok":{"classification":${ujson.write(Label.written(label))},"turn":0}}"""

  /** A turn, run by a runner over taps answering by `answers`: its query written, a classifier
    * asked, its route recorded, then its own call; the tape and what it showed.
    */
  private def rehearsed(answers: StandIns): (Tape, Rehearsed) = {
    val tape = new Tape
    val seed = Taps.seed(StubModels.Catalog.policy).fold(sys.error, identity)
    val models = Taps.models(seed, FakeDb, new InMemoryModelSettingStore, answers, tape)
    val classifier = Taps.classifier(Topics.Stub, answers, tape)
    val runner = new RehearsalRunner(_ => Left(InboxError.Invalid("unused")), tape)
    val body: WorkflowId => Durable^ ?=> String = _ =>
      d ?=> {
        d.step("ask:query")(() => models.provider(pinned).complete(request).toString)
        d.step("classify")(() =>
          classifier.asking(Routed.of(using TestTx.fake)).ask("lunch?", asked).toString
        )
        d.step("route")(() => route(Label.Public))
        d.step("ask:0")(() => models.provider(pinned).complete(request).toString)
        "answered"
      }
    runner.runs(
      new RehearsalTests.Writes,
      Right(Starts.Said(None, turn)),
      body,
      _ => _ ?=> "unused",
      _ => _ ?=> "unused"
    )
    (tape, Rehearsed.of(tape.entries, runner.ran))
  }

  val tests = Tests {
    test(
      "a dry rehearsal stops at the turn's own call and shows it unsent, every earlier call answered as a stand-in"
    ) {
      val (tape, shown) = rehearsed(StandIns.supplied(Map("ask:query" -> "lunch places")))
      tape.entries.takeRight(3) ==> Vector(
        Entry.Told(Event.Began("ask:0")),
        Entry.Called(Call.Model(Some("ask:0"), pinned, request, None)),
        Entry.Told(Event.Ended(StepRecord("ask:0", None, None)))
      )
      shown.workflows.map(w => (w.workflow, w.came)) ==>
        Vector((turn.workflowId, Came.Stopped("ask:0")))
      shown.calls.map(c => (c.step, c.answered)) ==> Vector(
        (Some("ask:query"), Answered.StandIn(StandIns.StandIn.Supplied, "lunch places")),
        (
          Some("classify"),
          Answered.StandIn(StandIns.StandIn.Stub, """{"q1":{"type":"noul","noul":0.9}}""")
        ),
        (Some("ask:0"), Answered.Unsent)
      )
    }

    test(
      "a model call's input is shown by part, the system prompt and each message sized in characters and in tokens as the kit estimates them"
    ) {
      val (_, shown) = rehearsed(StandIns.Stubbed)
      shown.calls.lastOption.map(_.parts) ==> Some(
        Vector(
          Part("system", "Be brief.", 9, CharEstimate.system("Be brief.")),
          Part("message 0, user", "lunch?", 6, CharEstimate.message(Message.User("lunch?")))
        )
      )
    }

    test(
      "a classifier call's questions and answers are shown as the classifier's wire carries them"
    ) {
      val choice = Question
        .choice("Which room?", Question.Key("here", None), Question.Key("there", None))
        .fold(e => sys.error(e.toString), identity)
      val questions = asked.questions :+ choice
      val shown = Rehearsed.of(
        Vector(
          Entry.Told(Event.Running(WorkflowId("t"))),
          Entry.Told(Event.Began("ask:triage")),
          Entry.Called(
            Call.Classifier(
              Some("ask:triage"),
              StubClassifier.Service,
              ujson.Obj(),
              questions,
              StandIns
                .supplied(Map("ask:triage" -> "0.25;1,0"))
                .judge("ask:triage", ujson.Obj(), questions)
            )
          ),
          Entry.Told(Event.Ended(StepRecord("ask:triage", None, None)))
        ),
        Vector(RehearsalRunner.Finished(WorkflowId("t"), Came.Threw("broke")))
      )
      (
        shown.calls.flatMap(_.parts).drop(1).map(_.text),
        shown.calls.map(_.answered)
      ) ==> (
        Vector(
          """{"type":"noul","instructions":"Is it about lunch?"}""",
          """{"type":"choice","instructions":"Which room?","criteria":{"here":null,"there":null}}"""
        ),
        Vector(
          Answered.StandIn(
            StandIns.StandIn.Supplied,
            """{"q1":{"type":"noul","noul":0.25},"q2":{"type":"choice","choice":"here","probabilities":{"here":1,"there":0},"confidence":1}}"""
          )
        )
      )
    }

    test(
      "a turn's calls are shown under the label its route recorded, and a call no record names as unrecorded"
    ) {
      val (_, shown) = rehearsed(StandIns.Stubbed)
      shown.calls.map(_.label) ==> Vector.fill(3)(Some(Label.Public))
      val elsewhere = Rehearsed.of(
        Vector(
          Entry.Told(Event.Running(WorkflowId("t"))),
          Entry.Told(Event.Began("ask:triage")),
          Entry.Called(Call.Model(Some("ask:triage"), pinned, request, None)),
          Entry.Told(Event.Ended(StepRecord("ask:triage", None, None)))
        ),
        Vector(RehearsalRunner.Finished(WorkflowId("t"), Came.Threw("broke")))
      )
      elsewhere.calls.map(_.label) ==> Vector(None)
    }

    test(
      "its shape names each part with its size, each answer by its kind, and holds no text sent or answered"
    ) {
      val (_, shown) = rehearsed(StandIns.supplied(Map("ask:query" -> "lunch places")))
      val counted = shown.counted
      (
        counted.startsWith(Rehearsed.Header),
        counted.contains("lunch"),
        counted.contains("Be brief"),
        counted.contains("    system: 9 chars, ~7 tokens"),
        counted.contains("answered: STAND-IN (supplied)"),
        counted.contains("answered: STAND-IN (stub)"),
        counted.contains("not sent: the turn's own call"),
        shown.written.contains("lunch places")
      ) ==> (true, false, false, true, true, true, true, true)
    }

    test(
      "a rehearsal with no classifier call has no stub-judge line in its header, and one that asked a classifier names the stub judge"
    ) {
      val elsewhere = Rehearsed.of(
        Vector(
          Entry.Told(Event.Running(WorkflowId("t"))),
          Entry.Told(Event.Began("ask:triage")),
          Entry.Called(Call.Model(Some("ask:triage"), pinned, request, None)),
          Entry.Told(Event.Ended(StepRecord("ask:triage", None, None)))
        ),
        Vector(RehearsalRunner.Finished(WorkflowId("t"), Came.Threw("broke")))
      )
      val (_, judged) = rehearsed(StandIns.Stubbed)
      (
        elsewhere.written.contains("stub classifier"),
        judged.written.contains("stub classifier")
      ) ==> (false, true)
    }
  }
}
