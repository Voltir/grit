package grit.kit.rehearse

import java.time.Instant

import scala.concurrent.duration.DurationInt

import grit.core.clock.Clock
import grit.core.durable.{Durable, StepRecord}
import grit.core.id.{
  ConversationId,
  EntryId,
  EntrySeq,
  PeriodRef,
  PeriodSeq,
  TriageRef,
  TurnRef,
  TurnSeq,
  WorkflowId
}
import grit.core.identity.Account
import grit.core.inbox.InboxError
import grit.core.stitch.{Opening, Placements}
import grit.core.store.{Conversation, Entry, Origin, Payload}
import grit.core.visibility.{Integrity, Label}
import grit.dbos.engine.Starts

import utest.*
import Rehearsal.Event
import RehearsalRunner.{Came, Ran}

/** [[RehearsalRunner]]: what it runs of a situation, in what order, and what its placements
  * answer.
  */
object RehearsalRunnerTests extends TestSuite {

  private val Now = Instant.parse("2026-10-17T09:00:00Z")

  /** The opening of conversation `c`'s first turn, a message heard in a Slack thread. */
  private def opening(c: String): Opening = {
    val id = ConversationId(c)
    val conversation =
      Conversation(id, Origin.Slack("T", "C1", s"$c.0"), Account.Local, Now, Label.Public)
    val entry =
      Entry(
        EntryId(s"$c:0"),
        id,
        TurnSeq.First,
        None,
        EntrySeq(0),
        Payload.Heard("lunch?"),
        Now,
        Integrity.Member
      )
    Opening
      .of(conversation, Vector(entry), TurnRef(id, TurnSeq.First))
      .getOrElse(sys.error("no opening"))
  }

  private def turn(n: Long) = TurnRef(ConversationId("c"), TurnSeq(n))
  private val heard = TriageRef(PeriodRef(ConversationId("c"), PeriodSeq.First), TurnSeq.First)

  /** A body that records one step named for its workflow, then does `after`, returning `id`. */
  private def body(
      after: WorkflowId => Unit = _ => ()
  ): WorkflowId ->{after} Durable^ ?->{after} String =
    id =>
      d ?=> {
        d.step(s"step:${WorkflowId.value(id)}")(() => "done")
        after(id)
        WorkflowId.value(id)
      }

  /** The steps told begun, in order. */
  private def begun(tape: Vector[Event]): Vector[String] =
    tape.collect { case Event.Began(step) => step }

  private def runner(tape: collection.mutable.ArrayBuffer[Event]): RehearsalRunner =
    new RehearsalRunner(_ => Left(InboxError.Invalid("unused")), e => { tape += e; () })

  val tests = Tests {
    test(
      "a heard opening is placed first, its triage next, then each turn it started once, in the order started, each body told as it runs"
    ) {
      val tape = collection.mutable.ArrayBuffer[Event]()
      val r = runner(tape)
      val o = opening("c")
      val seenPlaced = collection.mutable.ArrayBuffer[Either[String, String]]()
      r.runs(
        new RehearsalTests.Writes,
        Right(Starts.Heard(Some(o), Some(heard))),
        turn = body(),
        triage = body(_ => {
          seenPlaced += r.placements.awaited(o)
          r.started(turn(2))
          r.started(turn(1))
          r.started(turn(2))
          ()
        }),
        stitch = body()
      )
      val ids = Vector(o.ref.workflowId, heard.workflowId, turn(2).workflowId, turn(1).workflowId)
      r.ran ==> ids.map(id => Ran(id, Came.Returned(WorkflowId.value(id))))
      seenPlaced.toVector ==> Vector(Right(WorkflowId.value(o.ref.workflowId)))
      begun(tape.toVector) ==> ids.map(id => s"step:${WorkflowId.value(id)}")
      r.arrived ==> Some(Right(Starts.Heard(Some(o), Some(heard))))
    }

    test(
      "an opening it did not place is a Left, never a wait, and a bounded wait for one Failed"
    ) {
      val r = runner(collection.mutable.ArrayBuffer[Event]())
      val placed = opening("c")
      val other = opening("d")
      r.runs(
        new RehearsalTests.Writes,
        Right(Starts.Said(Some(placed), turn(0))),
        turn = body(),
        triage = body(),
        stitch = body()
      )
      val unplaced = s"${WorkflowId.value(other.ref.workflowId)} was not placed in this rehearsal"
      r.placements.awaited(other) ==> Left(unplaced)
      r.placements.awaitedWithin(other, 1.second, Clock.system()) ==>
        Left(Placements.Unplaced.Failed(unplaced))
      r.placements.awaited(placed) ==> Right(WorkflowId.value(placed.ref.workflowId))
    }

    test(
      "a body stopped at the turn's own call ends the rehearsal: no queued turn runs, and a turn started after is refused"
    ) {
      val tape = collection.mutable.ArrayBuffer[Event]()
      val r = runner(tape)
      r.runs(
        new RehearsalTests.Writes,
        Right(Starts.Heard(None, Some(heard))),
        turn = _ =>
          d ?=> {
            d.step[String]("ask:0")(() => throw Stop("ask:0"))
            "answered"
          },
        triage = body(_ => { r.started(turn(1)); r.started(turn(2)); () }),
        stitch = body()
      )
      r.ran ==> Vector(
        Ran(heard.workflowId, Came.Returned(WorkflowId.value(heard.workflowId))),
        Ran(turn(1).workflowId, Came.Stopped("ask:0"))
      )
      tape.toVector.takeRight(2) ==> Vector(
        Event.Began("ask:0"),
        Event.Ended(StepRecord("ask:0", None, None))
      )
      r.started(turn(3)) ==>
        Left(InboxError.Invalid("the rehearsal has stopped"))
    }

    test("a body that throws ends there, and the turns it started before still run") {
      val r = runner(collection.mutable.ArrayBuffer[Event]())
      r.runs(
        new RehearsalTests.Writes,
        Right(Starts.Heard(None, Some(heard))),
        turn = body(),
        triage = body(_ => { r.started(turn(1)); throw new IllegalStateException("broke") }),
        stitch = body()
      )
      r.ran ==> Vector(
        Ran(heard.workflowId, Came.Threw("java.lang.IllegalStateException: broke")),
        Ran(turn(1).workflowId, Came.Returned(WorkflowId.value(turn(1).workflowId)))
      )
    }

    test("a message its session refused runs nothing, and is what it arrived as") {
      val r = runner(collection.mutable.ArrayBuffer[Event]())
      val refused = InboxError.Invalid("no such room")
      r.runs(new RehearsalTests.Writes, Left(refused), body(), body(), body())
      (r.arrived, r.ran) ==> (Some(Left(refused)), Vector())
    }
  }
}
