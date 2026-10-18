package grit.kit.rehearse

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.control.NonFatal

import grit.core.durable.{Durable, StepRecord}
import grit.core.id.WorkflowId
import grit.core.store.{Jot, StoreError, Tx}
import grit.core.visibility.Subject
import grit.dbos.sql.TestTx

import utest.*
import Rehearsal.Event

/** [[Rehearsal]]'s own semantics, beside the [[grit.core.durable.DurableContract]] cases it
  * keeps ([[RehearsalContractTests]]).
  */
object RehearsalTests extends TestSuite {

  /** A [[Jot]] over no database: each write runs its body at once against a fake `Tx`, kept
    * when it returns a `Right` and undone otherwise, a body that throws a `Left`, as on the real
    * one; `refusing` stands for a transaction that cannot open, its body never run.
    */
  final class Writes(refusing: Option[StoreError] = None) extends Jot {
    // Read only by the test's assertions, on the test's own thread.
    @caps.unsafe.untrackedCaptures
    var opened = Vector.empty[Subject]
    @caps.unsafe.untrackedCaptures
    var kept = 0
    @caps.unsafe.untrackedCaptures
    var undone = 0

    def write[A](subject: Subject)(
        body: (Tx^) ?=> Either[StoreError, A]
    ): Either[StoreError, A] = refusing match {
      case Some(error) => Left(error)
      case None =>
        opened = opened :+ subject
        val result =
          try body(using TestTx.fake)
          catch { case NonFatal(e) => Left(StoreError.DatabaseError(e.toString)) }
        result.fold(_ => undone += 1, _ => kept += 1)
        result
    }
  }

  /** What one rehearsal of `body` told, with what it returned or threw. */
  private def rehearse(writes: Jot^)(
      body: Durable^ ?=> String
  ): (Either[Throwable, String], Vector[Event]) = {
    val tape = Vector.newBuilder[Event]
    val rehearsal = new Rehearsal(WorkflowId("rehearsal:1"), writes, e => { tape += e; () })
    val out =
      try Right(body(using rehearsal))
      catch { case NonFatal(e) => Left(e) }
    (out, tape.result())
  }

  val tests = Tests {
    test("a step's output is told when it ends, and a second run runs the body again") {
      // Counted only on the test's own thread, by the step below.
      @caps.unsafe.untrackedCaptures
      var ran = 0
      def wf(using d: Durable^): String = d.step("a") { () => ran += 1; s"run $ran" }
      val first = rehearse(new Writes)(wf)
      val second = rehearse(new Writes)(wf)
      (first, second, ran) ==> (
        (
          Right("run 1"),
          Vector(Event.Began("a"), Event.Ended(StepRecord("a", Some("run 1"), None)))
        ),
        (
          Right("run 2"),
          Vector(Event.Began("a"), Event.Ended(StepRecord("a", Some("run 2"), None)))
        ),
        2
      )
    }

    test(
      "the turn's record-speech is not run: the rehearsal stops before it, the step untold and nothing written"
    ) {
      val writes = new Writes
      val told = Vector.newBuilder[Event]
      val rehearsal = new Rehearsal(WorkflowId("c:0"), writes, e => { told += e; () })
      val subject = Subject.Conversation(grit.core.id.ConversationId("c1"))
      val stopped =
        try {
          rehearsal.step("judge")(() => "passed")
          rehearsal.transact(grit.turn.Turn.Step.RecordSpeech, subject)("posted")
          None
        } catch { case s: Stop => Some(s) }
      (stopped, told.result(), writes.opened) ==> (
        Some(Stop(grit.turn.Turn.Step.RecordSpeech)),
        Vector(Event.Began("judge"), Event.Ended(StepRecord("judge", Some("passed"), None))),
        Vector()
      )
    }

    test("transact runs its body in a transaction writes opens for its subject, kept") {
      val writes = new Writes
      val subject = Subject.Conversation(grit.core.id.ConversationId("c1"))
      val (out, told) = rehearse(writes)(d ?=> d.transact("w", subject) { "written" })
      (out, told, writes.opened, writes.kept, writes.undone) ==> (
        Right("written"),
        Vector(Event.Began("w"), Event.Ended(StepRecord("w", Some("written"), None))),
        Vector(subject),
        1,
        0
      )
    }

    test("a transact body that throws is undone, its throw rethrown and told as no output") {
      val writes = new Writes
      val (out, told) = rehearse(writes)(d ?=>
        d.transact[String]("w", Subject.Public) { throw new IllegalStateException("boom") }
      )
      (
        out.left.map(e => (e.getClass, e.getMessage)),
        told,
        writes.kept,
        writes.undone
      ) ==> (
        Left((classOf[IllegalStateException], "boom")),
        Vector(Event.Began("w"), Event.Ended(StepRecord("w", None, None))),
        0,
        1
      )
    }

    test("a transact whose transaction cannot open throws why, its body not run") {
      // Counted only on the test's own thread, by the body below.
      @caps.unsafe.untrackedCaptures
      var ran = 0
      val refused = StoreError.DatabaseError("down")
      val (out, _) = rehearse(new Writes(Some(refused)))(d ?=>
        d.transact("w", Subject.Public) { ran += 1; "written" }
      )
      (out, ran) ==> (Left(Rehearsal.Unwritten("w", refused)), 0)
    }

    test("every operation is told in order: a taken patch's marker, a stream's pieces, a wait") {
      def wf(using d: Durable^): String = {
        val out = d.stream("k")
        val branch = if (d.patch("p")) "new" else "old"
        val said = d.step(branch) { () => out.write("piece"); branch }
        said + d.recv("t", 5.minutes).getOrElse(",none")
      }
      val started = System.nanoTime()
      val (out, told) = rehearse(new Writes)(wf)
      val waited = FiniteDuration(System.nanoTime() - started, "ns")
      (out, told, waited < 1.minute) ==> (
        Right("new,none"),
        Vector(
          Event.Began(StepRecord.marker("p")),
          Event.Ended(StepRecord(StepRecord.marker("p"), None, None)),
          Event.Began("new"),
          Event.Streamed("k", "piece"),
          Event.Ended(StepRecord("new", Some("new"), None)),
          Event.Began(StepRecord.Received),
          Event.Ended(StepRecord(StepRecord.Received, None, None)),
          Event.Began(StepRecord.Slept),
          Event.Ended(StepRecord(StepRecord.Slept, Some("300000"), None))
        ),
        true
      )
    }
  }
}
