package grit.kit.rehearse

import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

import grit.core.durable.{Durable, Journaled, StepRecord, StreamWriter, UnreadableJournal}
import grit.core.id.WorkflowId
import grit.core.store.{Jot, StoreError, Tx}
import grit.core.visibility.Subject

/** A [[Durable]] that runs the body of workflow `workflowId` once, now, keeping each step's
  * output nowhere but in what it tells `told`: every body runs (nothing replays, and a run never
  * resumes); `transact` runs its body in a transaction `writes` opens for its subject, undone
  * when the body throws, and throws what the body threw, or [[Rehearsal.Unwritten]] when the
  * transaction cannot open or keep what it wrote; `patch` takes every new branch; a stream's
  * pieces are told and kept nowhere; `recv` answers `None` at once. A step whose output its
  * [[Journaled]] cannot read back throws [[UnreadableJournal]], as a run that records it does.
  *
  * `told` hears each operation begin and end, in order, as a run on the engine would record
  * them: a taken patch as its marker, a `recv` as its two records. For seeing what a shipped
  * body would do; never for a workflow that must survive a crash.
  */
final class Rehearsal(workflowId: WorkflowId, writes: Jot^, told: Rehearsal.Told^) extends Durable {
  import Rehearsal.*

  def step[A: Journaled](name: String)(body: () => A): A =
    operation(name)(() => body())

  def transact[A: Journaled](name: String, subject: Subject)(body: (Tx^) ?=> A): A =
    operation(name) { () =>
      // Set only by the body below, on this thread, inside this call: what the body threw,
      // turned into a `Left` so `writes` undoes it, then rethrown.
      var thrown: Option[Throwable] = None
      writes.write(subject) {
        try Right(body)
        catch {
          case NonFatal(e) =>
            thrown = Some(e)
            Left(StoreError.DatabaseError(s"the body of '$name' threw"))
        }
      } match {
        case Right(a) => a
        case Left(error) => throw thrown.getOrElse(Unwritten(name, error))
      }
    }

  def patch(name: String): Boolean = {
    marked(StepRecord.marker(name), None)
    true
  }

  // A rehearsal takes every new branch, so there is no marked history to step past.
  def deprecatePatch(name: String): Unit = ()

  def stream(key: String): StreamWriter = {
    val tell = told
    new StreamWriter {
      def write(piece: String): Unit = tell(Event.Streamed(key, piece))
    }
  }

  def recv(topic: String, timeout: FiniteDuration): Option[String] = {
    marked(StepRecord.Received, None)
    marked(StepRecord.Slept, Some(timeout.toMillis.toString))
    None
  }

  /** `body` run as the operation `name`, told as it begins and as it ends. */
  private def operation[A](name: String)(body: () => A)(using j: Journaled[A]): A = {
    told(Event.Began(name))
    val encoded =
      try j.encode(body())
      catch {
        case e: Throwable =>
          told(Event.Ended(StepRecord(name, None, None)))
          throw e
      }
    told(Event.Ended(StepRecord(name, Some(encoded), None)))
    decode(name, encoded)
  }

  /** An operation that runs nothing, told as one recording `output` would be. */
  private def marked(name: String, output: Option[String]): Unit = {
    told(Event.Began(name))
    told(Event.Ended(StepRecord(name, output, None)))
  }

  // A fresh output is decoded too, so a step returns what a run that recorded it would.
  private def decode[A](name: String, value: String)(using j: Journaled[A]): A =
    j.decode(value) match {
      case Right(a) => a
      case Left(reason) => throw UnreadableJournal(workflowId, name, reason)
    }
}

object Rehearsal {

  /** What a rehearsal tells as it runs. */
  enum Event {

    /** The operation `step` has begun. */
    case Began(step: String)

    /** An operation has ended, its output as its [[Journaled]] encoded it: `None` when it
      * threw, or when it records none, as a patch's marker and a wait's first record do.
      */
    case Ended(record: StepRecord)

    /** `piece` was written to the stream `key`. */
    case Streamed(key: String, piece: String)
  }

  /** Hears what a rehearsal tells, on the thread its body runs each operation on. */
  trait Told extends caps.SharedCapability {
    def apply(event: Event): Unit
  }

  /** The transaction of the `transact` step `step` could not open or keep its writes. */
  final case class Unwritten(step: String, error: StoreError)
      extends RuntimeException(s"rehearsed step '$step' could not write: $error")
}
