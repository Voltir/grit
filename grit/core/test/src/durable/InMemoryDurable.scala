package grit.core.durable

import scala.caps.unsafe.untrackedCaptures

import grit.core.id.WorkflowId
import grit.core.store.Tx
import grit.dbos.sql.TestTx

/** An in-memory stand-in for DBOS's workflow semantics, for tests of code written against
  * [[Durable]]: one journal per workflow id, kept across runs of the same id.
  *
  *   - A workflow that returned is not run again; its output is returned.
  *   - Any other run, after a crash or a thrown exception, runs the body again. Each
  *     recorded step returns its recorded output, or rethrows its recorded exception,
  *     without running its body (as DBOS 1.0.0 does for an `ERROR` workflow).
  *   - A step whose name differs from the one recorded at its position throws
  *     [[InMemoryDurable.UnexpectedStep]].
  *   - A step body that throws [[InMemoryDurable.Crash]] is left unrecorded, as if the
  *     process died inside it.
  *   - `patch` and `deprecatePatch` record and read DBOS's own marker,
  *     `DBOS.patch-{name}`, so a history captured from Postgres replays here unchanged.
  *
  * `transact` hands its body a [[TestTx]], so pair it with in-memory stores. Their writes
  * are not rolled back when the body throws.
  */
final class InMemoryDurable {
  import InMemoryDurable.*

  private enum Recorded {
    case Output(value: String)
    case Threw(error: Throwable)
    case Marker
  }

  @untrackedCaptures
  private var journals = Map.empty[WorkflowId, Vector[(String, Recorded)]]

  @untrackedCaptures
  private var outputs = Map.empty[WorkflowId, String]

  /** How far the last run of each workflow got through its journal. */
  @untrackedCaptures
  private var reached = Map.empty[WorkflowId, Int]

  /** Runs the workflow `id`, or returns its output if a run of it already returned. */
  def run(id: WorkflowId)(body: WorkflowId => Durable^ ?=> String): String =
    outputs.get(id) match {
      case Some(output) => output
      case None =>
        val output = body(id)(using new Run(id))
        outputs = outputs.updated(id, output)
        output
    }

  /** The names of the steps recorded for `id`, in order. */
  def recordedSteps(id: WorkflowId): Vector[String] =
    journals.getOrElse(id, Vector.empty).map(_._1)

  /** The journal of `id`, in the form a history fixture keeps. */
  def history(id: WorkflowId): Vector[Step] =
    journals.getOrElse(id, Vector.empty).map {
      case (name, Recorded.Output(value)) => Step(name, Some(value), None)
      case (name, Recorded.Threw(error)) => Step(name, None, Some(String.valueOf(error.getMessage)))
      case (name, Recorded.Marker) => Step(name, None, None)
    }

  /** Runs `body` as a resumption of a workflow that recorded `steps`, as the current build
    * would after a restart. `Left` names how the body failed to follow the history: a step
    * named differently at a recorded position, an output it cannot read, or an end before
    * the last recorded step. A body that goes on past the history runs its new steps.
    */
  def replay(id: WorkflowId, steps: Vector[Step])(
      body: WorkflowId => Durable^ ?=> String
  ): Either[String, String] = {
    val seeded = steps.map {
      case Step(name, Some(output), _) => name -> Recorded.Output(output)
      case Step(name, None, Some(error)) => name -> Recorded.Threw(new RecordedError(error))
      case Step(name, None, None) => name -> Recorded.Marker
    }
    journals = journals.updated(id, seeded)
    outputs = outputs.removed(id)
    val outcome =
      try Right(run(id)(body))
      catch {
        case e: UnexpectedStep => Left(e.getMessage)
        case e: UnreadableJournal => Left(e.getMessage)
        case e: RecordedError => Right(s"threw the recorded error: ${e.getMessage}")
      }
    outcome.flatMap { output =>
      val got = reached.getOrElse(id, 0)
      if (got < steps.size)
        Left(s"ended after $got of ${steps.size} recorded steps; next was '${steps(got).name}'")
      else Right(output)
    }
  }

  private final class Run(workflowId: WorkflowId) extends Durable {

    @untrackedCaptures
    private var next = 0

    def step[A: Journaled](name: String)(body: () => A): A =
      record(name, () => body())

    def transact[A: Journaled](name: String)(body: (Tx^) ?=> A): A =
      record(name, () => body(using TestTx.fake))

    def patch(name: String): Boolean = {
      val marker = patchMarker(name)
      journal.lift(next) match {
        case None =>
          journals = journals.updated(workflowId, journal :+ (marker -> Recorded.Marker))
          advance()
          true
        case Some((recorded, _)) if recorded == marker =>
          advance()
          true
        case Some(_) => false
      }
    }

    def deprecatePatch(name: String): Unit =
      journal.lift(next) match {
        case Some((recorded, _)) if recorded == patchMarker(name) => advance()
        case _ => ()
      }

    private def journal: Vector[(String, Recorded)] = journals.getOrElse(workflowId, Vector.empty)

    private def advance(): Unit = {
      next += 1
      reached = reached.updated(workflowId, next)
    }

    private def record[A](name: String, body: () => A)(using j: Journaled[A]): A = {
      val position = next
      advance()
      journal.lift(position) match {
        case Some((recorded, _)) if recorded != name =>
          throw UnexpectedStep(workflowId, position, name, recorded)
        case Some((_, Recorded.Threw(error))) => throw error
        case Some((_, Recorded.Output(value))) => decode(name, value)
        case Some((_, Recorded.Marker)) =>
          throw UnexpectedStep(workflowId, position, name, "a patch marker")
        case None =>
          val outcome =
            try Recorded.Output(j.encode(body()))
            catch {
              case crash: Crash => throw crash
              case e: Exception => Recorded.Threw(e)
            }
          journals = journals.updated(workflowId, journal :+ (name -> outcome))
          outcome match {
            case Recorded.Output(value) => decode(name, value)
            case Recorded.Threw(error) => throw error
            case Recorded.Marker => sys.error("unreachable: a step never records a marker")
          }
      }
    }

    // A fresh output is decoded too, so a first run returns exactly what a replay would.
    private def decode[A](name: String, value: String)(using j: Journaled[A]): A =
      j.decode(value) match {
        case Right(a) => a
        case Left(reason) => throw UnreadableJournal(workflowId, name, reason)
      }
  }
}

object InMemoryDurable {

  /** One recorded step: its name, and its output, its error's message, or neither for a
    * patch marker.
    */
  final case class Step(name: String, output: Option[String], error: Option[String])

  /** DBOS's step name for the patch `name` (`DBOSExecutor.patch`). */
  def patchMarker(name: String): String = s"DBOS.patch-$name"

  /** Thrown from a step body to stand for the process dying inside it. */
  final class Crash extends RuntimeException("simulated crash")

  /** A recorded step's error, rethrown on replay of a loaded history. */
  final class RecordedError(message: String) extends RuntimeException(message)

  /** A run reached a step under a different name than the one recorded at its position. */
  final case class UnexpectedStep(
      workflowId: WorkflowId,
      position: Int,
      name: String,
      recorded: String
  ) extends RuntimeException(
        s"workflow ${WorkflowId.value(workflowId)} step $position: ran '$name', recorded '$recorded'"
      )
}
