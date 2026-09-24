package grit.core

import grit.dbos.TestTx
import scala.caps.unsafe.untrackedCaptures

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
  *
  * `transact` hands its body a [[TestTx]], so pair it with in-memory stores. Their writes
  * are not rolled back when the body throws.
  */
final class InMemoryDurable {

  private enum Recorded {
    case Output(value: String)
    case Threw(error: Throwable)
  }

  @untrackedCaptures
  private var journals = Map.empty[WorkflowId, Vector[(String, Recorded)]]

  @untrackedCaptures
  private var outputs = Map.empty[WorkflowId, String]

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

  private final class Run(workflowId: WorkflowId) extends Durable {

    @untrackedCaptures
    private var next = 0

    def step[A: Journaled](name: String)(body: () => A): A =
      record(name, () => body())

    def transact[A: Journaled](name: String)(body: (Tx^) ?=> A): A =
      record(name, () => body(using TestTx.fake))

    private def record[A](name: String, body: () => A)(using j: Journaled[A]): A = {
      val position = next
      next += 1
      val journal = journals.getOrElse(workflowId, Vector.empty)
      journal.lift(position) match {
        case Some((recorded, _)) if recorded != name =>
          throw InMemoryDurable.UnexpectedStep(workflowId, position, name, recorded)
        case Some((_, Recorded.Threw(error))) => throw error
        case Some((_, Recorded.Output(value))) => decode(name, value)
        case None =>
          val outcome =
            try Recorded.Output(j.encode(body()))
            catch {
              case crash: InMemoryDurable.Crash => throw crash
              case e: Exception => Recorded.Threw(e)
            }
          journals = journals.updated(workflowId, journal :+ (name -> outcome))
          outcome match {
            case Recorded.Output(value) => decode(name, value)
            case Recorded.Threw(error) => throw error
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

  /** Thrown from a step body to stand for the process dying inside it. */
  final class Crash extends RuntimeException("simulated crash")

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
