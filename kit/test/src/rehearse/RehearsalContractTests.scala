package grit.kit.rehearse

import grit.core.durable.{Divergence, Durable, DurableContract, DurableRuntime, Settled, StepRecord}
import grit.core.id.WorkflowId

/** [[DurableContract]] against [[Rehearsal]]: every case that holds of a runtime keeping nothing
  * between runs and delivering no message holds of it, and every case that needs either fails.
  */
object RehearsalContractTests extends DurableContract {

  def runtime: DurableRuntime = new RehearsalRuntime

  // A rehearsal keeps nothing between runs and receives nothing; a stream write outside a step
  // is told as a piece, not recorded as an operation.
  override def divergences: Set[Divergence] =
    Set(Divergence.Replay, Divergence.Mail, Divergence.WriteOutsideStep)

  /** A fresh [[Rehearsal]] per run, over [[RehearsalTests.Writes]]; what each run of an id told is
    * its record, and its stream pieces are kept across its runs. A crash is [[Crash]] thrown from
    * the step.
    */
  final class RehearsalRuntime extends DurableRuntime {
    private final class Crash extends RuntimeException("simulated crash")

    // Read and written only by the runtime's own calls, on the test's own thread.
    @caps.unsafe.untrackedCaptures
    private var issued = 0
    @caps.unsafe.untrackedCaptures
    private var lastRun = Map.empty[WorkflowId, Vector[Rehearsal.Event]]
    @caps.unsafe.untrackedCaptures
    private var pieces = Map.empty[(WorkflowId, String), Vector[String]]

    def freshId(): WorkflowId = {
      issued += 1
      WorkflowId(s"rehearsal:$issued")
    }

    def run(id: WorkflowId)(body: WorkflowId => Durable^ ?=> String): Settled = {
      val tape = Vector.newBuilder[Rehearsal.Event]
      val rehearsal = new Rehearsal(
        id,
        new RehearsalTests.Writes,
        e => {
          tape += e
          e match {
            case Rehearsal.Event.Streamed(key, piece) =>
              pieces = pieces.updated((id, key), pieces.getOrElse((id, key), Vector.empty) :+ piece)
            case _ => ()
          }
        }
      )
      val settled =
        try Settled.Returned(body(id)(using rehearsal))
        catch {
          case _: Crash => Settled.Crashed
          case e: Exception => Settled.Threw(e)
        }
      lastRun = lastRun.updated(id, tape.result())
      settled
    }

    def recordedSteps(id: WorkflowId): Vector[String] = recorded(id).map(_.name)

    def recorded(id: WorkflowId): Vector[StepRecord] =
      lastRun.getOrElse(id, Vector.empty).collect { case Rehearsal.Event.Ended(record) => record }

    def streamed(id: WorkflowId, key: String): Vector[String] =
      pieces.getOrElse((id, key), Vector.empty)

    def crash(): Nothing = throw new Crash

    // Nothing is recorded, so no step is ever checked against a recorded name.
    def unexpectedStep(error: Throwable): Option[(String, String)] = None

    // A rehearsal has no mailbox: a send reaches nothing, refused or kept.
    def send(id: WorkflowId, topic: String, message: String, key: Option[String]): Unit = ()

    def noSuchWorkflow(error: Throwable): Option[WorkflowId] = None

    def unreceived(id: WorkflowId, topic: String): Vector[String] = Vector.empty
  }
}
