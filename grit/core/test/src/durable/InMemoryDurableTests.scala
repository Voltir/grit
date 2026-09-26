package grit.core.durable

import grit.core.id.WorkflowId

/** [[DurableContract]] against [[InMemoryDurable]]. */
object InMemoryDurableTests extends DurableContract {

  def runtime: DurableRuntime = new InMemoryRuntime

  // It throws InMemoryDurable.WriteOutsideStep for the first, and keeps the message for the
  // second.
  override def divergences: Set[Divergence] =
    Set(Divergence.WriteOutsideStep, Divergence.SendBeforeStart)

  /** One [[InMemoryDurable]]; a crash is [[InMemoryDurable.Crash]] thrown from the step. */
  final class InMemoryRuntime extends DurableRuntime {
    private val durable = new InMemoryDurable

    @caps.unsafe.untrackedCaptures
    private var issued = 0

    def freshId(): WorkflowId = {
      issued += 1
      WorkflowId(s"spec:$issued")
    }

    def run(id: WorkflowId)(body: WorkflowId => Durable^ ?=> String): Settled =
      try Settled.Returned(durable.run(id)(body))
      catch {
        case _: InMemoryDurable.Crash => Settled.Crashed
        case e: Exception => Settled.Threw(e)
      }

    def recordedSteps(id: WorkflowId): Vector[String] = durable.recordedSteps(id)

    def streamed(id: WorkflowId, key: String): Vector[String] = durable.streamed(id, key)

    def crash(): Nothing = throw new InMemoryDurable.Crash

    def unexpectedStep(error: Throwable): Option[(String, String)] = error match {
      case e: InMemoryDurable.UnexpectedStep => Some((e.name, e.recorded))
      case _ => None
    }

    def send(id: WorkflowId, topic: String, message: String, key: Option[String]): Unit =
      durable.send(id, topic, message, key)

    def unreceived(id: WorkflowId, topic: String): Vector[String] = durable.unreceived(id, topic)
  }
}
