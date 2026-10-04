package grit.dbos.engine

import scala.annotation.tailrec
import scala.concurrent.duration.*
import scala.util.control.NonFatal

import grit.core.clock.Clock
import grit.core.id.WorkflowId
import grit.core.stitch.{Opening, Placements}
import grit.dbos.workflow.Stitches

import dev.dbos.transact.DBOSClient

/** [[Placements]] through `client`: an opening's placement enqueued under its workflow id,
  * which DBOS keeps once (`ON CONFLICT (workflow_uuid)`), then its result waited for; a
  * bounded wait reads its status every [[DbosPlacements.PollEvery]] until it has ended.
  */
private[engine] final class DbosPlacements(client: DBOSClient) extends Placements {

  def awaited(opening: Opening): Either[String, String] =
    try {
      enqueue(opening)
      result(opening)
    } catch {
      case NonFatal(e) => Left(why(e))
    }

  def awaitedWithin(
      opening: Opening,
      within: FiniteDuration,
      clock: Clock^
  ): Either[Placements.Unplaced, String] =
    try {
      enqueue(opening)
      val id = WorkflowId.value(opening.ref.workflowId)
      val until = clock.millis() + within.toMillis
      // A status not yet readable is waited on like one still running.
      def ended: Boolean = client.getWorkflowStatus(id).map(!_.status().isActive).orElse(false)
      @tailrec def poll(): Either[Placements.Unplaced, String] =
        if (ended) result(opening).left.map(Placements.Unplaced.Failed(_))
        else if (clock.millis() >= until) Left(Placements.Unplaced.Late)
        else {
          clock.sleep(DbosPlacements.PollEvery)
          poll()
        }
      poll()
    } catch {
      case NonFatal(e) => Left(Placements.Unplaced.Failed(why(e)))
    }

  /** `opening`'s placement queued under its workflow id, unless it is already. */
  private def enqueue(opening: Opening): Unit = {
    // The array is empty and DBOS only reads it; separation checking treats arrays as mutable.
    client.enqueueWorkflow[String, Exception](
      Stitches.enqueueOptions(opening),
      caps.unsafe.unsafeAssumePure(Array.empty[AnyRef])
    )
    ()
  }

  /** What `opening`'s placement returned, once it has ended; throws what it threw. */
  private def result(opening: Opening): Either[String, String] = {
    val handle =
      client.retrieveWorkflow[String, Exception](WorkflowId.value(opening.ref.workflowId))
    Right(Option(handle.getResult()).getOrElse(""))
  }

  private def why(e: Throwable): String = Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
}

private[engine] object DbosPlacements {

  /** How often a bounded wait reads whether its placement has ended: one indexed status read
    * each time, and at most this long past the placement's end before the wait sees it.
    */
  val PollEvery: FiniteDuration = 50.millis
}
