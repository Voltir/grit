package grit.dbos.engine

import scala.util.control.NonFatal

import grit.core.id.WorkflowId
import grit.core.stitch.{Opening, Placements}
import grit.dbos.workflow.Stitches

import dev.dbos.transact.DBOSClient

/** [[Placements]] through `client`: an opening's placement enqueued under its workflow id,
  * which DBOS keeps once (`ON CONFLICT (workflow_uuid)`), then its result waited for.
  */
private[engine] final class DbosPlacements(client: DBOSClient) extends Placements {

  def awaited(opening: Opening): Either[String, String] =
    try {
      // The array is empty and DBOS only reads it; separation checking treats arrays as mutable.
      client.enqueueWorkflow[String, Exception](
        Stitches.enqueueOptions(opening),
        caps.unsafe.unsafeAssumePure(Array.empty[AnyRef])
      )
      val handle =
        client.retrieveWorkflow[String, Exception](WorkflowId.value(opening.ref.workflowId))
      Right(Option(handle.getResult()).getOrElse(""))
    } catch {
      case NonFatal(e) => Left(Option(e.getMessage).getOrElse(e.getClass.getSimpleName))
    }
}
