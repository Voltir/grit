package grit.lifecycle.replay

import grit.core.durable.{Flow, FlowFixtures, Flows, History, StepRecord}
import grit.core.id.WorkflowId
import grit.lifecycle.settle.Settle
import grit.lifecycle.shadow.Shadow
import grit.lifecycle.stitch.Stitch
import grit.turn.Turn

import utest.*

/** The read gate for the lifecycle's workflows: each history of the current epoch, read over
  * its workflow's flow from its id with no environment, names at every prefix the record the
  * run made next, and as a whole reads as ended. A flow whose description differs from what
  * its runs recorded fails here, though its replay may pass.
  */
object LifecycleReadTests extends TestSuite {

  private def histories: Vector[(os.Path, Either[String, History])] = {
    val dir = os.Path(sys.env("GRIT_HISTORIES")) / Turn.Epoch
    if (!os.isDir(dir)) Vector.empty
    else
      os.list(dir)
        .filter(_.ext == "json")
        .toVector
        .map(p => p -> History.read(ujson.read(os.read(p))))
  }

  /** Each workflow's flow, by the name its histories are recorded under. */
  private val flows: Map[String, Flow[?, WorkflowId, ?, ?]] = Map(
    "stitch" -> Stitch.flow,
    "shadow" -> Shadow.flow,
    "settle" -> Settle.flow
  )

  /** Where `history` reads otherwise than as its run recorded it, over `flow`. */
  private def misread(flow: Flow[?, WorkflowId, ?, ?], history: History): Vector[String] = {
    val records: Vector[StepRecord] = history.steps.map(FlowFixtures.recorded)
    val prefixes = records.indices.toVector.flatMap { k =>
      Flows.read(flow, history.id, records.take(k)).map(_.at) match {
        case Right(Flows.Read.Next(step, at)) if step == records(k).name && at == k => None
        case other => Some(s"at $k, before ${records(k).name}: $other")
      }
    }
    val whole = Flows.read(flow, history.id, records).map(_.at) match {
      case Right(Flows.Read.Ended(_, steps)) if steps == records.size => None
      case other => Some(s"whole: $other")
    }
    prefixes ++ whole
  }

  val tests = Tests {
    test(
      "every lifecycle history of the current epoch names its next record at every prefix, and as a whole has ended"
    ) {
      val read = histories.flatMap {
        case (path, Left(why)) => Vector(s"${path.last}: $why")
        case (path, Right(history)) =>
          flows.get(history.workflow) match {
            case None => Vector.empty
            case Some(flow) => misread(flow, history).map(m => s"${path.last}: $m")
          }
      }
      assert(histories.nonEmpty)
      read ==> Vector.empty
    }
  }
}
