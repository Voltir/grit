package grit.job.replay

import grit.core.durable.{FlowFixtures, Flows, History, StepRecord}
import grit.core.job.Jobs
import grit.job.run.Run
import grit.job.run.RunFixtures.*
import grit.turn.Turn

import utest.*

/** The read gate for the run workflow: each run history of the current epoch, read over
  * [[Run.flow]] from its id with no environment, names at every prefix the record the run made
  * next, and as a whole reads as ended. A prefix cut inside the job's moves is read by running
  * its planner again over the moves the prefix holds. A flow whose description differs from
  * what its runs recorded fails here, though its replay may pass.
  */
object JobReadTests extends TestSuite {

  private def histories: Vector[(os.Path, Either[String, History])] = {
    val dir = os.Path(sys.env("GRIT_HISTORIES")) / Turn.Epoch
    if (!os.isDir(dir)) Vector.empty
    else
      os.list(dir)
        .filter(_.ext == "json")
        .toVector
        .map(p => p -> History.read(ujson.read(os.read(p))))
  }

  /** The one history whose run never ended: it made a move, and was redeployed before its next
    * step.
    */
  private val Unended = "run-moved-then-redeployed.json"

  /** The jobs `history` was recorded with: [[Noting]] for one that kept, [[probes]] for one
    * that made another move, else `remind`, which makes none.
    */
  private def jobsFor(history: History): Jobs =
    if (history.steps.exists(_.name == "move:note")) keepingJobs(new Noting(1))
    else if (history.steps.exists(_.name.startsWith("move:"))) jobsOf(probing(1))
    else jobs(1)

  /** The name a reader gives `records(k)`: its own, or for a wait's record, the step that ends
    * it when its edge is silent, of the latest call before it: its first wait's `expire`, its
    * second's `abandon`.
    */
  private def named(records: Vector[StepRecord], k: Int): String = {
    val name = records(k).name
    if (name != StepRecord.Received && name != StepRecord.Slept) name
    else {
      val before = records.take(k).map(_.name)
      val call = before.lastIndexWhere(n => n.startsWith("move:") && n.count(_ == ':') == 1)
      val waits = before.drop(call + 1).count(_ == StepRecord.Received)
      val phase = if (name == StepRecord.Received) waits else waits - 1
      s"${before(call)}:${if (phase == 0) "expire" else "abandon"}"
    }
  }

  /** Where `history` reads otherwise than as its run recorded it, over `jobs`' flow. */
  private def misread(history: History, unended: Boolean): Vector[String] = {
    val world = new World().env()
    val flow = Run.flow(jobsFor(history), world.budget, world.keepers)
    val records = history.steps.map(FlowFixtures.recorded)
    val prefixes = records.indices.toVector.flatMap { k =>
      Flows.read(flow, history.id, records.take(k)).map(_.at) match {
        case Right(Flows.Read.Next(step, at)) if step == named(records, k) && at == k => None
        case other => Some(s"at $k, before ${records(k).name}: $other")
      }
    }
    val whole = Flows.read(flow, history.id, records).map(_.at) match {
      case Right(Flows.Read.Ended(_, steps)) if !unended && steps == records.size => None
      case Right(Flows.Read.Next(_, at)) if unended && at == records.size => None
      case other => Some(s"whole: $other")
    }
    prefixes ++ whole
  }

  val tests = Tests {
    test(
      "every run history of the current epoch names its next record at every prefix, inside its moves too, and as a whole has ended or stands where it stopped"
    ) {
      val runs = histories.collect {
        case (path, Right(h)) if h.workflow == "run" => (path.last, h)
      }
      val read = histories.flatMap {
        case (path, Left(why)) => Vector(s"${path.last}: $why")
        case (path, Right(h)) if h.workflow == "run" =>
          misread(h, path.last == Unended).map(m => s"${path.last}: $m")
        case _ => Vector()
      }
      // Without moves cut inside, and a wait among them, the gate would not reach the planner.
      val inside = runs.flatMap((_, h) => h.steps.map(_.name)).filter(_.startsWith("move:"))
      (runs.size, inside.exists(_.endsWith(":expire")), read) ==> (19, true, Vector())
    }
  }
}
