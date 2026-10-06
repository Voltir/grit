package grit.job.replay

import grit.core.durable.{History, InMemoryDurable}
import grit.job.run.Run
import grit.job.run.RunFixtures.*
import grit.turn.Turn

import utest.*

/** The versioning gate for the run workflow (ADR 0004): every run history recorded under the
  * engine's current epoch, [[Turn.Epoch]], must replay under today's body. A step renamed,
  * reordered, dropped or given an output the old records cannot satisfy fails here, before it
  * strands a run in flight.
  */
object JobReplayTests extends TestSuite {

  private def histories: Vector[(os.Path, Either[String, History])] = {
    val dir = os.Path(sys.env("GRIT_HISTORIES")) / Turn.Epoch
    if (!os.isDir(dir)) Vector.empty
    else
      os.list(dir)
        .filter(_.ext == "json")
        .toVector
        .map(p => p -> History.read(ujson.read(os.read(p))))
  }

  val tests = Tests {
    test("the current epoch has run histories to replay, a redeployed one's among them") {
      // Without them the gate below passes vacuously.
      val runs = histories.collect { case (path, Right(h)) if h.workflow == "run" => path.last }
      assert(runs.size >= 6, runs.contains("run-replied-then-redeployed.json"))
    }

    test("every run history of the current epoch replays under today's body, at a later version") {
      // An empty world: every recorded step reads its output back, and a later deploy's jobs
      // decide nothing a recorded step already did.
      val failures = histories.flatMap { case (path, parsed) =>
        val outcome = parsed.flatMap { history =>
          if (history.epoch != Turn.Epoch) Left(s"recorded under epoch ${history.epoch}")
          else if (history.workflow != "run") Left(s"a ${history.workflow} history")
          else {
            val w = new World
            new InMemoryDurable().replay(history.id, history.steps)(Run.body(w.env(), jobs(2)))
          }
        }
        outcome.left.toOption.map(why => s"${path.last}: $why")
      }
      assert(failures.isEmpty)
    }
  }
}
