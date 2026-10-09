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

  /** Whether `history` made a move: a step of `grit.act.moves.MoveSteps`'. */
  private def moved(history: History): Boolean = history.steps.exists(_.name.startsWith("move:"))

  /** Each history `pick` takes that does not replay under `jobs`, and why. */
  private def failing(pick: History => Boolean, jobs: grit.core.job.Jobs): Vector[String] =
    histories.flatMap { case (path, parsed) =>
      val outcome = parsed.flatMap { history =>
        if (history.epoch != Turn.Epoch) Left(s"recorded under epoch ${history.epoch}")
        else if (history.workflow != "run") Left(s"a ${history.workflow} history")
        else if (!pick(history)) Right("not picked")
        else {
          val w = new World
          new InMemoryDurable().replay(history.id, history.steps)(Run.body(w.env(), jobs))
        }
      }
      outcome.left.toOption.map(why => s"${path.last}: $why")
    }

  val tests = Tests {
    test("the current epoch has run histories to replay, redeployed ones and moves among them") {
      // Without them the gates below pass vacuously.
      val runs = histories.collect { case (path, Right(h)) if h.workflow == "run" => path.last }
      val expected = Vector(
        "run-replied-then-redeployed",
        "run-asked",
        "run-called",
        "run-called-then-asked",
        "run-call-unserved",
        "run-call-unclaimed",
        "run-call-refused",
        "run-capped",
        "run-redeployed-before-moves",
        "run-moved-then-redeployed"
      ).map(_ + ".json")
      expected.filterNot(runs.contains) ==> Vector()
    }

    test("every run history that made no move replays under today's body, at a later version") {
      // An empty world: every recorded step reads its output back, and a later deploy's jobs
      // decide nothing a recorded step already did.
      failing(h => !moved(h), jobs(2)) ==> Vector()
    }

    test("every run history that made moves replays under today's body, at its own version") {
      failing(moved, jobsOf(probing(1))) ==> Vector()
    }

    test("a run that recorded no move, resumed at another version, recorded superseded") {
      val replayed = histories.collect {
        case (path, Right(h)) if path.last == "run-redeployed-before-moves.json" =>
          new InMemoryDurable().replay(h.id, h.steps)(Run.body(new World().env(), jobs(2)))
      }
      replayed ==> Vector(Right("superseded: started at v1, its job at v2"))
    }

    test(
      "a run that made a move, resumed at another version, ends in error at its reply, which meets the move"
    ) {
      val replayed = histories.collect {
        case (path, Right(h)) if path.last == "run-moved-then-redeployed.json" =>
          new InMemoryDurable().replay(h.id, h.steps)(Run.body(new World().env(), jobs(2)))
      }
      replayed.map(_.left.map(_.replaceAll("workflow \\S+ ", "workflow "))) ==>
        Vector(Left("workflow step 1: ran 'reply', recorded 'move:a'"))
    }
  }
}
