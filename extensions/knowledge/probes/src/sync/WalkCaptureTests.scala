package grit.knowledge.sync

import grit.core.durable.Probes
import grit.core.durable.Probes.{classpath, erased, flowsInto, heldImpure, options}

import utest.*

/** What capture checking rejects about the walk: it is data, so it holds no move, and neither
  * the planner nor a source's agenda can make one. Pinned by compiling probe sources against knowledge with its own
  * flags; `assertCompileError` cannot see capture errors (docs/capture-checking.md).
  */
object WalkCaptureTests extends TestSuite {

  private val prelude =
    """package grit.knowledge.probe
      |import grit.core.act.*
      |import grit.core.visibility.Label
      |import grit.knowledge.drive.{DriveCards, DriveListing, DriveWalk}
      |import grit.knowledge.github.{GitHubCards, Listing, RepositoryWalk}
      |import grit.knowledge.sync.*
      |""".stripMargin

  /** An agenda whose `take` makes the moves of the run it plans. */
  private val agendaTakesMoves =
    """def agenda(cards: GitHubCards, moves: Keeping^): Walking.Agenda[Listing, Way, Way] =
      |  Walking.Agenda(cards, (_, w) => { val m = moves; w }, (_, _) => false, _ => Vector.empty)
      |""".stripMargin

  /** A walk of `Walk`'s shape, declared as `Walk` is, holding the moves of the run it plans. */
  private val walkHoldsMoves =
    """final class Walk private (moves: Keeping^, held: Vector[Held]) extends caps.Pure {
      |  def next: Either[Synced, Step] = Right(Step.Drop(held))
      |}
      |""".stripMargin

  private val walkIsPure =
    """def pure[A <: caps.Pure](a: A): A = a
      |def walk(cards: GitHubCards, floor: Label): Walk[Listing] =
      |  pure(RepositoryWalk.start(cards, 1, floor, Vector.empty))
      |def drive(cards: DriveCards, floor: Label): Walk[DriveListing] =
      |  pure(DriveWalk.start(cards, 1, floor, Vector.empty, 0L))
      |""".stripMargin

  private lazy val compiled =
    Probes.batch(prelude, List(walkIsPure, walkHoldsMoves, agendaTakesMoves))

  private val unchecked = options.filterNot(_.startsWith("-language:experimental."))

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains("-language:experimental.captureChecking"))
    }

    test("one compile answers every probe: the walks' purity, held moves, an agenda's moves") {
      compiled.runs ==> 1
    }

    test("the walk is pure data: a bound asking for caps.Pure takes either source's") {
      assert(compiled.errors(walkIsPure).isEmpty)
    }

    test("a walk holding its run's moves is rejected") {
      assert(heldImpure(compiled.errors(walkHoldsMoves)))
    }

    test("capture checking is what rejects it") {
      assert(Probes.errors(erased(prelude + walkHoldsMoves), unchecked).isEmpty)
    }

    test("an agenda whose take makes its run's moves is rejected") {
      val errs = compiled.errors(agendaTakesMoves)
      assert(flowsInto("{}")(errs))
    }

    test("capture checking is what rejects the agenda") {
      assert(Probes.errors(erased(prelude + agendaTakesMoves), unchecked).isEmpty)
    }
  }
}
