package grit.knowledge.sync

import grit.core.durable.Probes
import grit.core.durable.Probes.{classpath, erased, heldImpure, options}

import utest.*

/** What capture checking rejects about the walk: it is data, so it holds no move, and the
  * planner cannot make one. Pinned by compiling probe sources against knowledge with its own
  * flags; `assertCompileError` cannot see capture errors (docs/capture-checking.md).
  */
object WalkCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.act.*
      |import grit.core.visibility.Label
      |import grit.knowledge.github.GitHubCards
      |import grit.knowledge.sync.*
      |""".stripMargin

  private def errors(body: String, flags: List[String] = options): List[String] =
    Probes.errors(prelude + body + "\n", flags)

  /** A walk of `Walk`'s shape, declared as `Walk` is, holding the moves of the run it plans. */
  private val walkHoldsMoves =
    """final class Walk private (moves: Keeping^, held: Vector[Held]) extends caps.Pure {
      |  def next: Either[Synced, Step] = Right(Step.Drop(held))
      |}
      |""".stripMargin

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains("-language:experimental.captureChecking"))
    }

    test("the walk is pure data: a bound asking for caps.Pure takes it") {
      val errs = errors(
        """def pure[A <: caps.Pure](a: A): A = a
          |def walk(cards: GitHubCards, floor: Label): Walk =
          |  pure(Walk.start(cards, 1, floor, Vector.empty))
          |""".stripMargin
      )
      assert(errs.isEmpty)
    }

    test("a walk holding its run's moves is rejected") {
      assert(heldImpure(errors(walkHoldsMoves)))
    }

    test("capture checking is what rejects it") {
      val flags = options.filterNot(_.startsWith("-language:experimental."))
      assert(Probes.errors(erased(prelude + walkHoldsMoves), flags).isEmpty)
    }
  }
}
