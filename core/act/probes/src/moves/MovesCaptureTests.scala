package grit.act.moves

import grit.core.durable.Probes
import grit.core.durable.Probes.classpath

import utest.*

/** What capture checking rejects about [[DurableMoves]] (ADR 0034): what a run's body returns,
  * through `plain` or `keeping`, cannot carry its moves out, and the body makes no step of its
  * own beside them, so the run's next step may use the `Durable` they were made over. Pinned
  * by compiling probe sources against act with act's own flags; `assertCompileError` cannot see
  * capture errors (docs/capture-checking.md).
  */
object MovesCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.act.moves.*
      |import grit.core.act.*
      |import grit.core.document.DocumentKeeper
      |import grit.core.durable.Durable
      |final case class Reply(text: String) extends caps.Pure
      |""".stripMargin

  /** `body` as a run's: over an acting, its limits, an environment and a keeper, in a workflow. */
  private def run(body: String): String =
    "def run(acting: Acting, limits: MoveLimits, env: MovesEnv^, keeper: DocumentKeeper)" +
      s"(using d: Durable^): Reply = {\n$body\n}"

  /** The error messages from compiling `body` as a run's, with every probe below. */
  private def errors(body: String): List[String] = compiled.errors(run(body))

  private val bound = "does not conform to upper bound scala.caps.Pure"

  private val runs =
    """val plain = DurableMoves.plain(acting, limits, env)(moves => Reply(moves.toString))
      |val kept = DurableMoves.keeping(acting, limits, env, keeper)(moves => Reply(moves.toString))
      |Reply(d.step("reply") { () => plain.text + kept.text })""".stripMargin

  private val plainReturnsMoves =
    """val _ = DurableMoves.plain[Moves^](acting, limits, env)(moves => moves)
      |Reply("")""".stripMargin

  private val keepingReturnsMoves =
    """val _ = DurableMoves.keeping[Keeping^](acting, limits, env, keeper)(moves => moves)
      |Reply("")""".stripMargin

  private val plainSteps =
    """DurableMoves.plain(acting, limits, env)(moves => {
      |  val seen = d.step("peek") { () => "x" }
      |  Reply(moves.toString + seen)
      |})""".stripMargin

  private val keepingSteps =
    """DurableMoves.keeping(acting, limits, env, keeper)(moves => {
      |  val seen = d.step("peek") { () => "x" }
      |  Reply(moves.toString + seen)
      |})""".stripMargin

  /* The bounds' refusals are the typer's, so a second compile answers the rest. */
  private lazy val compiled = Probes.batch(
    prelude,
    List(runs, plainReturnsMoves, keepingReturnsMoves, plainSteps, keepingSteps).map(run)
  )

  val tests = Tests {
    test("two compiles answer every probe: the typer's refusals of the bounds, then the rest") {
      compiled.runs ==> 2
    }

    test("a run's body, plain or keeping, then the run's next step, compiles") {
      val errs = errors(runs)
      assert(classpath.nonEmpty, errs.isEmpty)
    }

    test("a plain run's body that returns its moves is rejected by its bound") {
      val errs = errors(plainReturnsMoves)
      assert(errs.contains(s"Type argument grit.core.act.Moves^ $bound"))
    }

    test("a keeping run's body that returns its moves is rejected by its bound") {
      val errs = errors(keepingReturnsMoves)
      assert(errs.contains(s"Type argument grit.core.act.Keeping^ $bound"))
    }

    test("a plain run's body that makes a step while its moves are live is rejected") {
      val errs = errors(plainSteps)
      assert(errs.exists(_.startsWith("Separation failure")))
    }

    test("a keeping run's body that makes a step while its moves are live is rejected") {
      val errs = errors(keepingSteps)
      assert(errs.exists(_.startsWith("Separation failure")))
    }
  }
}
