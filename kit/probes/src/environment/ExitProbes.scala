package grit.kit.environment

import grit.core.durable.Probes

import utest.*

/** That an [[Exit]] is a tracked capability: code that may end the program says so in its type,
  * and a function typed pure cannot hold one.
  */
object ExitProbes extends TestSuite {

  private val prelude =
    """package outside
      |import grit.kit.environment.Exit
      |""".stripMargin

  private val control = "def f(e: Exit): Int = e(Right(1))\n"

  // `Exit`, not `Exit^`: the trait itself must be tracked, whatever a signature writes.
  private val pureHolding = "def f(e: Exit): () -> Int = () => e(Right(1))\n"

  private lazy val compiled = Probes.batch(prelude, List(control, pureHolding))

  val tests = Tests {
    test("control: code holding an exit refuses through it") {
      compiled.errors(control) ==> Nil
    }

    test("a pure function holding an exit is rejected") {
      assert(compiled.errors(pureHolding).nonEmpty)
    }
  }
}
