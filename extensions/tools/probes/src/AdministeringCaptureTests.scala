package grit.tools

import grit.core.durable.Probes

import utest.*

/** `label_room` and `quiet_room` capture the administration they act through, and their type
  * says so: no pure tool list, such as a plugin's, can hold them unseen.
  */
object AdministeringCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.admin.Administration
      |import grit.core.clock.Clock
      |import grit.core.tool.Tool
      |import grit.tools.Administering
      |""".stripMargin

  private val held =
    """def held(admin: Administration^, clock: Clock^): Vector[Tool.Offered^{admin, clock}] =
      |    Administering.tools(admin, clock)""".stripMargin

  private val unseen =
    """def unseen(admin: Administration^, clock: Clock^): Vector[Tool.Offered] =
      |    Administering.tools(admin, clock)""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(held, unseen))

  val tests = Tests {
    test("one compile answers every probe") {
      assert(compiled.runs == 1)
    }

    test("control: the tools at a type naming what they capture compile") {
      compiled.errors(held) ==> Nil
    }

    test("the tools at a pure tool list's type are rejected, naming the administration") {
      val errs = compiled.errors(unseen)
      assert(errs.exists(e => e.contains("admin") && e.contains("cannot flow into capture set")))
    }
  }
}
