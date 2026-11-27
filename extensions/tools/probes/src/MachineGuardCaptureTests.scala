package grit.tools

import grit.core.durable.Probes

import utest.*

/** What [[MachineGuard]]'s constructor refuses: the stash it reads before a `git stash` that
  * takes an entry off is read through a capability of its own, [[grit.core.host.Stashes]], so
  * neither the model's runs nor a whole `Processes` can stand in for it.
  */
object MachineGuardCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.host.{AbsPath, Processes, Reach, Runs, Stashes}
      |import grit.tools.MachineGuard
      |""".stripMargin

  private def guarded(stash: String): String =
    s"def guarded(rc: Reach^, s: $stash^): MachineGuard^ = new MachineGuard(rc, s, Left(\"none\"), AbsPath.Root, None, Vector.empty)"

  private val itself = guarded("Stashes")
  private val overRuns = guarded("Runs")
  private val overProcesses = guarded("Processes")
  private lazy val compiled = Probes.batch(prelude, List(itself, overRuns, overProcesses))

  val tests = Tests {
    test("control: a guard over its own stash capability compiles") {
      compiled.errors(itself) ==> Nil
    }

    test("a guard cannot read the stash through the model's runs, or through a whole Processes") {
      assert(
        compiled.errors(overRuns).exists(_.contains("Required: grit.core.host.Stashes^")),
        compiled.errors(overProcesses).exists(_.contains("Required: grit.core.host.Stashes^"))
      )
    }
  }
}
