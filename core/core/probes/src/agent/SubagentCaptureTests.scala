package grit.core.agent

import grit.core.durable.Probes

import utest.*

/** What the compiler holds of a deployment's subagents: they are pure data, so a value or step
  * that must hold no capability may hold them. Nothing outside their files can build one holding
  * a capability (their constructors are private, `Tools` is an enum, and every field's type is
  * pure), so the breach has no probe of its own.
  */
object SubagentCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.agent.*
      |""".stripMargin

  private val pure =
    """def pure[A <: caps.Pure](a: A): A = a
      |def kept(s: Subagent, t: Subagent.Tools, all: Subagents): (Subagent, Subagent.Tools, Subagents) =
      |  (pure(s), pure(t), pure(all))
      |""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(pure))

  val tests = Tests {
    test(
      "a subagent, its tools and a deployment's subagents are pure: a caps.Pure bound takes each"
    ) {
      compiled.errors(pure) ==> Nil
    }
  }
}
