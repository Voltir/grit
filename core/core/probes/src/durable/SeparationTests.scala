package grit.core.durable

import utest.*
import Probes.{classpath, options}

/** What separation checking rejects about [[Durable]] (ADR 0003), pinned by compiling
  * probe sources against core with core's own flags. `assertCompileError` cannot do this: it
  * never reports capture- or separation-checking errors (docs/capture-checking.md).
  *
  * Expires: delete once separation checking is no longer experimental. Until then an
  * upgrade can change what it rejects silently; this suite is re-read at each Scala bump.
  */
object SeparationTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.durable.*
      |import grit.core.id.*
      |import grit.core.store.*
      |import grit.core.visibility.Subject
      |trait Provider extends caps.SharedCapability { def call(): String }
      |object Probe {
      |""".stripMargin

  /** The error messages from compiling `body` inside `object Probe`. */
  private def errors(body: String, flags: List[String] = options): List[String] =
    Probes.errors(prelude + body + "\n}\n", flags)

  private val nested =
    """def f(p: Provider^)(using d: Durable^): String =
      |  d.step("outer") { () => d.step("inner") { () => p.call() } }
      |""".stripMargin

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains("-language:experimental.separationChecking"))
    }

    test("a step body may capture other capabilities, in sequence and in a transaction") {
      val errs = errors(
        """def f(p: Provider^)(using d: Durable^): String = {
          |  val a = d.step("a") { () => p.call() }
          |  val b = d.transact("b", Subject.Public) { p.call() }
          |  a + b
          |}
          |""".stripMargin
      )
      assert(errs.isEmpty)
    }

    test("a step inside a step is rejected") {
      val errs = errors(nested)
      assert(errs.exists(_.contains("Separation failure")))
    }

    test("a step in a closure bound first, then run as a step, is rejected") {
      val errs = errors(
        """def f(p: Provider^)(using d: Durable^): String = {
          |  val inner = () => d.step("inner") { () => p.call() }
          |  d.step("outer")(inner)
          |}
          |""".stripMargin
      )
      assert(errs.exists(_.contains("Separation failure")))
    }

    test("a step inside a transaction is rejected") {
      val errs = errors(
        """def f(p: Provider^)(using d: Durable^): String =
          |  d.transact("outer", Subject.Public) { d.step("inner") { () => p.call() } }
          |""".stripMargin
      )
      assert(errs.exists(_.contains("Separation failure")))
    }

    test("separation checking is what rejects the nested step") {
      val errs = errors(nested, options.filterNot(_ == "-language:experimental.separationChecking"))
      assert(errs.isEmpty)
    }
  }
}
