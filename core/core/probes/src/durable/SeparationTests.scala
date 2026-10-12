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
      |""".stripMargin

  private val nested =
    """def f(p: Provider^)(using d: Durable^): String =
      |  d.step("outer") { () => d.step("inner") { () => p.call() } }
      |""".stripMargin

  private val sequenced =
    """def f(p: Provider^)(using d: Durable^): String = {
      |  val a = d.step("a") { () => p.call() }
      |  val b = d.transact("b", Subject.Public) { p.call() }
      |  a + b
      |}
      |""".stripMargin

  private val boundFirst =
    """def f(p: Provider^)(using d: Durable^): String = {
      |  val inner = () => d.step("inner") { () => p.call() }
      |  d.step("outer")(inner)
      |}
      |""".stripMargin

  private val inTransaction =
    """def f(p: Provider^)(using d: Durable^): String =
      |  d.transact("outer", Subject.Public) { d.step("inner") { () => p.call() } }
      |""".stripMargin

  /** Every probe above, compiled in one run with core's flags. */
  private lazy val compiled =
    Probes.batch(prelude, List(sequenced, nested, boundFirst, inTransaction))

  private def errors(body: String): List[String] = compiled.errors(body)

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains("-language:experimental.separationChecking"))
    }

    test("one compile answers every probe") {
      assert(compiled.runs == 1)
    }

    test("a step body may capture other capabilities, in sequence and in a transaction") {
      assert(errors(sequenced).isEmpty)
    }

    test("a step inside a step is rejected") {
      val errs = errors(nested)
      assert(errs.exists(_.contains("Separation failure")))
    }

    test("a step in a closure bound first, then run as a step, is rejected") {
      assert(errors(boundFirst).exists(_.contains("Separation failure")))
    }

    test("a step inside a transaction is rejected") {
      assert(errors(inTransaction).exists(_.contains("Separation failure")))
    }

    test("separation checking is what rejects the nested step") {
      val flags = options.filterNot(_ == "-language:experimental.separationChecking")
      assert(Probes.batch(prelude, List(nested), flags).errors(nested).isEmpty)
    }
  }
}
