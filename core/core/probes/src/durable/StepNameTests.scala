package grit.core.durable

import utest.*
import Probes.classpath

/** What the compiler refuses as a [[StepName]]: a name the runtime keeps for itself, fixed or as
  * a family, and a name that is not a literal, pinned by compiling probe sources against core.
  */
object StepNameTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.durable.*
      |""".stripMargin

  private val literals =
    """val fixed: StepName[Any] = StepName("settle")
      |val family: StepName[Int] = StepName.keyed[Int]("tool")(_.toString)
      |""".stripMargin

  private val reservedFixed = """val n: StepName[Any] = StepName("DBOS.recv")"""

  private val reservedFamily =
    """val n: StepName[Int] = StepName.keyed[Int]("DBOS.patch-x")(_.toString)"""

  private val builtFixed = """def n(s: String): StepName[Any] = StepName(s)"""

  private val builtFamily =
    """def n(s: String): StepName[Int] = StepName.keyed[Int](s)(_.toString)"""

  private lazy val compiled =
    Probes.batch(prelude, List(literals, reservedFixed, reservedFamily, builtFixed, builtFamily))

  private def errors(body: String): List[String] = compiled.errors(body)

  val tests = Tests {
    test("control: a literal name and a literal family compile") {
      assert(classpath.nonEmpty)
      errors(literals) ==> Nil
    }

    test("a literal the runtime keeps for itself is refused, fixed or as a family") {
      val refused = List(reservedFixed, reservedFamily).map(errors)
      refused ==> List(
        List("'DBOS.recv' is a name the runtime keeps for itself"),
        List("'DBOS.patch-x' is a name the runtime keeps for itself")
      )
    }

    test("a name built at run time is refused, fixed or as a family") {
      val refused = List(builtFixed, builtFamily).map(errors)
      refused ==> List(List("a step name is a literal"), List("a step name is a literal"))
    }
  }
}
