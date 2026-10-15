package grit.core.act

import grit.core.durable.Probes

import utest.*

/** What the compiler refuses as a [[MoveName]] literal: one that is empty, over 200 characters,
  * or holds `:` or a control character, and a name that is not a literal, pinned by compiling
  * probe sources against core.
  */
object MoveNameLiteralTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.act.MoveName
      |""".stripMargin

  private val literals =
    s"""val short: MoveName = MoveName("triage")
       |val longest: MoveName = MoveName("${"a" * 200}")
       |""".stripMargin

  private val empty = """val n: MoveName = MoveName("")"""

  private val long = s"""val n: MoveName = MoveName("${"a" * 201}")"""

  private val colon = """val n: MoveName = MoveName("a:record")"""

  private val control = """val n: MoveName = MoveName("a\tb")"""

  private val built = """def n(s: String): MoveName = MoveName(s)"""

  private lazy val compiled =
    Probes.batch(prelude, List(literals, empty, long, colon, control, built))

  private def errors(body: String): List[String] = compiled.errors(body)

  private val Refused =
    "a move's name is 1 to 200 characters, none of them ':' or a control character"

  val tests = Tests {
    test("control: a literal of 1 to 200 characters, none ':' or a control, compiles") {
      errors(literals) ==> Nil
    }

    test("a literal empty, over 200 characters, or holding ':' or a control is refused") {
      List(empty, long, colon, control).map(errors) ==> List.fill(4)(List(Refused))
    }

    test("a name built at run time is refused") {
      errors(built) ==> List("a move's name is a literal; MoveName.of reads one made at run time")
    }
  }
}
