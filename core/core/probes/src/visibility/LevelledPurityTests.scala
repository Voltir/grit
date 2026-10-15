package grit.core.visibility

import grit.core.durable.Probes

import utest.*

/** A label or an integrity captures nothing: the one representation both are over takes only a
  * pure chain of levels, so not even this package can build one over a level type that might
  * hold a capability. Compiled inside the package, where the representation can be named.
  */
object LevelledPurityTests extends TestSuite {

  private val prelude =
    """package grit.core.visibility
      |enum Plain extends caps.Pure { case Low, High }
      |enum Impure { case Low, High }
      |""".stripMargin

  private def over(level: String): String =
    s"""def levels: Levels[$level] = new Levels[$level] {
       |    val all: Vector[$level] = $level.values.toVector
       |    def name(l: $level): String = l.toString
       |    def shownAs: String = "x"
       |    def refusedAs: String = "x"
       |  }
       |  val made = Levelled.at($level.Low, Nil)(using levels)""".stripMargin

  private val pure = over("Plain")
  private val impure = over("Impure")

  private lazy val compiled = Probes.batch(prelude, List(pure, impure))

  private def probe(body: String): List[String] = compiled.errors(body)

  val tests = Tests {
    test("control: the representation is built over a pure chain of levels") {
      probe(pure) ==> Nil
    }

    test("a chain of levels that is not pure is refused at the representation's bound") {
      probe(impure).exists(e => e.contains("Impure") && e.contains("caps.Pure")) ==> true
    }
  }
}
