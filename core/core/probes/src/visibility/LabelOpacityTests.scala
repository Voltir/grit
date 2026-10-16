package grit.core.visibility

import grit.core.durable.Probes

import utest.*

/** A label outside this package cannot be taken apart: typer errors, compiled in a probe
  * package of its own against core.
  */
object LabelOpacityTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.visibility.*
      |""".stripMargin

  private def opaque(body: String): String = s"val l: Label = Label.at(Level.Internal)\n$body"

  private val outside =
    opaque("val ok = (l.dominates(Label.Public), l.join(l).meet(l) == l, Label.written(l))")
  private val level = opaque("val lv = l.level")
  private val compartments = opaque("val cs = l.compartments")
  private val matchRepr = opaque("def f(x: Label) = x match { case r: Levelled[?] => r.level }")
  private val buildRepr = opaque("val r = Levelled.at(Level.Public, Nil)")
  private val readCompartments = opaque("val cs = Label.compartments(l)")
  private val readLevel = opaque("val lv = Label.level(l)")
  private val integrityLevel = "val lv = Integrity.Member.level"
  private val matchIntegrity =
    "def f(x: Integrity) = x match { case r: Levelled[?] => r.compartments }"
  private val readIntegrityLevel = "val lv = Integrity.level(Integrity.Member)"

  private lazy val compiled = Probes.batch(
    prelude,
    List(
      outside,
      level,
      compartments,
      matchRepr,
      buildRepr,
      readCompartments,
      readLevel,
      integrityLevel,
      matchIntegrity,
      readIntegrityLevel
    )
  )

  private def probe(body: String): List[String] = compiled.errors(body)

  val tests = Tests {
    test("two compiles answer every probe: the typer's refusals, then the control") {
      compiled.runs ==> 2
    }

    test("control: comparing, joining, meeting and writing a label compiles outside") {
      probe(outside) ==> Nil
    }

    test("a label shows no level or compartments outside its package") {
      val errs = probe(level) ++ probe(compartments)
      assert(
        errs.exists(_.contains("value level is not a member of grit.core.visibility.Label")),
        errs.exists(_.contains("value compartments is not a member of grit.core.visibility.Label"))
      )
    }

    test("a label's representation cannot be named outside its package, to match on or build") {
      // Outside its package neither the representation's class nor its object is found: the
      // compiler, finding only the other one, names each as the wrong kind.
      assert(
        probe(matchRepr).exists(_.contains("Expected a type, but found a term: Levelled")),
        probe(buildRepr).exists(_.contains("Expected a term, but found a type: Levelled"))
      )
    }

    test("a label's compartments and level cannot be read outside its package") {
      assert(
        probe(readCompartments).exists(
          _.contains(
            "private[visibility] method compartments can only be accessed from package " +
              "grit.core.visibility in package grit.core."
          )
        ),
        probe(readLevel).exists(
          _.contains(
            "private[visibility] method level can only be accessed from package " +
              "grit.core.visibility in package grit.core."
          )
        )
      )
    }

    test("an integrity, over the same representation, is as opaque outside its package") {
      assert(
        probe(integrityLevel)
          .exists(_.contains("value level is not a member of grit.core.visibility.Integrity")),
        probe(matchIntegrity).exists(_.contains("Expected a type, but found a term: Levelled")),
        probe(readIntegrityLevel).exists(_.contains("level cannot be accessed"))
      )
    }
  }
}
