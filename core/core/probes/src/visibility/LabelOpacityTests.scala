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
  private val matchRepr = opaque("def f(x: Label) = x match { case r: Label.Repr => r.level }")
  private val buildRepr =
    opaque("val r = new Label.Repr(Level.Public, scala.collection.immutable.SortedSet.empty)")
  private val readCompartments = opaque("val cs = Label.compartments(l)")
  private val readLevel = opaque("val lv = Label.level(l)")

  private lazy val compiled = Probes.batch(
    prelude,
    List(outside, level, compartments, matchRepr, buildRepr, readCompartments, readLevel)
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
      val errs = probe(matchRepr) ++ probe(buildRepr)
      errs.count(_.contains("Repr cannot be accessed")) ==> 2
    }

    test("a label's compartments and level cannot be read outside its package") {
      val errs = probe(readCompartments) ++ probe(readLevel)
      errs.count(_.contains("cannot be accessed")) ==> 2
    }
  }
}
