package grit.core.visibility

import grit.core.durable.Probes

import utest.*

/** A label outside this package cannot be taken apart: typer errors, compiled in a probe
  * package of its own against core.
  */
object LabelOpacityTests extends TestSuite {

  private def probe(body: String): List[String] =
    Probes.errors(
      s"""package outside
         |import grit.core.visibility.*
         |object Probe {
         |  val l: Label = Label.at(Level.Internal)
         |  $body
         |}
         |""".stripMargin
    )

  val tests = Tests {
    test("control: comparing, joining, meeting and writing a label compiles outside") {
      probe(
        "val ok = (l.dominates(Label.Public), l.join(l).meet(l) == l, Label.written(l))"
      ) ==> Nil
    }

    test("a label shows no level or compartments outside its package") {
      val errs = probe("val lv = l.level") ++ probe("val cs = l.compartments")
      assert(
        errs.exists(_.contains("value level is not a member of grit.core.visibility.Label")),
        errs.exists(_.contains("value compartments is not a member of grit.core.visibility.Label"))
      )
    }

    test("a label's representation cannot be named outside its package, to match on or build") {
      val errs = probe("def f(x: Label) = x match { case r: Label.Repr => r.level }") ++
        probe("val r = new Label.Repr(Level.Public, scala.collection.immutable.SortedSet.empty)")
      errs.count(_.contains("Repr cannot be accessed")) ==> 2
    }

    test("a label's compartments and level cannot be read outside its package") {
      val errs = probe("val cs = Label.compartments(l)") ++ probe("val lv = Label.level(l)")
      errs.count(_.contains("cannot be accessed")) ==> 2
    }
  }
}
