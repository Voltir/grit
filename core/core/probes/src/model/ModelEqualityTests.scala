package grit.core.model

import grit.core.durable.Probes

import utest.*

/** A model type is compared only with its own kind: each derives a reflexive `CanEqual`, so
  * `==` between two unrelated ones is a typer error without global `strictEquality`.
  */
object ModelEqualityTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.model.*
      |import grit.core.visibility.*
      |""".stripMargin

  private val same =
    """def same(a: Assignment, l: Ladder, p: Pinned, ps: Pins, x: Party, c: Classification): Boolean =
      |    a == l.first.assignment && ps.first == p && l.first.grade == p.grade &&
      |      x == x && c == c && Option(p) == ps.all.headOption""".stripMargin
  private val assignmentLadder = "def f(a: Assignment, l: Ladder): Boolean = a == l"
  private val pinnedPins = "def g(p: Pinned, ps: Pins): Boolean = p == ps"
  private val gradeRung = "def h(g: Grade, r: Rung): Boolean = r != g"
  private val partyCleared = "def k(p: Party, c: Cleared): Boolean = c == p"

  private lazy val compiled =
    Probes.batch(prelude, List(same, assignmentLadder, pinnedPins, gradeRung, partyCleared))

  private def probe(body: String): List[String] = compiled.errors(body)

  private def uncomparable(left: String, right: String)(errs: List[String]): Boolean =
    errs.size == 1 && errs.forall(e =>
      e.contains(
        s"Values of types grit.core.model.$left and grit.core.model.$right cannot be compared"
      ) || e.contains(s"Values of types $left and $right cannot be compared")
    )

  val tests = Tests {
    test("two compiles answer every probe: the typer's refusals, then the control") {
      compiled.runs ==> 2
    }

    test("control: each model type is compared with its own kind") {
      probe(same) ==> Nil
    }

    test("an assignment is never compared with a ladder") {
      val errs = probe(assignmentLadder)
      assert(uncomparable("Assignment", "Ladder")(errs))
    }

    test("a pin is never compared with a role's pins") {
      val errs = probe(pinnedPins)
      assert(uncomparable("Pins", "Pinned")(errs) || uncomparable("Pinned", "Pins")(errs))
    }

    test("a grade is never compared with a rung") {
      val errs = probe(gradeRung)
      assert(uncomparable("Rung", "Grade")(errs))
    }

    test("a party is never compared with its clearance") {
      val errs = probe(partyCleared)
      assert(
        errs.size == 1,
        errs.forall(
          _.contains(
            "Values of types grit.core.visibility.Cleared and grit.core.visibility.Party cannot be compared"
          )
        )
      )
    }
  }
}
