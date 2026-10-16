package grit.core.visibility

import grit.core.durable.Probes

import utest.*

/** What no label placed is marked unmapped, and a deployment cannot hand that mark to a person
  * or declare a room at it: a grant and a declared room's label take a label of declared
  * compartments alone, and no compartment is the mark. Typer errors, compiled against core in a
  * probe package of its own.
  */
object UnmappedMarkTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.place.Place
      |import grit.core.visibility.*
      |""".stripMargin

  private val group = """GroupName.of("staff").getOrElse(???)"""

  private val declared =
    s"""val grant = Grant($group, Label.at(Level.Internal))
       |  val rooms = RoomLabels.of(Vector(Place.Everywhere -> Label.at(Level.Internal)), Labelled.Mapped(Label.Public))""".stripMargin
  private val grantTop = s"val grant = Grant($group, Compartments.Shipped.top)"
  private val roomAtTop =
    """val rooms = RoomLabels.of(Vector(Place.Everywhere -> Compartments.Shipped.top), Labelled.Mapped(Label.Public))"""
  private val stewardOfMark = s"val steward = Steward(Compartment.Unmapped, $group)"

  private lazy val compiled =
    Probes.batch(prelude, List(declared, grantTop, roomAtTop, stewardOfMark))

  val tests = Tests {
    test("one compile answers the batch") {
      compiled.runs ==> 2
    }

    test("control: a grant and a room declared at a label made of declared compartments compile") {
      compiled.errors(declared) ==> Nil
    }

    test("a grant of a label that may hold the mark, grit's own top, does not compile") {
      val errs = compiled.errors(grantTop)
      assert(
        errs.exists(e =>
          e.contains("Found:") && e.contains("Required: grit.core.visibility.DeclaredLabel")
        )
      )
    }

    test("a room declared at a label that may hold the mark does not compile") {
      val errs = compiled.errors(roomAtTop)
      assert(errs.exists(_.contains("grit.core.visibility.DeclaredLabel")))
    }

    test("no compartment is the mark, so no one is made its steward") {
      val errs = compiled.errors(stewardOfMark)
      assert(
        errs.exists(
          _.contains("value Unmapped is not a member of object grit.core.visibility.Compartment")
        )
      )
    }
  }
}
