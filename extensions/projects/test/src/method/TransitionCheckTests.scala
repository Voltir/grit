package grit.projects.method

import grit.projects.method.MethodFixtures.*
import grit.projects.method.TransitionChecks.*
import grit.projects.name.CheckName

import utest.*

object TransitionCheckTests extends TestSuite {

  private val kinds: Vector[ArtifactKind] = Vector(change, note)
  private val parent = artifact(head("p/epic", change, "building"), "The epic.")

  val tests = Tests {
    test("children-ended names each child not in an end state of its kind") {
      val heads = Vector(
        parent.head,
        head("p/a", change, "done", parent = Some("p/epic")),
        head("p/b", change, "building", parent = Some("p/epic")),
        head("p/c", note, "open"),
        head("p/d", note, "closed", parent = Some("p/epic"))
      )
      assert(
        ChildrenEnded.holds(parent, heads, kinds) == Left("not ended yet: p/b (building)"),
        ChildrenEnded.holds(parent, heads.filterNot(_.key == key("p/b")), kinds) == Right(())
      )
    }
    test("cited-accepted holds once an artifact citing it is accepted") {
      val citing = head("p/review", note, "open", cites = Vector("p/epic"))
      val elsewhere = head("p/other", note, "accepted", cites = Vector("p/a"))
      assert(
        CitedAccepted.holds(parent, Vector(parent.head, citing, elsewhere), kinds) ==
          Left("nothing citing p/epic is accepted"),
        CitedAccepted.holds(
          parent,
          Vector(parent.head, citing.copy(state = state("accepted"))),
          kinds
        ) == Right(())
      )
    }
    test("sections-present names each section its kind lists that its body lacks") {
      val kinds = Vector(change.copy(sections = Vector("Why", "How")))
      val why = artifact(
        head("p/x", change, "building"),
        "## Why\n\nBecause.\n\n### How\n\nNested, not a section."
      )
      val both = artifact(head("p/x", change, "building"), "## Why\n\nBecause.\n\n## How\n\nSo.")
      assert(
        SectionsPresent.holds(why, Vector(why.head), kinds) == Left("its body lacks ## How"),
        SectionsPresent.holds(both, Vector(both.head), kinds) == Right(())
      )
    }
    test("every built-in's name is one CheckName.of accepts") {
      val names = TransitionChecks.all.map(c => CheckName.value(c.name))
      assert(names.map(CheckName.of(_).map(CheckName.value)) == names.map(Right(_)))
    }
  }
}
