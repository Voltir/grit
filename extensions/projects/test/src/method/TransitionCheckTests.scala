package grit.projects.method

import grit.projects.artifact.{Artifact, ArtifactHead, Rank}
import grit.projects.method.MethodFixtures.*
import grit.projects.method.TransitionChecks.*
import grit.projects.name.CheckName

import utest.*

object TransitionCheckTests extends TestSuite {

  private val kinds: Vector[ArtifactKind] = Vector(change, note)
  private val parent = artifact(head("p/epic", change, "building"), "The epic.")

  /** A change ranked `m` under the epic, and its project's heads: before it, a building and a
    * proposed change and a done one under the epic; after it, a proposed one; ranked before it, a
    * note under the epic, a change at the root and one under another parent.
    */
  private object siblings {
    private def at(h: ArtifactHead, r: String) = h.copy(rank = right(Rank.of(r)))
    val judged: Artifact =
      artifact(at(head("p/j", change, "proposed", parent = Some("p/epic")), "m"), "J.")
    val heads: Vector[ArtifactHead] = Vector(
      parent.head,
      judged.head,
      at(head("p/a", change, "building", parent = Some("p/epic")), "d"),
      at(head("p/b", change, "proposed", parent = Some("p/epic")), "c"),
      at(head("p/c", change, "done", parent = Some("p/epic")), "b"),
      at(head("p/d", change, "proposed", parent = Some("p/epic")), "x"),
      at(head("p/e", note, "open", parent = Some("p/epic")), "b"),
      at(head("p/f", change, "proposed"), "b"),
      at(head("p/g", change, "proposed", parent = Some("p/other")), "b")
    )
  }

  val tests = Tests {
    test(
      "predecessor-released names each sibling of its kind ranked before it and still holding it, in rank order"
    ) {
      val unended = Set(key("p/a"), key("p/b"))
      assert(
        PredecessorReleased.holds(siblings.judged, siblings.heads, kinds) == Left(
          "listed before it and still holding it: p/2 b (proposed), p/1 a (building)"
        ),
        PredecessorReleased.holds(
          siblings.judged,
          siblings.heads.filterNot(h => unended(h.key)),
          kinds
        ) == Right(())
      )
    }
    test("predecessor-released passes a sibling in a state its kind releases") {
      val releasing = Vector(change.copy(releases = Set(state("building"))), note)
      PredecessorReleased.holds(siblings.judged, siblings.heads, releasing) ==>
        Left("listed before it and still holding it: p/2 b (proposed)")
    }
    test("children-ended names each child not in an end state of its kind") {
      val heads = Vector(
        parent.head,
        head("p/a", change, "done", parent = Some("p/epic")),
        head("p/b", change, "building", parent = Some("p/epic")),
        head("p/c", note, "open"),
        head("p/d", note, "closed", parent = Some("p/epic")),
        head("p/e", note, "open", parent = Some("p/epic"))
      )
      val unended = Set(key("p/b"), key("p/e"))
      assert(
        ChildrenEnded.holds(parent, heads, kinds) == Left(
          "not ended yet: p/2 b (building), p/5 e (open)"
        ),
        ChildrenEnded.holds(parent, heads.filterNot(h => unended(h.key)), kinds) == Right(())
      )
    }
    test("cited-accepted holds once an artifact citing it is accepted") {
      val citing = head("p/review", note, "open", cites = Vector("p/epic"))
      val elsewhere = head("p/other", note, "accepted", cites = Vector("p/a"))
      assert(
        CitedAccepted.holds(parent, Vector(parent.head, citing, elsewhere), kinds) ==
          Left("nothing citing p/21 epic is accepted"),
        CitedAccepted.holds(
          parent,
          Vector(parent.head, citing.copy(state = state("accepted"))),
          kinds
        ) == Right(())
      )
    }
    test("sections-present names each section its kind lists that its body lacks") {
      val kinds = Vector(change.copy(sections = Vector("Why", "How", "Risks")))
      val why = artifact(
        head("p/x", change, "building"),
        "## Why\n\nBecause.\n\n### How\n\nNested, not a section."
      )
      val all = artifact(
        head("p/x", change, "building"),
        "## Why\n\nBecause.\n\n## How\n\nSo.\n\n## Risks\n\nNone."
      )
      assert(
        SectionsPresent.holds(why, Vector(why.head), kinds) == Left(
          "its body lacks ## How, ## Risks"
        ),
        SectionsPresent.holds(all, Vector(all.head), kinds) == Right(())
      )
    }
    test("every built-in's name is one CheckName.of accepts") {
      val names = TransitionChecks.all.map(c => CheckName.value(c.name))
      assert(names.map(CheckName.of(_).map(CheckName.value)) == names.map(Right(_)))
    }
  }
}
