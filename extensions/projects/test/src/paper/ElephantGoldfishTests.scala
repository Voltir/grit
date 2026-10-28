package grit.projects.paper

import grit.projects.method.Example.{Expect, Move}
import grit.projects.method.MethodFixtures.*
import grit.projects.method.{
  ArtifactKind,
  Example,
  Method,
  MethodBench,
  RoleView,
  Transition,
  TransitionAction,
  TransitionChecks
}
import grit.projects.name.MethodName
import grit.projects.render.Brief

import utest.*

/** The elephant-goldfish model (an elephant, rich in context, drafts; goldfish, given only the
  * draft and what it cites, review it; a person decides) as method data: the paper check that
  * the method language can say it.
  */
object ElephantGoldfishTests extends TestSuite {

  private val source =
    ArtifactKind(kind("source"), state("current"), Vector.empty, Set.empty, Vector.empty)
  private val draft = ArtifactKind(
    kind("draft"),
    state("drafting"),
    Vector(state("reviewing"), state("adopted"), state("dropped")),
    Set(state("adopted"), state("dropped")),
    Vector.empty
  )
  private val review =
    ArtifactKind(
      kind("review"),
      state("open"),
      Vector(state("given")),
      Set(state("given")),
      Vector("Findings")
    )
  private val decision = ArtifactKind(
    kind("decision"),
    state("proposed"),
    Vector(state("accepted"), state("rejected")),
    Set(state("accepted"), state("rejected")),
    Vector.empty
  )

  private def step(k: ArtifactKind, from: String, to: String, checks: String*): Transition =
    Transition(k.name, state(from), state(to), checks.toVector.map(check), TransitionAction.Nothing)

  private val elephant = right(
    RoleView.of(role("elephant"), Set(source.name, draft.name, review.name, decision.name), 3)
  )
  private val goldfish = right(RoleView.of(role("goldfish"), Set(source.name, draft.name), 1))

  private val model: Method = right(
    Method
      .of(
        right(MethodName.of("elephant-goldfish")),
        1,
        Vector(source, draft, review, decision),
        Vector(
          step(draft, "drafting", "reviewing"),
          step(draft, "reviewing", "adopted", "children-ended", "cited-accepted"),
          step(draft, "reviewing", "dropped"),
          step(review, "open", "given", "sections-present"),
          step(decision, "proposed", "accepted"),
          step(decision, "proposed", "rejected")
        ),
        Vector(elephant, goldfish),
        TransitionChecks.all
      )
      .left
      .map(_.mkString("; "))
  )

  private val far = artifact(head("p/far", source, "current"), "What the source rests on.")
  private val near =
    artifact(head("p/near", source, "current", cites = Vector("p/far")), "The source.")
  private val theDraft =
    artifact(head("p/plan", draft, "drafting", cites = Vector("p/near")), "We should do it.")
  private val theReview =
    artifact(head("p/look", review, "open", parent = Some("p/plan")), "## Findings\n\nOne gap.")
  private val theDecision =
    artifact(head("p/call", decision, "proposed", cites = Vector("p/plan")), "Do it.")
  private val start = project(far, near, theDraft, theReview, theDecision)

  private def to(k: String, s: String, expect: Expect = Expect.Allowed) =
    Move(key(k), state(s), expect)

  private val examples: Vector[Example] = Vector(
    Example(
      "a draft is adopted once its reviews are given and a person accepts it",
      start,
      Vector(
        to("p/plan", "reviewing"),
        to("p/plan", "adopted", Expect.RefusedBy(check("children-ended"))),
        to("p/look", "given"),
        to("p/plan", "adopted", Expect.RefusedBy(check("cited-accepted"))),
        to("p/call", "accepted"),
        to("p/plan", "adopted")
      )
    )
  )

  val tests = Tests {
    test("the elephant-goldfish model passes Method.of and its examples pass the bench") {
      assert(MethodBench.failures(model, examples) == Vector.empty)
    }
    test("a goldfish is shown the draft and what it cites, one link deep; an elephant more") {
      val shown = (v: RoleView) =>
        right(Brief.of(start, key("p/plan"), v, Brief.HookChars).left.map(_.toString)).text
      val fish = shown(goldfish)
      val big = shown(elephant)
      assert(
        fish.contains("We should do it."),
        fish.contains("### p/near"),
        !fish.contains("p/far"),
        big.contains("### p/far")
      )
    }
  }
}
