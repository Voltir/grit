package grit.projects.method

import grit.projects.artifact.{Artifact, ArtifactHead}
import grit.projects.method.MethodFixtures.*
import grit.projects.method.MethodRefusal.*
import grit.projects.name.{CheckName, LocalKey}

import utest.*

object MethodTests extends TestSuite {

  private val kinds: Vector[ArtifactKind] = Vector(change, note)

  /** `sections-present` as compiled at `v`, deciding nothing. */
  private def sectionsAt(v: Int): TransitionCheck = new TransitionCheck {
    val name: CheckName = check("sections-present")
    val version: Int = v
    def holds(
        judged: Artifact,
        heads: Vector[ArtifactHead],
        kinds: Vector[ArtifactKind]
    ): Either[String, Unit] =
      Right(())
  }

  val tests = Tests {
    test("a transition naming an undeclared state is refused, naming it") {
      val shipped = transitions :+ move(change, "building", "shipped")
      val made = Method.of(method, 1, kinds, shipped, views, TransitionChecks.all)
      assert(made == Left(Vector(UndeclaredState(change.name, state("shipped")))))
    }
    test("a check not compiled in is refused") {
      val made = Method.of(method, 1, kinds, transitions, views, Vector.empty)
      assert(made == Left(Vector(NotCompiled(check("sections-present")))))
    }
    test("a transition merging deltas into an undeclared kind is refused, naming it") {
      val merging = Transition(
        change.name,
        state("building"),
        state("done"),
        Vector.empty,
        TransitionAction.MergeDeltas(kind("spec"))
      )
      val made =
        Method.of(method, 1, kinds, transitions.updated(1, merging), views, TransitionChecks.all)
      assert(made == Left(Vector(UndeclaredKind(kind("spec")))))
    }
    test("an unreachable state is refused") {
      val made = Method.of(method, 1, kinds, transitions.drop(1), views, TransitionChecks.all)
      assert(
        made == Left(
          Vector(
            Unreachable(change.name, state("building")),
            Unreachable(change.name, state("done"))
          )
        )
      )
    }
    test("every refusal is reported, not the first") {
      val made = Method.of(method, 0, kinds :+ note, transitions, views, Vector.empty)
      assert(
        made == Left(
          Vector(VersionUnderOne(0), KindTwice(note.name), NotCompiled(check("sections-present")))
        )
      )
    }
    test("a method's checks are the compiled ones its transitions name") {
      val made = Method.of(method, 1, kinds, transitions, views, TransitionChecks.all)
      assert(made.map(_.checks) == Right(Vector(TransitionChecks.SectionsPresent)))
    }
    test("digest ignores declared order") {
      val forward =
        Method.of(method, 1, kinds, transitions, views, TransitionChecks.all).map(_.digest)
      val reversed = Method
        .of(method, 1, kinds.reverse, transitions.reverse, views, TransitionChecks.all.reverse)
        .map(_.digest)
      val more = Method
        .of(
          method,
          1,
          kinds,
          transitions :+ move(change, "proposed", "done"),
          views,
          TransitionChecks.all
        )
        .map(_.digest)
      assert(forward == reversed, more.isRight, forward != more)
    }
    test("a check's version bump changes the digest") {
      val at1 = Method.of(method, 1, kinds, transitions, views, Vector(sectionsAt(1))).map(_.digest)
      val at2 = Method.of(method, 1, kinds, transitions, views, Vector(sectionsAt(2))).map(_.digest)
      assert(at1.isRight, at1 != at2)
    }
    test("a role view's change changes the digest, and reordering views does not") {
      val reviewer = right(RoleView.of(role("reviewer"), Set(note.name), 2))
      def digest(vs: Vector[RoleView]) =
        Method.of(method, 1, kinds, transitions, vs, TransitionChecks.all).map(_.digest)
      val deeper = right(RoleView.of(role("reviewer"), Set(note.name), 3))
      val carrying =
        right(RoleView.of(role("reviewer"), Set(note.name), 2, Set(right(LocalKey.of("rules")))))
      assert(
        digest(views :+ reviewer) == digest(reviewer +: views),
        digest(views :+ reviewer).isRight,
        digest(views :+ reviewer) != digest(views :+ deeper),
        digest(views :+ reviewer) != digest(views :+ carrying)
      )
    }
    test("a method's digest is pinned: a change to it refuses every pinned project's moves") {
      // Every project records this digest in its pin; a digest that drifts makes each one stale.
      val digest =
        Method.of(method, 1, kinds, transitions, views, TransitionChecks.all).map(_.digest.hex)
      assert(digest == Right("f49dd63d8bcf94b8f37cf4b2ed0768c1945cc7381a6f6fe43523eb29c597c1c8"))
    }

    test("views carrying nothing digest as before views could carry, so earlier pins hold") {
      // Pinned as they were before a view could carry artifacts: OpenSpec's and the
      // elephant-goldfish model's views, of depths 2, 1 and 3.
      (
        grit.projects.paper.OpenSpecTests.openSpec.digest.hex,
        grit.projects.paper.ElephantGoldfishTests.model.digest.hex
      ) ==> ((
        "a6c2646b7b67f379df318cb6a030fb1712cdde4ad95ca2757141e0f50f73584c",
        "bd9d10b09c491a675b18f9fce800c02c80c5eb69e196a048a1e97182704e1d14"
      ))
    }
  }
}
