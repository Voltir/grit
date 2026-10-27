package grit.projects.method

import grit.projects.artifact.{Artifact, ArtifactHead}
import grit.projects.method.MethodFixtures.*
import grit.projects.method.MethodRefusal.*
import grit.projects.name.CheckName

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
      assert(forward == reversed, more.isRight, forward != more, forward.map(_.length) == Right(64))
    }
    test("a check's version bump changes the digest") {
      val at1 = Method.of(method, 1, kinds, transitions, views, Vector(sectionsAt(1))).map(_.digest)
      val at2 = Method.of(method, 1, kinds, transitions, views, Vector(sectionsAt(2))).map(_.digest)
      assert(at1.isRight, at1 != at2)
    }
  }
}
