package grit.projects.name

import utest.*

object NameTests extends TestSuite {
  private def local(s: String): Handle =
    Handle.of(s).fold(why => throw new java.lang.AssertionError(why), identity)

  val tests = Tests {
    test(
      "nearest puts keys extending the asked one first, then by edit distance, at most three, none past a third of its length"
    ) {
      val among = Vector(
        "plan-c5-runs-turns",
        "plan-c5b",
        "plan-c4",
        "plan-c7",
        "plan-c5",
        "note-c5",
        "unrelated"
      ).map(local)
      // plan-c5b extends it by one, plan-c5-runs-turns by eleven; plan-c4 and plan-c7 are one
      // edit away and tie, by key, so the cut at three leaves plan-c7.
      val nearest = Handle.nearest(local("plan-c5"), among)
      // plan-c47 is two edits from plan-c5, within 7 / 3 = 2; note-c5 is four.
      val within = Handle.nearest(local("plan-c5"), Vector("note-c5", "plan-c47").map(local))
      (nearest, within, Handle.nearest(local("z"), Vector("a", "b").map(local))) ==> (
        Vector("plan-c5b", "plan-c5-runs-turns", "plan-c4").map(local),
        Vector(local("plan-c47")),
        Vector.empty
      )
    }
    test("an id reads back as written, naming its project and its number") {
      val read = ArtifactKey.read("grit/412")
      assert(
        read.map(_.value) == Right("grit/412"),
        read.map(k => (ProjectKey.value(k.project), ArtifactNumber.value(k.number))) ==
          Right(("grit", 412))
      )
    }
    test("a reference led by a digit is an id, any other a handle, and either refused says why") {
      val grit = ProjectKey.of("grit").fold(sys.error, identity)
      assert(
        ArtifactRef.read("grit/412") ==
          Right(
            ArtifactRef.Id(ArtifactKey.of(grit, ArtifactNumber.of(412).fold(sys.error, identity)))
          ),
        ArtifactRef.read("grit/release-notes") ==
          Right(ArtifactRef.Named(grit, local("release-notes"))),
        ArtifactRef.read("grit/4x") == Left("an artifact's number is digits alone: 4x"),
        ArtifactRef.read("grit/0") == Left("an artifact's number is below 1: 0"),
        ArtifactRef.read("grit") == Left("an artifact's key has no '/': grit")
      )
    }
    test("a name holding an uppercase letter is refused, naming the letter") {
      assert(
        KindName.of("Change") == Left(
          "a kind's name holds 'C', not a lowercase letter, a digit or '-': Change"
        )
      )
    }
    test(
      "a handle is led by a letter; empty, too long, or from '-' or a digit is refused, saying which"
    ) {
      assert(
        Handle.of("2nd-try") == Left("a handle starts with a digit: 2nd-try"),
        Handle.of("") == Left("a handle is empty"),
        Handle.of("-a") == Left("a handle starts with '-': -a"),
        Handle.of("a" * 81) == Left(s"a handle is over 80 characters: ${"a" * 81}"),
        ProjectKey.of("a" * 41) ==
          Left(s"a project's key is over 40 characters: ${"a" * 41}")
      )
    }
    test("ArtifactRef.id gives an id's key and refuses a handle, naming it") {
      (
        ArtifactRef.read("p/4").flatMap(ArtifactRef.id).map(_.value),
        ArtifactRef.read("p/notes").flatMap(ArtifactRef.id)
      ) ==> (
        Right("p/4"),
        Left("p/notes names no id: only its project tells which artifact it is")
      )
    }
    test("a pin under version 1 is refused") {
      val pin = MethodName.of("grit").flatMap(Pin.read(_, 0, "ab" * 32))
      assert(pin == Left("a pinned version is under 1: 0"))
    }
  }
}
