package grit.projects.name

import utest.*

object NameTests extends TestSuite {
  private def local(s: String): LocalKey =
    LocalKey.of(s).fold(why => throw new java.lang.AssertionError(why), identity)

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
      val nearest = LocalKey.nearest(local("plan-c5"), among)
      // plan-c47 is two edits from plan-c5, within 7 / 3 = 2; note-c5 is four.
      val within = LocalKey.nearest(local("plan-c5"), Vector("note-c5", "plan-c47").map(local))
      (nearest, within, LocalKey.nearest(local("z"), Vector("a", "b").map(local))) ==> (
        Vector("plan-c5b", "plan-c5-runs-turns", "plan-c4").map(local),
        Vector(local("plan-c47")),
        Vector.empty
      )
    }
    test("an artifact key reads back as written, naming its project") {
      val read = ArtifactKey.read("grit/add-search")
      assert(read.map(_.value) == Right("grit/add-search"))
      assert(read.map(k => ProjectKey.value(k.project)) == Right("grit"))
    }
    test("a name holding an uppercase letter is refused, naming the letter") {
      assert(
        KindName.of("Change") == Left(
          "a kind's name holds 'C', not a lowercase letter, a digit or '-': Change"
        )
      )
    }
    test(
      "a key part may start with a digit; empty, too long or from '-' is refused, saying which"
    ) {
      def key(local: String) = ProjectKey.of("p").flatMap(ArtifactKey.of(_, local)).map(_.value)
      assert(
        key("2nd-try") == Right("p/2nd-try"),
        key("") == Left("an artifact's part of its key is empty"),
        key("-a") == Left("an artifact's part of its key starts with '-': -a"),
        ProjectKey.of("a" * 41) ==
          Left(s"a project's key is over 40 characters: ${"a" * 41}")
      )
    }
    test("a pin under version 1 is refused") {
      val pin = MethodName.of("grit").flatMap(Pin.read(_, 0, "ab" * 32))
      assert(pin == Left("a pinned version is under 1: 0"))
    }
  }
}
