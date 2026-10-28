package grit.projects.name

import utest.*

object NameTests extends TestSuite {

  val tests = Tests {
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
    test("a slot is parts joined by `/`, at most 160 characters") {
      val long = Vector.fill(3)("a" * 60).mkString("/")
      assert(
        BriefSlot.of("coder-opus/abc").map(BriefSlot.value) == Right("coder-opus/abc"),
        BriefSlot.of("") == Left("a part of a slot is empty"),
        BriefSlot.of("a//b") == Left("a part of a slot is empty"),
        BriefSlot.of("Coder/abc") == Left(
          "a part of a slot holds 'C', not a lowercase letter, a digit or '-': Coder"
        ),
        BriefSlot.of(long) == Left(s"a slot holds at most 160 characters: $long")
      )
    }
    test("a pin under version 1 is refused") {
      val pin = MethodName.of("grit").flatMap(Pin.read(_, 0, "ab" * 32))
      assert(pin == Left("a pinned version is under 1: 0"))
    }
  }
}
