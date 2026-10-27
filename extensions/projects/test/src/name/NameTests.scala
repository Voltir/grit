package grit.projects.name

import utest.*

object NameTests extends TestSuite {

  val tests = Tests {
    test("an artifact key reads back as written, naming its project") {
      val read = ArtifactKey.read("grit/add-search")
      assert(read.map(ArtifactKey.value) == Right("grit/add-search"))
      assert(read.map(k => ProjectKey.value(ArtifactKey.project(k))) == Right("grit"))
    }
    test("a name holding an uppercase letter is refused, naming the letter") {
      assert(
        KindName.of("Change") == Left(
          "a kind's name holds 'C', not a lowercase letter, a digit or '-': Change"
        )
      )
    }
    test("a pin under version 1 is refused") {
      val pin = MethodName.of("grit").flatMap(Pin.of(_, 0, "ab" * 32))
      assert(pin == Left("a pinned version is under 1: 0"))
    }
  }
}
