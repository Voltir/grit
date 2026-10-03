package grit.core.id

import utest.*

object QuestionNameTests extends TestSuite {

  private def name(s: String): QuestionName =
    QuestionName.of(s).getOrElse(throw new java.lang.AssertionError(s))

  private def source(s: String): KnowledgeSourceName =
    KnowledgeSourceName.of(s).getOrElse(throw new java.lang.AssertionError(s))

  val tests = Tests {
    test(
      "a declared question's name is a lowercase letter, then lowercase letters, digits or dashes"
    ) {
      List("gap", "to", "to-2", "a1")
        .map(QuestionName.of(_).isRight) ==> List(true, true, true, true)
      // A colon is the per-source form's alone, so no declared name can be one.
      List("", "Gap", "2a", "-a", "a_b", "a b", "source:github")
        .map(QuestionName.of(_).isRight) ==> List(false, false, false, false, false, false, false)
      QuestionName.of("source:github") ==>
        Left(
          "a question's name is a lowercase letter, then lowercase letters, digits or -: source:github"
        )
    }

    test("a per-source name is <prefix>:<source>, and read takes it and a declared name back") {
      val github = QuestionName.per(name("source"), source("github"))
      // A pin of a recorded name: a set's answers are stored under it.
      QuestionName.value(github) ==> "source:github"
      QuestionName.read("source:github") ==> Right(github)
      QuestionName.read("gap") ==> Right(name("gap"))
      // A per-source name used as a prefix reads back too.
      val nested = QuestionName.per(github, source("issues_2"))
      QuestionName.read(QuestionName.value(nested)) ==> Right(nested)
      List("Source:github", "source:Github", "source:git-hub", "source:", ":github")
        .map(QuestionName.read(_).isRight) ==> List(false, false, false, false, false)
    }

    test("a knowledge source's name is a lowercase letter, then lowercase letters, digits or _") {
      List("github", "conversations", "a_1").map(KnowledgeSourceName.of(_).isRight) ==>
        List(true, true, true)
      List("", "GitHub", "_a", "1a", "a-b", "a:b").map(KnowledgeSourceName.of(_).isRight) ==>
        List(false, false, false, false, false, false)
      KnowledgeSourceName.of("a-b") ==>
        Left(
          "a knowledge source's name is a lowercase letter, then lowercase letters, digits or _: a-b"
        )
    }
  }
}
