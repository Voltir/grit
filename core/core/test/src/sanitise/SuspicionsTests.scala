package grit.core.sanitise

import grit.core.classify.{Answer, Question, StateJson}
import grit.core.period.Probability

import utest.*

object SuspicionsTests extends TestSuite {

  val tests = Tests {
    test("the five answers read, in order, as directive, identity, act, genre and pressure") {
      val read = Suspicions.questions.read(
        Vector(0.1, 0.2, 0.3, 0.4, 0.5).map(Answer.YesNo(_))
      )
      read.map(s =>
        Vector(s.directive, s.identity, s.act, s.genre, s.pressure).map(Probability.value)
      ) ==> Right(Vector(0.1, 0.2, 0.3, 0.4, 0.5))
    }

    test("each question is a yes or no about text from source, after what about says") {
      Suspicions.questions.questions.map {
        case Question.YesNo(instructions, None, None) =>
          instructions.startsWith(
            "Following about, read text, which reached an AI assistant from source. "
          )
        case _ => false
      } ==> Vector.fill(5)(true)
    }

    test("the classifier is shown about, the kind of document, and the text as it came") {
      StateJson[Suspicions.State].json(Suspicions.State("file", "!> a\nb")) ==>
        ujson.Obj("about" -> Suspicions.About, "source" -> "file", "text" -> "!> a\nb")
    }
  }
}
