package grit.models

import grit.core.classify.Question

import utest.*

/** [[AnswersSpec]] against [[StubClassifier]]'s answers and Jev's documented ones. */
object AnswersSpecTests extends TestSuite {
  import JevFixtures.*

  private val two = Question
    .choice("Which?", Question.Key("a", None), Question.Key("b", None))
    .fold(d => throw new java.lang.AssertionError(s"keys repeat: $d"), identity)

  private val questions: Vector[Question] = department ++ Vector(urgent, frustration, two)

  /** Each question's answer that breaks the spec, and how. */
  private def broken(questions: Vector[Question], answers: Vector[grit.core.classify.Answer]) =
    questions.zip(answers).flatMap((q, a) => AnswersSpec.broken(q, a).map(why => s"$q: $why"))

  val tests = Tests {
    test("the stub's answers keep the spec, marked or not") {
      val messages = Vector(
        "anything",
        "~0.1",
        "~back:technical",
        "~back:b",
        "~level:0",
        "~level:1",
        "~level:7",
        "~back:sales ~level:2 ~1"
      )
      messages.flatMap { m =>
        val answers = StubClassifier.answers(ujson.Obj("new_message" -> m), questions).answers
        (answers.size == questions.size, broken(questions, answers)) match {
          case (true, why) => why.map(w => s"$m: $w")
          case (false, _) => Vector(s"$m: ${answers.size} answers to ${questions.size}")
        }
      } ==> Vector()
    }

    test("Jev's documented answers, as read, keep the spec") {
      val read = for {
        score <- JevJson.response(Vector(frustration), ScoreBody)
        choice <- JevJson.response(department :+ urgent, ChoiceBody)
      } yield broken(Vector(frustration), score.answers) ++
        broken(department :+ urgent, choice.answers)
      read ==> Right(Vector())
    }
  }
}
