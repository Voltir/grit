package grit.models

import grit.core.classify.{Answer, Ask, ClassifierError, Criterion, Question}
import grit.core.message.Tokens

import utest.*

object JevJsonTests extends TestSuite {

  import JevFixtures.*

  val tests = Tests {
    test("request: the docs' score body, its levels an array in order") {
      JevJson.body(
        "jev-latest",
        ujson.Str("Help! My payouts have been failing for 3 days."),
        Vector(frustration)
      ) ==>
        """{"model":"jev-latest","state":"Help! My payouts have been failing for 3 days.","questions":{"q1":{"type":"score","instructions":"How frustrated is the customer?","criteria":["Calm","Frustrated","Very angry"]}}}"""
    }

    test("response: the docs' score answer, its weights in level order, the legend not read") {
      JevJson.response(Vector(frustration), ScoreBody).map(_.answers) ==>
        Right(Vector(Answer.Score(1.05, Vector(0.0, 0.95, 0.05), 0.92)))
    }

    test(
      "response: a score's missing level weighs 0, one past the last is not read; a missing score or weights is Unreadable"
    ) {
      def answer(raw: String) = JevJson.response(
        Vector(frustration),
        ujson.read(s"""{"model": "m", "answers": {"q1": $raw}}""")
      )
      answer("""{"type": "score", "score": 1.0, "probabilities": {"1": 1.0}, "confidence": 1.0}""")
        .map(_.answers) ==> Right(Vector(Answer.Score(1.0, Vector(0.0, 1.0, 0.0), 1.0)))
      // A weight on a level past the last is not read: the weights are the levels asked.
      answer(
        """{"type": "score", "score": 1.0, "probabilities": {"1": 0.5, "3": 0.5}, "confidence": 1.0}"""
      ).map(_.answers) ==> Right(Vector(Answer.Score(1.0, Vector(0.0, 0.5, 0.0), 1.0)))
      answer("""{"type": "score", "probabilities": {"1": 1.0}, "confidence": 1.0}""") ==>
        Left(ClassifierError.Unreadable("unreadable response: q1: no score"))
      answer("""{"type": "score", "score": 1.0, "confidence": 1.0}""") ==>
        Left(ClassifierError.Unreadable("unreadable response: q1: no probabilities"))
      answer("""{"type": "score", "score": 1.0, "probabilities": {"1": 1.0}}""") ==>
        Left(ClassifierError.Unreadable("unreadable response: q1: no confidence"))
    }

    test("request: the docs' example body, ids by position, a null criterion with no description") {
      val body = JevJson.request(
        "jev-latest",
        ujson.Str("Help! My payouts have been failing for 3 days."),
        department :+ urgent
      )
      body ==> ujson.read("""{
        "model": "jev-latest",
        "state": "Help! My payouts have been failing for 3 days.",
        "questions": {
          "q1": {
            "type": "choice",
            "instructions": "Which team should handle this?",
            "criteria": {
              "billing": "Payments, invoicing, refunds",
              "technical": "Bugs, outages, integrations",
              "sales": null
            }
          },
          "q2": {
            "type": "noul",
            "instructions": "Does this convey urgency?",
            "criteria": {"true": "Explicitly time-sensitive", "false": "No urgency expressed"}
          }
        }
      }""")
    }

    test("request: a yes/no with no criteria sends none") {
      val body = JevJson.request(
        "jev-latest",
        ujson.Obj("a" -> 1),
        Vector(Question.YesNo("Yes?", None, None))
      )
      assert(!body("questions")("q1").obj.contains("criteria"))
    }

    test("response: the docs' choice and noul answers, in the question's option order, priced") {
      val answers = JevJson.response(department :+ urgent, ChoiceBody)
      assert(answers.map(_.model) == Right("jev-1.13.0"))
      assert(
        answers.map(_.answers) == Right(
          Vector(
            Answer.Choice(
              "billing",
              Vector(
                Answer.Weight("billing", 0.88),
                Answer.Weight("technical", 0.12),
                Answer.Weight("sales", 0.0)
              ),
              0.81
            ),
            Answer.YesNo(0.95)
          )
        )
      )
      assert(answers.map(_.usage.input) == Right(Tokens(318)))
      assert(answers.map(_.usage.output) == Right(Tokens(34)))
      // 318 tokens at $0.042 per million
      assert(answers.map(_.usage.costUsd) == Right(Some(BigDecimal("0.000013356"))))
    }

    test("response: a missing answer, or a choice outside the options, is Unreadable") {
      val missing = ujson.read("""{"model": "m", "answers": {}, "usage": {}}""")
      val outside = ujson.read("""{"model": "m", "answers": {"q1":
        {"type": "choice", "choice": "hr", "probabilities": {"hr": 1.0}, "confidence": 1.0}}}""")
      Vector(missing, outside, ujson.Arr()).foreach { body =>
        assert(JevJson.response(department, body) match {
          case Left(ClassifierError.Unreadable(_)) => true
          case _ => false
        })
      }
    }

    test("error: the status and the body's detail") {
      JevJson.error(422, """{"detail": "questions.q.criteria: field required"}""") ==>
        ClassifierError.Unavailable("HTTP 422: questions.q.criteria: field required")
      JevJson.error(529, "overloaded") ==> ClassifierError.Unavailable("HTTP 529: overloaded")
    }

    test(
      "config: the key comes from JEV_API_KEY, a missing one names the variable, and toString hides it"
    ) {
      assert(JevConfig.fromEnv(Map()).left.map(_.message) == Left("JEV_API_KEY is not set"))
      assert(
        JevConfig.fromEnv(Map("JEV_API_KEY" -> " ")).left.map(_.message) == Left(
          "JEV_API_KEY is empty"
        )
      )
      val c = JevConfig.fromEnv(Map("JEV_API_KEY" -> "sk-secret"))
      assert(c.map(_.model) == Right("jev-latest"))
      assert(!c.map(_.toString).exists(_.contains("sk-secret")))
    }
  }
}
