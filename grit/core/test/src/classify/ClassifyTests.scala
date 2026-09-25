package grit.core.classify

import grit.core.message.{Tokens, Usage}

import utest.*

object ClassifyTests extends TestSuite {

  private enum Team { case Billing, Technical, Sales }

  private val department = Ask.choice(
    QuestionId("department"),
    "Which team should handle this?",
    Vector(
      Criterion(Team.Billing, "billing", Some("Payments, invoicing, refunds")),
      Criterion(Team.Technical, "technical", Some("Bugs, outages, integrations")),
      Criterion(Team.Sales, "sales", None)
    )
  )

  private val urgent = Ask.noul(QuestionId("urgent"), "Is this urgent?", None, None)

  private val free = Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0)))

  /** Answers every request with `answers`, whatever it asked. */
  private final class Canned(answers: Map[QuestionId, Answer]) extends Classifier {
    def answer(
        state: ujson.Value,
        questions: Vector[(QuestionId, Question)]
    ): Either[ClassifierError, Answers] = Right(Answers(answers, free, "canned"))
  }

  val tests = Tests {
    test("confidence: (n·max − 1)/(n − 1) matches every example in Jev's docs, to their rounding") {
      // (probabilities, the confidence Jev reported), from api.md, primitives_choice.md and
      // the live call the main session made on 2026-09-24.
      val documented = Vector(
        Vector(0.88, 0.12, 0.0) -> 0.81,
        Vector(0.61, 0.35, 0.04) -> 0.42,
        Vector(0.74, 0.26, 0.0, 0.0, 0.0) -> 0.67,
        Vector(0.4, 0.34, 0.02, 0.24) -> 0.2,
        Vector(0.84, 0.16, 0.0) -> 0.76,
        Vector(1.0, 0.0, 0.0) -> 1.0,
        Vector(0.76, 0.24, 0.0, 0.0, 0.0) -> 0.69
      )
      documented.foreach { (ps, reported) =>
        val mine = Answer.confidence(ps)
        // Both figures are printed to two places, so up to 0.015 apart for n = 3.
        assert(math.abs(mine - reported) <= 0.015)
      }
    }

    test("Answer.choice normalises, picks the most probable, and refuses no mass") {
      val a = Answer.choice(Vector("x" -> 1.0, "y" -> 3.0))
      assert(a.map(_.choice) == Some("y"))
      assert(a.map(_.probabilities) == Some(Vector("x" -> 0.25, "y" -> 0.75)))
      assert(Answer.choice(Vector("x" -> 0.0, "y" -> 0.0)).isEmpty)
      assert(Answer.choice(Vector()).isEmpty)
    }

    test("a choice is read back into its values, in the question's order") {
      val canned = new Canned(
        Map(
          QuestionId("department") -> Answer.Choice(
            "billing",
            Vector("sales" -> 0.0, "technical" -> 0.12, "billing" -> 0.88),
            0.81
          )
        )
      )
      val d = canned.ask(ujson.Str("Help!"), department).map(_.value)
      assert(d.map(_.choice) == Right(Team.Billing))
      assert(
        d.map(_.probabilities) == Right(
          Vector(Team.Billing -> 0.88, Team.Technical -> 0.12, Team.Sales -> 0.0)
        )
      )
      assert(d.map(_.top) == Right(0.88))
    }

    test("zip asks both questions and reads both answers") {
      val both = department.zip(urgent)
      assert(both.questions.map(_._1) == Vector(QuestionId("department"), QuestionId("urgent")))
      val canned = new Canned(
        Map(
          QuestionId("department") -> Answer.Choice("sales", Vector("sales" -> 1.0), 1.0),
          QuestionId("urgent") -> Answer.Noul(0.95)
        )
      )
      val read = canned.ask(ujson.Str("x"), both).map(_.value)
      assert(read.map((d, u) => (d.choice, u)) == Right((Team.Sales, 0.95)))
    }

    test("an answer outside the options, or of the wrong kind, is Unreadable") {
      val outside = new Canned(
        Map(QuestionId("department") -> Answer.Choice("hr", Vector("hr" -> 1.0), 1.0))
      )
      val wrongKind = new Canned(Map(QuestionId("department") -> Answer.Noul(0.5)))
      val missing = new Canned(Map())
      Vector(outside, wrongKind, missing).foreach { c =>
        assert(c.ask(ujson.Str("x"), department) match {
          case Left(ClassifierError.Unreadable(_)) => true
          case _ => false
        })
      }
    }

    test("a repeated id, or a choice with too few or repeated keys, is Invalid before asking") {
      val canned = new Canned(Map())
      val one = Ask.choice(QuestionId("q"), "?", Vector(Criterion(1, "a", None)))
      val twice =
        Ask.choice(QuestionId("q"), "?", Vector(Criterion(1, "a", None), Criterion(2, "a", None)))
      Vector(urgent.zip(urgent).map(_ => ()), one.map(_ => ()), twice.map(_ => ())).foreach { ask =>
        assert(canned.ask(ujson.Str("x"), ask) match {
          case Left(ClassifierError.Invalid(_)) => true
          case _ => false
        })
      }
    }

    test("Classifier.none answers nothing, Unavailable with its reason") {
      assert(Classifier.none("no key").ask(ujson.Str("x"), urgent) match {
        case Left(ClassifierError.Unavailable("no key")) => true
        case _ => false
      })
    }
  }
}
