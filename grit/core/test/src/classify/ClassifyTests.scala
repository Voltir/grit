package grit.core.classify

import grit.core.message.{Tokens, Usage}

import utest.*

object ClassifyTests extends TestSuite {

  private enum Team { case Billing, Technical, Sales }

  /** A support ticket, sent as `{"ticket": text}`. */
  private final case class Ticket(text: String)
  private given StateJson[Ticket] = StateJson.instance(t => ujson.Obj("ticket" -> t.text))

  private def department(sales: String = "sales") = Ask.choice[Ticket, Team](
    "Which team should handle `ticket`?",
    Criterion(Team.Billing, "billing", Some("Payments, invoicing, refunds")),
    Criterion(Team.Technical, "technical", Some("Bugs, outages, integrations")),
    Criterion(Team.Sales, sales, None)
  )

  private val urgent = Ask.yesNo[Ticket]("Is `ticket` urgent?", None, None)

  private val free = Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0)))

  /** Answers every request with `answers`, whatever it asked, and keeps the last state. */
  private final class Canned(answers: Answer*) extends Classifier {
    @caps.unsafe.untrackedCaptures
    var state: ujson.Value = ujson.Null

    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] = {
      this.state = state
      Right(Answers(answers.toVector, free, "canned"))
    }
  }

  private def w(key: String, p: Double) = Answer.Weight(key, p)

  private def unreadable[T](r: Either[ClassifierError, T]): Boolean = r match {
    case Left(ClassifierError.Unreadable(_)) => true
    case _ => false
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

    test(
      "Answer.choice normalises, picks the most probable (the first on a tie), refuses no mass"
    ) {
      val a = Answer.choice(Vector(w("x", 1.0), w("y", 3.0)))
      assert(a.map(_.choice) == Some("y"))
      assert(a.map(_.probabilities) == Some(Vector(w("x", 0.25), w("y", 0.75))))
      assert(
        Answer.choice(Vector(w("x", 2.0), w("y", 2.0), w("z", Double.NaN))).map(_.choice) == Some(
          "x"
        )
      )
      assert(Answer.choice(Vector(w("x", 0.0), w("y", -1.0))).isEmpty)
      assert(Answer.choice(Vector()).isEmpty)
    }

    test("the state goes as its StateJson makes it") {
      val canned = new Canned(Answer.YesNo(0.3))
      canned.ask(Ticket("Help!"), urgent).map(_.value) ==> Right(0.3)
      canned.state ==> ujson.Obj("ticket" -> "Help!")
    }

    test("a choice is read back into its values, in the question's order") {
      val canned = new Canned(
        Answer.Choice(
          "billing",
          Vector(w("sales", 0.0), w("technical", 0.12), w("billing", 0.88)),
          0.81
        )
      )
      val d =
        department().flatMap(q => canned.ask(Ticket("Help!"), q).left.map(_.toString)).map(_.value)
      assert(d.map(_.choice) == Right(Team.Billing))
      assert(
        d.map(_.probabilities) == Right(
          Vector(
            Decision.Weight(Team.Billing, 0.88),
            Decision.Weight(Team.Technical, 0.12),
            Decision.Weight(Team.Sales, 0.0)
          )
        )
      )
      assert(d.map(_.top) == Right(0.88))
    }

    test("zip asks both questions in order and reads each answer by position") {
      val both = department().map(_.zip(urgent))
      assert(both.map(_.questions.size) == Right(2))
      val canned =
        new Canned(Answer.Choice("sales", Vector(w("sales", 1.0)), 1.0), Answer.YesNo(0.95))
      val read = both.flatMap(q => canned.ask(Ticket("x"), q).left.map(_.toString)).map(_.value)
      assert(read.map((d, u) => (d.choice, u)) == Right((Team.Sales, 0.95)))
    }

    test("asking a question twice is two questions") {
      val canned = new Canned(Answer.YesNo(0.1), Answer.YesNo(0.9))
      canned.ask(Ticket("x"), urgent.zip(urgent)).map(_.value) ==> Right((0.1, 0.9))
    }

    test("an answer outside the options, of the wrong kind, or missing is Unreadable") {
      val outside = new Canned(Answer.Choice("hr", Vector(w("hr", 1.0)), 1.0))
      val wrongKind = new Canned(Answer.YesNo(0.5))
      val missing = new Canned()
      val extra = new Canned(Answer.YesNo(0.5), Answer.YesNo(0.5))
      department().foreach(q =>
        Vector(outside, wrongKind, missing).foreach(c => assert(unreadable(c.ask(Ticket("x"), q))))
      )
      assert(unreadable(extra.ask(Ticket("x"), urgent)))
    }

    test("a choice whose keys repeat is not built") {
      department(sales = "billing").map(_ => ()) ==> Left(Ask.DuplicateKey("billing"))
    }

    test("a decision's probability of a value sums over the criteria that stand for it") {
      val d = Decision(
        1,
        Vector(Decision.Weight(1, 0.25), Decision.Weight(2, 0.5), Decision.Weight(1, 0.25)),
        0.0
      )
      d.probability(1) ==> 0.5
      d.probability(3) ==> 0.0
    }

    test("Classifier.none answers nothing, Unavailable with its reason") {
      assert(Classifier.none("no key").ask(Ticket("x"), urgent) match {
        case Left(ClassifierError.Unavailable("no key")) => true
        case _ => false
      })
    }
  }
}
