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

  /** Answers every request with `answers`, whatever it asked, and keeps the last state and
    * questions.
    */
  private final class Canned(answers: Answer*) extends Classifier {
    @caps.unsafe.untrackedCaptures
    var state: ujson.Value = ujson.Null
    @caps.unsafe.untrackedCaptures
    var asked: Vector[Question] = Vector.empty

    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] = {
      this.state = state
      this.asked = questions
      Right(Answers(answers.toVector, free, "canned"))
    }
  }

  private def w(key: String, p: Double) = Answer.Weight(key, p)

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
      val canned =
        new Canned(Answer.Choice("sales", Vector(w("sales", 1.0)), 1.0), Answer.YesNo(0.95))
      val read = both.flatMap(q => canned.ask(Ticket("x"), q).left.map(_.toString)).map(_.value)
      assert(read.map((d, u) => (d.choice, u)) == Right((Team.Sales, 0.95)))
      val sent = canned.asked.map {
        case Question.Choice(instructions, _, _, _) => s"choice: $instructions"
        case Question.YesNo(instructions, _, _) => s"yes/no: $instructions"
      }
      sent ==> Vector("choice: Which team should handle `ticket`?", "yes/no: Is `ticket` urgent?")
    }

    test("asking a question twice is two questions") {
      val canned = new Canned(Answer.YesNo(0.1), Answer.YesNo(0.9))
      canned.ask(Ticket("x"), urgent.zip(urgent)).map(_.value) ==> Right((0.1, 0.9))
    }

    test("an answer outside the options, of the wrong kind, missing or extra is Unreadable") {
      def asked[T](q: Ask[Ticket, T], answers: Answer*) =
        new Canned(answers*).ask(Ticket("x"), q).map(_ => ())
      val toChoice = "\"Which team should handle `ticket`?\": "
      department().map(q =>
        Vector(
          asked(q, Answer.Choice("hr", Vector(w("hr", 1.0)), 1.0)),
          asked(q, Answer.YesNo(0.5)),
          asked(q)
        )
      ) ==> Right(
        Vector(
          Left(ClassifierError.Unreadable(toChoice + "chose hr, not an option")),
          Left(ClassifierError.Unreadable(toChoice + "answered yes/no to a choice")),
          Left(ClassifierError.Unreadable("0 answers to 1 questions"))
        )
      )
      asked(urgent, Answer.Choice("billing", Vector(w("billing", 1.0)), 1.0)) ==>
        Left(ClassifierError.Unreadable("\"Is `ticket` urgent?\": answered a choice to a yes/no"))
      asked(urgent, Answer.YesNo(0.5), Answer.YesNo(0.5)) ==>
        Left(ClassifierError.Unreadable("2 answers to 1 questions"))
    }

    test("a choice whose keys repeat is not built") {
      department(sales = "billing").map(_ => ()) ==> Left(Ask.DuplicateKey("billing"))
    }

    test("a decision's probability of a value sums over the criteria that stand for it") {
      val ones = Ask.choice[Ticket, Int](
        "?",
        Criterion(1, "a", None),
        Criterion(2, "b", None),
        Criterion(1, "c", None)
      )
      val canned =
        new Canned(Answer.Choice("b", Vector(w("a", 0.25), w("b", 0.5), w("c", 0.25)), 0.0))
      val d = ones.flatMap(q => canned.ask(Ticket("x"), q).left.map(_.toString)).map(_.value)
      assert(d.map(_.choice) == Right(2), d.map(_.probability(1)) == Right(0.5))
      assert(d.map(_.probability(3)) == Right(0.0))
    }

    test("a decision's probabilities sum to 1 and its choice is the most probable, whatever came") {
      def decide(choice: String, ps: Answer.Weight*) =
        department()
          .flatMap(q =>
            new Canned(Answer.Choice(choice, ps.toVector, 0.5))
              .ask(Ticket("x"), q)
              .left
              .map(_.productPrefix)
          )
          .map(_.value)
      val answers = Vector(
        "missing a key" -> decide("billing", w("billing", 1.5), w("technical", 0.5)),
        "mass off the options, choosing less" -> decide(
          "billing",
          w("sales", 1.5),
          w("hr", 3.0),
          w("billing", 0.5)
        ),
        "NaN, negative, infinite" ->
          decide(
            "billing",
            w("billing", Double.NaN),
            w("technical", -1.0),
            w("sales", Double.PositiveInfinity)
          ),
        "NaN beside mass" -> decide("billing", w("billing", Double.NaN), w("technical", 0.4)),
        "a tie" -> decide("sales", w("billing", 0.4), w("technical", 0.2), w("sales", 0.4))
      )
      val read =
        answers.map((why, d) => why -> d.map(d => (d.choice, d.probabilities.map(_.probability))))
      assert(
        read == Vector(
          "missing a key" -> Right((Team.Billing, Vector(0.75, 0.25, 0.0))),
          "mass off the options, choosing less" -> Right((Team.Sales, Vector(0.25, 0.0, 0.75))),
          "NaN, negative, infinite" -> Left("Unreadable"),
          "NaN beside mass" -> Right((Team.Technical, Vector(0.0, 1.0, 0.0))),
          "a tie" -> Right((Team.Billing, Vector(0.4, 0.2, 0.4)))
        )
      )
    }

    test("a decision is built only by reading an answer") {
      import scala.compiletime.testing.typeChecks
      assert(
        typeChecks("Decision.Weight(1, 0.5)"),
        !typeChecks("Decision(1, Vector(Decision.Weight(1, 5.0)), 0.0)"),
        typeChecks("(d: Decision[Int]) => d.choice"),
        !typeChecks("(d: Decision[Int]) => d.copy(choice = 2)")
      )
    }

    test("Classifier.none answers nothing, Unavailable with its reason") {
      assert(Classifier.none("no key").ask(Ticket("x"), urgent) match {
        case Left(ClassifierError.Unavailable("no key")) => true
        case _ => false
      })
    }
  }
}
