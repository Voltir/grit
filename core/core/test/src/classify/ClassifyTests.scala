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

  private enum Mood { case Calm, Frustrated, Angry }

  private val frustration = Ask.score[Ticket, Mood](
    "How frustrated is `ticket`?",
    Level(Mood.Calm, "Calm"),
    Level(Mood.Frustrated, "Frustrated"),
    Level(Mood.Angry, "Very angry")
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

    test("scoreConfidence matches both Score examples in Jev's docs, to their rounding") {
      // (probabilities, the confidence Jev reported), from api.md and introduction_quickstart.md.
      val documented = Vector(Vector(0.0, 0.95, 0.05) -> 0.92, Vector(0.0, 1.0, 0.0) -> 1.0)
      documented.foreach { (ps, reported) =>
        // Both are printed to two places: 0.925 here, from probabilities that may be 0.005 off.
        assert(math.abs(Answer.scoreConfidence(ps) - reported) <= 0.01)
      }
    }

    test("scoreConfidence falls with the weight's distance from the likeliest level") {
      // Weight split between neighbours is surer than weight split between the ends, though
      // the peak is the same: Σ pᵢ·|i − m| over the same sum for even weights.
      Answer.scoreConfidence(Vector(0.5, 0.5, 0.0)) ==> 0.5
      Answer.scoreConfidence(Vector(0.5, 0.0, 0.5)) ==> 0.0
      // The likeliest level at an end has more room to spread: u is 1 there, 2/3 in the middle.
      Answer.scoreConfidence(Vector(0.8, 0.2, 0.0)) ==> 0.8
      assert(math.abs(Answer.scoreConfidence(Vector(0.2, 0.8, 0.0)) - 0.7) < 1e-9)
      Answer.scoreConfidence(Vector(1.0)) ==> 1.0
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
        case Question.Score(instructions, _, _, _) => s"score: $instructions"
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

    test("Ask.read is Unreadable for other than one answer per question, reading none") {
      (
        urgent.read(Vector.empty),
        urgent.read(Vector(Answer.YesNo(0.5), Answer.YesNo(0.5))),
        urgent.zip(urgent).read(Vector(Answer.YesNo(0.1)))
      ) ==> (
        Left(ClassifierError.Unreadable("0 answers to 1 questions")),
        Left(ClassifierError.Unreadable("2 answers to 1 questions")),
        Left(ClassifierError.Unreadable("1 answers to 2 questions"))
      )
    }

    test("Classifier.answers is Unreadable for a reply short, long or of another kind") {
      def answered(answers: Answer*) =
        new Canned(answers*).answers(Request.of(Ticket("x"), urgent)).map(_.answers)
      (
        answered(),
        answered(Answer.YesNo(0.5), Answer.YesNo(0.5)),
        answered(Answer.Choice("billing", Vector(w("billing", 1.0)), 1.0)),
        answered(Answer.YesNo(0.5))
      ) ==> (
        Left(ClassifierError.Unreadable("0 answers to 1 questions")),
        Left(ClassifierError.Unreadable("2 answers to 1 questions")),
        Left(ClassifierError.Unreadable("\"Is `ticket` urgent?\": answered a choice to a yes/no")),
        Right(Vector(Answer.YesNo(0.5)))
      )
    }

    test("an answer naming no key comes through Classifier.answers, refused only by Ask.read") {
      val hr = Answer.Choice("hr", Vector(w("hr", 1.0)), 1.0)
      department().map { q =>
        val got = new Canned(hr).answers(Request.of(Ticket("x"), q))
        (got.map(_.answers), got.flatMap(a => q.read(a.answers)).map(_ => ()))
      } ==> Right(
        (
          Right(Vector(hr)),
          Left(
            ClassifierError.Unreadable(
              "\"Which team should handle `ticket`?\": chose hr, not an option"
            )
          )
        )
      )
    }

    test("a choice whose keys repeat is not built") {
      department(sales = "billing").map(_ => ()) ==> Left(Ask.DuplicateKey("billing"))
    }

    test("Question.choice keeps its keys in order, and is not built when two share a name") {
      def key(name: String) = Question.Key(name, None)
      Question.choice("?", key("a"), key("b"), key("c")).map(_.keys.map(_.name)) ==>
        Right(Vector("a", "b", "c"))
      Question.choice("?", key("a"), key("b"), key("a")) ==> Left(Ask.DuplicateKey("a"))
    }

    test("Question.score keeps its levels in order, and is not built with more than ten") {
      Question.score("?", "calm", "frustrated", "very angry").map(_.levels) ==>
        Right(Vector("calm", "frustrated", "very angry"))
      val ten = (3 to 10).map(_.toString)
      Question.score("?", "1", "2", ten*).map(_.levels.size) ==> Right(Question.MaxLevels)
      Question.score("?", "1", "2", (ten :+ "11")*) ==> Left(Question.TooManyLevels(11))
    }

    test("Ask.answer keeps a score as given, Unreadable for another weight count or kind") {
      val frustration = Question
        .score("How frustrated is `ticket`?", "calm", "frustrated", "very angry")
        .getOrElse(throw new java.lang.AssertionError("levels"))
      def asked(answer: Answer) =
        new Canned(answer).ask(Ticket("x"), Ask.answer[Ticket](frustration)).map(_.value)
      // As given: a position beyond the levels, and weights not summing to 1, are kept.
      val raw = Answer.Score(2.5, Vector(0.2, 0.2, 0.9), 0.1)
      asked(raw) ==> Right(raw)
      val toScore = "\"How frustrated is `ticket`?\": "
      asked(Answer.Score(1.0, Vector(0.0, 1.0), 1.0)) ==>
        Left(ClassifierError.Unreadable(toScore + "weighs 2 levels of 3"))
      asked(Answer.YesNo(0.5)) ==>
        Left(ClassifierError.Unreadable(toScore + "answered yes/no to a score"))
      asked(Answer.Choice("calm", Vector(w("calm", 1.0)), 1.0)) ==>
        Left(ClassifierError.Unreadable(toScore + "answered a choice to a score"))
      new Canned(raw).ask(Ticket("x"), urgent) ==>
        Left(ClassifierError.Unreadable("\"Is `ticket` urgent?\": answered a score to a yes/no"))
      department().map(q => new Canned(raw).ask(Ticket("x"), q).map(_ => ())) ==>
        Right(
          Left(
            ClassifierError.Unreadable(
              "\"Which team should handle `ticket`?\": answered a score to a choice"
            )
          )
        )
    }

    test("Ask.answer keeps an answer as given, Unreadable for a key not asked or the other kind") {
      val team = Question
        .choice(
          "Which team should handle `ticket`?",
          Question.Key("billing", None),
          Question.Key("sales", None)
        )
        .getOrElse(throw new java.lang.AssertionError("keys"))
      val choice = Ask.answer[Ticket](team)
      val yesNo = Ask.answer[Ticket](Question.YesNo("Is `ticket` urgent?", None, None))
      def asked(q: Ask[Ticket, Answer], answer: Answer) =
        new Canned(answer).ask(Ticket("x"), q).map(_.value)
      // As given: weights not summing to 1, and on a key the question lacks, are kept.
      val raw = Answer.Choice("sales", Vector(w("sales", 0.7), w("hr", 0.6)), 0.2)
      asked(choice, raw) ==> Right(raw)
      asked(yesNo, Answer.YesNo(1.4)) ==> Right(Answer.YesNo(1.4))
      val toChoice = "\"Which team should handle `ticket`?\": "
      asked(choice, Answer.Choice("hr", Vector(w("hr", 1.0)), 1.0)) ==>
        Left(ClassifierError.Unreadable(toChoice + "chose hr, not an option"))
      asked(choice, Answer.YesNo(0.5)) ==>
        Left(ClassifierError.Unreadable(toChoice + "answered yes/no to a choice"))
      asked(yesNo, raw) ==>
        Left(ClassifierError.Unreadable("\"Is `ticket` urgent?\": answered a choice to a yes/no"))
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

    test("a score is read into its levels' values: Jev's documented answer") {
      val canned = new Canned(Answer.Score(1.05, Vector(0.0, 0.95, 0.05), 0.92))
      val read = frustration.flatMap(q => canned.ask(Ticket("x"), q).left.map(_.toString))
      read.map(a =>
        (a.value.position, a.value.likeliest, a.value.probability(Mood.Angry), a.value.confidence)
      ) ==> Right((1.05, Mood.Frustrated, 0.05, 0.92))
      read.map(_.value.probabilities) ==> Right(
        Vector(
          Decision.Weight(Mood.Calm, 0.0),
          Decision.Weight(Mood.Frustrated, 0.95),
          Decision.Weight(Mood.Angry, 0.05)
        )
      )
      canned.asked.collect { case q: Question.Score => q.levels } ==>
        Vector(Vector("Calm", "Frustrated", "Very angry"))
    }

    test("a score's weights are normalised, its likeliest the first on a tie, summed per value") {
      def scored(ps: Double*) = Ask
        .score[Ticket, Int]("?", Level(1, "a"), Level(2, "b"), Level(1, "c"))
        .flatMap(q =>
          new Canned(Answer.Score(1.0, ps.toVector, 0.5)).ask(Ticket("x"), q).left.map(_.toString)
        )
        .map(_.value)
      scored(1.0, 3.0, 0.0).map(s => s.probabilities.map(_.probability)) ==>
        Right(Vector(0.25, 0.75, 0.0))
      scored(0.25, 0.5, 0.25).map(s => (s.likeliest, s.probability(1), s.probability(3))) ==>
        Right((2, 0.5, 0.0))
      scored(0.4, 0.4, 0.2).map(_.likeliest) ==> Right(1)
    }

    test(
      "a score is Unreadable for another weight count, no weight, or a position off the levels"
    ) {
      def asked(answer: Answer) =
        frustration.map(q => new Canned(answer).ask(Ticket("x"), q).map(_.value.likeliest))
      val toScore = "\"How frustrated is `ticket`?\": "
      def refused(why: String) = Right(Left(ClassifierError.Unreadable(toScore + why)))
      asked(Answer.Score(1.0, Vector(0.0, 1.0), 1.0)) ==> refused("weighs 2 levels of 3")
      asked(Answer.Score(1.0, Vector(0.0, -1.0, Double.NaN), 1.0)) ==>
        refused("no level has weight")
      asked(Answer.Score(2.5, Vector(0.0, 0.0, 1.0), 1.0)) ==> refused("at 2.5, off the levels")
      asked(Answer.Score(-0.5, Vector(1.0, 0.0, 0.0), 1.0)) ==> refused("at -0.5, off the levels")
      asked(Answer.Score(Double.NaN, Vector(1.0, 0.0, 0.0), 1.0)) ==>
        refused("at NaN, off the levels")
      asked(Answer.YesNo(0.5)) ==> refused("answered yes/no to a score")
      // The ends themselves are on the levels.
      asked(Answer.Score(2.0, Vector(0.0, 0.0, 1.0), 1.0)) ==> Right(Right(Mood.Angry))
    }

    test("Ask.score is not built with more than ten levels") {
      val more = (3 to 11).map(i => Level(i, s"$i"))
      Ask.score[Ticket, Int]("?", Level(1, "1"), Level(2, "2"), more*).map(_ => ()) ==>
        Left(Question.TooManyLevels(11))
    }

    test("a score is built only by reading an answer") {
      import scala.compiletime.testing.typeChecks
      assert(
        typeChecks("(s: Scored[Int]) => s.likeliest"),
        !typeChecks("Scored(1.0, Vector(Decision.Weight(1, 5.0)), 0.0)(1)"),
        !typeChecks("(s: Scored[Int]) => s.copy(position = 9.0)")
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
