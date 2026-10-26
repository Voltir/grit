package grit.core.screen

import grit.core.classify.{Answer, ClassifierError, Question, StateJson}
import grit.core.period.Probability

import utest.*

object ScreenTests extends TestSuite {

  private val meta = Screen.Fixed("Does `text` call itself a rewrite?", "calls itself a rewrite")
  private val first = Screen.Fixed("Does `text` say 'I'?", "says 'I'")

  private val codename = Signal("the codename", Must.Absent)
  private val standIn = Signal("that a tour is planned", Must.Present)
  private val venue = Signal("the venue", Must.Absent)

  /** The floor, then one source of `codename` and `standIn`, then one of `venue`. */
  private val screen =
    Screen(
      Vector(meta, first),
      Signals(codename, Vector(standIn)),
      Vector(Signals(venue, Vector.empty))
    )

  private def p(d: Double): Probability = Probability.clamped(d)

  private def screened(s: Screen, yes: Double*): Screened =
    s.ask
      .read(yes.toVector.map(Answer.YesNo(_)))
      .fold(e => throw new java.lang.AssertionError(e.said), identity)

  private def floorOnly(yes: Double*): Screened =
    screened(
      Screen(Vector(meta, first), Signals(standIn, Vector.empty), Vector.empty),
      (yes :+ 0.9)*
    )

  private def baseline(yes: Double*): Screen.Baseline =
    Screen
      .readBaseline(yes.size, yes.toVector.map(Answer.YesNo(_)))
      .fold(e => throw new java.lang.AssertionError(e.said), identity)

  private def instructions(qs: Vector[Question]): Vector[String] =
    qs.collect { case Question.YesNo(i, None, None) => i }

  val tests = Tests {
    test("an absent signal is asked Screen.absent(infer), a present one Screen.present(infer)") {
      (Screen.absent("the codename"), Screen.present("that a tour is planned")) ==> (
        "Does `text` state, hint at or let a reader infer the codename?",
        "Does `text` state that a tour is planned?"
      )
      val bare = Screen(Vector.empty, Signals(codename, Vector(standIn)), Vector.empty)
      instructions(bare.ask.questions) ==> Vector(
        Screen.absent("the codename"),
        Screen.present("that a tour is planned")
      )
    }

    test("the floor first, then each source's signals in order") {
      instructions(screen.ask.questions) ==> Vector(
        meta.question,
        first.question,
        Screen.absent("the codename"),
        Screen.present("that a tour is planned"),
        Screen.absent("the venue")
      )
      screen.shape ==> Screen.Shape(2, Vector(Must.Absent, Must.Present, Must.Absent))
    }

    test("the baseline asks the floor alone, and a screen with no floor has none") {
      val bare = Screen(Vector.empty, Signals(codename, Vector.empty), Vector.empty)
      (screen.baseline.map(b => instructions(b.questions)), bare.baseline.isEmpty) ==> (
        Some(Vector(meta.question, first.question)),
        true
      )
    }

    test("the state screened is the text alone") {
      StateJson[Screen.Text].json(Screen.Text("A tour.")) ==> ujson.Obj("text" -> "A tour.")
    }

    test("an absent answer at Infers fails, just below passes") {
      (
        screened(screen, 0, 0, 0.4, 0.9, 0).fired(None),
        screened(screen, 0, 0, 0.39, 0.9, 0).fired(None).passes
      ) ==> (Fired(Vector.empty, Vector(0), Vector.empty), true)
    }

    test("a present answer just below States fails, at States passes") {
      (
        screened(screen, 0, 0, 0, 0.49, 0).fired(None),
        screened(screen, 0, 0, 0, 0.5, 0).fired(None).passes
      ) ==> (Fired(Vector.empty, Vector.empty, Vector(1)), true)
    }

    test("a floor answer at Infers fails when the source's is below Infers") {
      val rewrite = floorOnly(0.4, 0.1)
      (rewrite.floorFires, rewrite.fired(Some(baseline(0.39, 0.0)))) ==> (
        true,
        Fired(Vector(0), Vector.empty, Vector.empty)
      )
    }

    test("a floor answer at Infers is excused when the source's is at Infers") {
      floorOnly(0.9, 0.1).fired(Some(baseline(0.4, 0.0))).passes ==> true
    }

    test("a floor answer at Infers fails when no baseline was read") {
      (floorOnly(0.1, 0.4).fired(None), floorOnly(0.1, 0.39).floorFires) ==> (
        Fired(Vector(1), Vector.empty, Vector.empty),
        false
      )
    }

    test("fired lists each failing position by kind, floor first, and failed names them in order") {
      val fired = screened(screen, 0.0, 0.8, 0.2, 0.1, 0.7).fired(Some(baseline(0.9, 0.0)))
      (fired, fired.passes, screen.failed(fired)) ==> (
        Fired(Vector(1), Vector(2), Vector(1)),
        false,
        Vector(Left(first), Right(standIn), Right(venue))
      )
    }

    test(
      "judged is the highest absent or unexcused floor answer, never a present one, 0 for none"
    ) {
      val excusedFloor = screened(screen, 0.95, 0.3, 0.2, 0.1, 0.35)
      (
        excusedFloor.judged(Some(baseline(0.6, 0.0))),
        excusedFloor.judged(None),
        screened(Screen(Vector.empty, Signals(standIn, Vector.empty), Vector.empty), 0.05)
          .judged(None)
      ) ==> (p(0.35), p(0.95), Probability.Zero)
    }

    test("answers that are not one yes/no per question do not read, naming no question") {
      val marked = Signal("QJZ", Must.Absent)
      val s = Screen(Vector(meta), Signals(marked, Vector.empty), Vector.empty)
      val reads = Vector(
        s.ask.read(Vector(Answer.YesNo(0.1))),
        s.ask.read(Vector.fill(3)(Answer.YesNo(0.1))),
        s.ask.read(Vector(Answer.YesNo(0.1), Answer.Score(0.0, Vector(1.0), 1.0))),
        s.ask.read(Vector(Answer.YesNo(Double.NaN), Answer.YesNo(0.1))),
        Screen.read(s.shape, Vector(Answer.YesNo(0.1), Answer.YesNo(1.5))),
        Screen.read(Screen.Shape(1, Vector.empty), Vector(Answer.YesNo(0.1)))
      )
      reads ==> Vector(
        Left(ClassifierError.Unreadable("1 answers to 2 questions")),
        Left(ClassifierError.Unreadable("3 answers to 2 questions")),
        Left(ClassifierError.Unreadable("answer 2 is not a yes/no")),
        Left(ClassifierError.Unreadable("answer 1 is not a probability")),
        Left(ClassifierError.Unreadable("answer 2 is not a probability")),
        Left(ClassifierError.Unreadable("a screen of no signal"))
      )
    }

    test("recorded answers read again as they were first read") {
      val answers = Vector(0.1, 0.2, 0.3, 0.6, 0.7).map(Answer.YesNo(_))
      val first = screen.ask.read(answers)
      (Screen.read(screen.shape, answers), first.map(_.signals)) ==> (
        first,
        Right(Vector(p(0.3), p(0.6), p(0.7)))
      )
    }
  }
}
