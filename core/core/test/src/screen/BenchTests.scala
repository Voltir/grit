package grit.core.screen

import grit.core.classify.{Answer, Question}
import grit.core.period.Probability

import Benched.Kind.{Accept, Refuse}
import utest.*

object BenchTests extends TestSuite {

  /** An absent signal, then a present one; two accepts and one refuse. */
  private val checks: Checks =
    Checks(
      Signals(
        Signal("the codename", Must.Absent),
        Vector(Signal("that a tour is planned", Must.Present))
      ),
      Examples(Vector("A tour is planned.", "A spring tour is planned."), Vector("KETTAMA tours."))
    )

  private def p(d: Double): Probability = Probability.clamped(d)

  /** An example of `kind` screened by the checks' signals answering `absent` and `present`. */
  private def answered(
      kind: Benched.Kind,
      absent: Double,
      present: Double
  ): (Benched.Kind, Screened) =
    (
      kind,
      Bench
        .screen(checks)
        .ask
        .read(Vector(Answer.YesNo(absent), Answer.YesNo(present)))
        .fold(e => throw new java.lang.AssertionError(e.said), identity)
    )

  private def held: Vector[(Benched.Kind, Screened)] =
    Vector(answered(Accept, 0.1, 0.9), answered(Accept, 0.2, 0.8), answered(Refuse, 0.7, 0.9))

  val tests = Tests {
    test("the bench asks no floor question") {
      val screen = Bench.screen(checks)
      (
        screen.floor,
        screen.shape.floor,
        screen.ask.questions.collect { case Question.YesNo(i, _, _) =>
          i
        }
      ) ==> (
        Vector.empty,
        0,
        Vector(Screen.absent("the codename"), Screen.present("that a tour is planned"))
      )
    }

    test("example gives the accepts then the refuses by index from 0, and None outside them") {
      (0 to 4).map(i => Bench.example(checks, i - 1)).toVector ==> Vector(
        None,
        Some((kind = Accept, text = Screen.Text("A tour is planned."))),
        Some((kind = Accept, text = Screen.Text("A spring tour is planned."))),
        Some((kind = Refuse, text = Screen.Text("KETTAMA tours."))),
        None
      )
    }

    test("a bench passes when every accept passes and every refuse is refused") {
      val benched = Bench.verdict(held)
      (benched.passed, benched.line, benched.examples.map(e => (e.kind, e.at, e.held))) ==> (
        true,
        None,
        Vector((Accept, 0, true), (Accept, 1, true), (Refuse, 0, true))
      )
    }

    test("an accept refused fails it, naming the example and its deciding signal") {
      val benched = Bench.verdict(held.updated(1, answered(Accept, 0.63, 0.9)))
      (benched.passed, benched.line.map(_.said)) ==> (
        false,
        Some("accept example 2 refused by signal 1 (0.63)")
      )
    }

    test("a refuse that passes fails it, naming its deciding signal") {
      val benched = Bench.verdict(held.updated(2, answered(Refuse, 0.21, 0.9)))
      (benched.passed, benched.line.map(_.said)) ==> (
        false,
        Some("refuse example 1 passed (signal 1, 0.21)")
      )
    }

    test("the deciding signal is the one furthest toward failing, for either kind") {
      def deciding(kind: Benched.Kind, absent: Double, present: Double) =
        Bench.verdict(Vector(answered(kind, absent, present))).examples.map(e => (e.signal, e.p))
      (
        deciding(Accept, 0.45, 0.52), // the absent fails; the present passes nearer its own line
        deciding(Accept, 0.1, 0.3), // the present fails
        deciding(Refuse, 0.38, 0.6), // neither fails: the absent is nearer failing
        deciding(Refuse, 0.2, 0.55) // neither fails: the present is nearer failing
      ) ==> (
        Vector((0, p(0.45))),
        Vector((1, p(0.3))),
        Vector((0, p(0.38))),
        Vector((1, p(0.55)))
      )
    }

    test("of two signals as far toward failing, the first decides") {
      val twin = Signal("the codename", Must.Absent)
      val twins = Checks(Signals(twin, Vector(twin)), checks.examples)
      val screened = Bench.screen(twins).ask.read(Vector(Answer.YesNo(0.2), Answer.YesNo(0.2)))
      screened.map(s => Bench.verdict(Vector((Refuse, s))).examples.map(_.signal)) ==> Right(
        Vector(0)
      )
    }

    test("a bench's examples are written and read back as they were") {
      val benched = Bench.verdict(held.updated(1, answered(Accept, 0.63, 0.9)))
      val written = Benched.write(benched)
      (written(1), Benched.read(benched.passed, written)) ==> (
        ujson.Obj("at" -> 1, "kind" -> "accept", "must" -> "absent", "p" -> 0.63, "signal" -> 0),
        Right(benched)
      )
    }

    test("a row whose examples are not as written, or disagree with its passed, does not read") {
      val written = Benched.write(Bench.verdict(held))
      val unknownKind = Benched.write(Bench.verdict(held))
      unknownKind(0)("kind") = "maybe"
      (
        Benched.read(false, written),
        Benched.read(true, ujson.Obj()),
        Benched.read(true, unknownKind)
      ) ==> (
        Left("passed disagrees with its examples"),
        Left("examples: not a list"),
        Left("example 1: not of its shape")
      )
    }
  }
}
