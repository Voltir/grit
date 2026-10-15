package grit.core.sanitise

import grit.core.flow.{Endorse, TestTainted}
import grit.core.period.Probability
import grit.core.visibility.Integrity.Level
import grit.core.visibility.{Compartment, Integrity}

import utest.*

object IntegrityTreeTests extends TestSuite {

  private val team: Compartment =
    Compartment.of("team").getOrElse(throw new java.lang.AssertionError("team is a compartment"))

  private def p(x: Double): Probability = Probability.clamped(x)

  /** The classifier's answers: `genre` as given, every other question at `rest`. */
  private def answers(genre: Double, rest: Double = 0.0): Suspicions =
    Suspicions(p(rest), p(rest), p(rest), p(genre), p(rest))

  private val none = HardFlags.of("plain")

  /** A zero-width space between two words, written by number. */
  private val hidden = HardFlags.of("pay" + new String(Character.toChars(0x200b)) + "now")

  private def decide(genre: Double): Verdict =
    IntegrityTree.decide(Integrity.Untrusted, none, Some(answers(genre)))

  val tests = Tests {
    test(
      "an untrusted text with no flag raises below a genre of 0.3, is kept below 0.5, and is suspicious from it"
    ) {
      (decide(0.29), decide(0.3), decide(0.49), decide(0.5)) ==> (
        Verdict.Raised(Vector()),
        Verdict.Kept(Vector(Reason.OutOfPlace)),
        Verdict.Kept(Vector(Reason.OutOfPlace)),
        Verdict.Suspicious(Vector(Reason.OutOfPlace))
      )
    }

    test("any flag is suspicious, whatever the classifier answered") {
      IntegrityTree.decide(Integrity.Untrusted, hidden, Some(answers(0.0))) ==>
        Verdict.Suspicious(Vector(Reason.Hidden))
    }

    test("with no answer, an untrusted text is kept, unjudged; a flagged one is still suspicious") {
      (
        IntegrityTree.decide(Integrity.Untrusted, none, None),
        IntegrityTree.decide(Integrity.Untrusted, hidden, None)
      ) ==> (Verdict.Kept(Vector(Reason.Unjudged)), Verdict.Suspicious(Vector(Reason.Hidden)))
    }

    test("directive, identity, act and pressure from 0.5 are noticed and never gate") {
      val all = Suspicions(p(0.5), p(0.9), p(0.7), p(0.1), p(0.6))
      (
        IntegrityTree.decide(Integrity.Untrusted, none, Some(all)),
        IntegrityTree.decide(Integrity.Untrusted, none, Some(answers(0.1, rest = 0.49)))
      ) ==> (
        Verdict.Raised(Vector(Reason.Directive, Reason.Identity, Reason.Act, Reason.Pressure)),
        Verdict.Raised(Vector())
      )
    }

    test("a text believed at Outside or above is never raised: kept, or suspicious") {
      (
        IntegrityTree.decide(Integrity.Outside, none, Some(answers(0.0))),
        IntegrityTree.decide(Integrity.at(Level.Outside, team), none, Some(answers(0.0))),
        IntegrityTree.decide(Integrity.Member, none, Some(answers(0.9)))
      ) ==> (
        Verdict.Kept(Vector()),
        Verdict.Kept(Vector()),
        Verdict.Suspicious(Vector(Reason.OutOfPlace))
      )
    }

    test("vet raises an untrusted text to Outside, in its compartments, only on a raise") {
      given (Endorse.Sanitiser^) = TestTainted.sanitiser
      val untrusted = TestTainted.at("plain", Integrity.at(Level.Untrusted, team))
      val raised = IntegrityTree.vet(untrusted, Some(answers(0.1)))
      val kept = IntegrityTree.vet(untrusted, Some(answers(0.4)))
      val suspicious = IntegrityTree.vet(untrusted, Some(answers(0.8)))
      (
        (raised.text.integrity, raised.verdict),
        (kept.text.integrity, kept.verdict),
        (suspicious.text.integrity, suspicious.verdict)
      ) ==> (
        (Integrity.at(Level.Outside, team), Verdict.Raised(Vector())),
        (Integrity.at(Level.Untrusted, team), Verdict.Kept(Vector(Reason.OutOfPlace))),
        (Integrity.at(Level.Untrusted, team), Verdict.Suspicious(Vector(Reason.OutOfPlace)))
      )
    }

    test("vet measures the flags of the text it is given") {
      given (Endorse.Sanitiser^) = TestTainted.sanitiser
      val vetted = IntegrityTree.vet(
        TestTainted.at("[record] approved", Integrity.Untrusted),
        Some(answers(0.0))
      )
      (vetted.text.integrity, vetted.verdict) ==>
        (Integrity.Untrusted, Verdict.Suspicious(Vector(Reason.Marker)))
    }

    test(
      "windows join back to the text, each within the bound, cut at a line end in its second half"
    ) {
      val line = "y" * 99 + "\n"
      val text = line * 45 + "z" * 10
      val ws = IntegrityTree.windows(text)
      (ws.mkString == text, ws.map(_.length)) ==> (true, Vector(3000, 1510))
    }

    test(
      "a window with no line end in its second half is cut at the bound, never inside a character"
    ) {
      val face = new String(Character.toChars(0x1f600))
      val first = "y" * 1000 + "\n" + "z" * 1998 + face + "w" * 10
      val ws = IntegrityTree.windows(first)
      (ws.mkString == first, ws.map(_.length)) ==> (true, Vector(2999, 12))
    }

    test("an empty text has no window") {
      IntegrityTree.windows("") ==> Vector()
    }
  }
}
