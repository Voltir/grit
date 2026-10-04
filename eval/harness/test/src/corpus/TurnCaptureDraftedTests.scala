package grit.eval.harness.corpus

import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.speech.{Judged, Outcome}

import utest.*

/** What a case keeps of a draft's outcome: its kind and the judge's scores, by what it judged. */
object TurnCaptureDraftedTests extends TestSuite {

  private def p(x: Double) = Probability.clamped(x)
  private val usage = Usage(Tokens(40), Tokens.Zero, Tokens.Zero, None)

  val tests = Tests {
    test("an unprompted draft keeps grounded and worth; a named one keeps answers") {
      Vector(
        Outcome.Below(Judged(Judged.Scores.Unprompted(p(0.4), p(0.7)), "jev", usage), p(0.5)),
        Outcome.Posted(Judged(Judged.Scores.Named(p(0.8)), "jev", usage))
      ).map(TurnCapture.drafted) ==> Vector(
        Drafted(Drafted.Kind.Below, Some(p(0.4)), Some(p(0.7)), None, Some(p(0.5))),
        Drafted(Drafted.Kind.Posted, None, None, Some(p(0.8)), None)
      )
    }
  }
}
