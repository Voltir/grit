package grit.eval.harness.jev

import grit.core.classify.{Classifier, Request}
import grit.core.stitch.{Offer, Stitching, Tuning}
import grit.core.triage.Corpora
import grit.eval.harness.capture.Digest
import grit.lifecycle.triage.{TriageQuestion, TriageQuestions}

/** One of a case's questions, as the shipped call asks it. */
enum Asking {

  /** Triage's: `state`, in `wording`. */
  case Triage(state: TriageQuestion.State, wording: TriageQuestion.Wording)

  /** Stitching's: where `offer`'s message goes, under `tuning`. */
  case Stitch(offer: Offer, tuning: Tuning)

  /** A question set's, of `state`, with `sources`' per-source questions. */
  case Questions(set: TriageQuestions, state: TriageQuestion.State, sources: Corpora)
}

object Asking {

  /** `asking` put to `classifier` through the shipped call (v1 in its wording, or the set,
    * [[TriageQuestions.ask]]; [[Stitching.place]]); what it makes of the answer is dropped.
    */
  def ask(asking: Asking, classifier: Classifier^): Unit = asking match {
    case Triage(state, wording) =>
      val _ = TriageQuestions.v1(wording).ask(classifier, state, Corpora.Empty)
    case Stitch(offer, tuning) => val _ = Stitching.place(classifier, offer, tuning)
    case Questions(set, state, sources) => val _ = set.ask(classifier, state, sources)
  }
}

/** A question as rebuilt: how it is asked, the request the shipped builder makes for it, and
  * the digest of the state it shows.
  */
final case class Posed(asking: Asking, request: Request, state: Digest)
