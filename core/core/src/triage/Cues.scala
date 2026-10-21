package grit.core.triage

import grit.core.id.{EntryId, PluginName}
import grit.core.period.{Judgement, Probability}

/** What code read around a heard message when it was weighed, beside triage's answers, each as
  * it stood then: whether it names someone other than grit (`named`,
  * [[grit.core.speech.Reach.asked]]); the assistant's own reply after it (`spoken`); its
  * author's next message said before the decision, in its thread or an opening placed to follow
  * its strand (`continued`); the latest settle verdict on the exchange it follows, made before
  * it (`settled`: its own conversation's for a reply, the followed root's for an opening; `None`
  * when that exchange has no open period or no verdict); and the best document match for its
  * text and its thread's tail (`records`; `None` when nothing matched). An unanswered verdict is
  * kept with its words.
  */
final case class Cues(
    named: Boolean,
    spoken: Option[EntryId],
    continued: Option[EntryId],
    settled: Option[Judgement],
    records: Option[Cues.Match]
)

object Cues {

  /** The best match's `plugin` and `score` (on the document index's own scale, comparable only
    * within one deployment's index), and how many documents `matched`, at most [[Searched]].
    */
  final case class Match(plugin: PluginName, score: Double, matched: Int)

  /** 3: how many of the best record matches a heard message's cue reads. */
  val Searched: Int = 3

  /** Nothing read: what a decision kept before cues reads as. */
  val Empty: Cues = Cues(named = false, None, None, None, None)
}

/** What code read around a message, as a probability a gate reads ([[Reading.Fact]]): always
  * answered, and 0 where its cue is absent, as for a row kept before cues. So a fact only
  * raises: written as `AtLeast` within an `AnyOf` branch, or `Below` beside a Jev reading,
  * never as a `Below` that alone would hold every row without it.
  */
enum Fact {

  /** 1 when it names someone other than grit, else 0. */
  case Named

  /** The followed exchange's latest settle verdict's probability that a person was left
    * waiting; 0 when there is none or it was unanswered.
    */
  case SettledOnPerson

  /** `s / (PlainMatch + s)` over the best record match's score `s`: one half at
    * [[Fact.PlainMatch]]; 0 when nothing matched.
    */
  case RecordScore

  /** Its value read from `cues`. */
  def read(cues: Cues): Probability = this match {
    case Named => if (cues.named) Probability.One else Probability.Zero
    case SettledOnPerson =>
      cues.settled.fold(Probability.Zero) {
        case Judgement.Weighed(_, onPerson, _, _) => onPerson
        case Judgement.Unanswered(_) => Probability.Zero
      }
    case RecordScore =>
      // The index scores a match above 0; a score that is not is read as no match.
      cues.records.fold(Probability.Zero)(m =>
        if (m.score > 0) Probability.clamped(m.score / (Fact.PlainMatch + m.score))
        else Probability.Zero
      )
  }
}

object Fact {

  /** 20: the best record match's score at which [[Fact.RecordScore]] reads one half, where a
    * record plainly matches. The document index scores BM25 summed over the query's words, so
    * any shared word scores, and a longer query scores higher. On one deployment's index of
    * about 140 documents, a heard message's best match (its words and its thread's tail)
    * scored a median of 7 and a 90th percentile of 18, a line of small talk alone up to 9.5,
    * and 120 characters quoted from a document at least 31.
    */
  val PlainMatch: Double = 20.0
}
