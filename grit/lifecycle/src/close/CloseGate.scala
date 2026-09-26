package grit.lifecycle.close

import grit.core.classify.{Ask, Classifier, ClassifierError, StateJson}
import grit.core.period.{Balance, Section}

/** What a period's closing needs, asked of a classifier in one call, beside what is already
  * known: was the request answered (the outcome); did it settle a decision or establish a
  * fact not already known; did it leave something open not already known; did it answer,
  * finish or overturn anything already known. A period that did none of the last three,
  * such as a recap or a lookup, is carried with no summary.
  */
object CloseGate {

  /** A period as the classifier is shown it: `{"already_known": {"open": [...],
    * "standing": [...]}, "known_elsewhere": [...], "transcript": ...}`, `known`'s open and
    * standing lines, the lines its windows showed from other conversations (`elsewhere`,
    * [[grit.lifecycle.transcript.PeriodTranscript.elsewhere]]), then its turns in order.
    */
  final case class Transcript(known: Balance, elsewhere: Vector[String], text: String)

  given StateJson[Transcript] = StateJson.instance(t =>
    ujson.Obj(
      "already_known" -> ujson.Obj(
        "open" -> ujson.Arr.from(t.known.in(Section.Open).map(l => ujson.Str(l.text))),
        "standing" -> ujson.Arr.from(t.known.in(Section.Standing).map(l => ujson.Str(l.text)))
      ),
      "known_elsewhere" -> ujson.Arr.from(t.elsewhere.map(ujson.Str(_))),
      "transcript" -> t.text
    )
  )

  /** The probability of yes at or above which a part is asked for. */
  val Threshold = 0.5

  private val questions: Ask[Transcript, Asked] =
    Ask
      .yesNo[Transcript](
        "Read transcript. Was the question or request that started it answered or resolved?",
        Some("an answer or a result was reached"),
        Some("it was left unanswered, or there was no real question")
      )
      .zip(
        Ask.yesNo[Transcript](
          "Read transcript. Did it settle a decision or establish a fact worth keeping (a " +
            "name, a number, a path, how something works) that is not already in " +
            "already_known or known_elsewhere? Recaps, lookups and lists of earlier activity " +
            "do not count.",
          None,
          None
        )
      )
      .zip(
        Ask.yesNo[Transcript](
          "Read transcript. Did it leave something open (a question unanswered, a task " +
            "unfinished, a follow-up promised, something not known) that is not already in " +
            "already_known or known_elsewhere?",
          None,
          None
        )
      )
      .zip(
        Ask.yesNo[Transcript](
          "Read transcript. Did it answer, finish or overturn anything listed in " +
            "already_known?",
          None,
          None
        )
      )
      .map { case (((outcome, standing), open), settled) =>
        Asked(outcome >= Threshold, open >= Threshold, standing >= Threshold, settled >= Threshold)
      }

  /** The parts `transcript` needs, as `classifier` answers; [[Asked.Every]], and why, when it
    * does not answer.
    */
  def asked(classifier: Classifier^, transcript: Transcript): (Asked, Option[String]) =
    classifier.ask(transcript, questions) match {
      case Right(answered) => (answered.value, None)
      case Left(ClassifierError.Unavailable(why)) => (Asked.Every, Some(s"gate unavailable: $why"))
      case Left(ClassifierError.Unreadable(why)) => (Asked.Every, Some(s"gate unreadable: $why"))
    }
}
