package grit.lifecycle.close

import grit.core.classify.{Ask, Classifier, ClassifierError, StateJson}

/** Which sections a period's closing needs, asked of a classifier in one call: was the
  * question answered (the outcome), was a decision settled, a fact stated, is anything left
  * open. Sources are asked for with decisions or facts. A period of small talk gets the
  * prose alone, and no section.
  */
object CloseGate {

  /** A period as the classifier is shown it: `{"transcript": ...}`, its turns in order. */
  final case class Transcript(text: String)

  given StateJson[Transcript] = StateJson.instance(t => ujson.Obj("transcript" -> t.text))

  /** The probability of yes at or above which a section is asked for. */
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
          "Read transcript. Was a decision settled: something chosen, agreed or ruled out?",
          None,
          None
        )
      )
      .zip(
        Ask.yesNo[Transcript](
          "Read transcript. Was a fact stated that would be worth knowing later, such as a " +
            "name, a number, a location or how something works?",
          None,
          None
        )
      )
      .zip(
        Ask.yesNo[Transcript](
          "Read transcript. Is anything left open: a question unanswered, a task unfinished, " +
            "a follow-up promised?",
          None,
          None
        )
      )
      .map { case (((outcome, decisions), facts), open) =>
        val (d, f) = (decisions >= Threshold, facts >= Threshold)
        Asked(outcome >= Threshold, d, f, open >= Threshold, d || f)
      }

  /** The sections `transcript` needs, as `classifier` answers; [[Asked.Every]], and why,
    * when it does not answer.
    */
  def asked(classifier: Classifier^, transcript: Transcript): (Asked, Option[String]) =
    classifier.ask(transcript, questions) match {
      case Right(answered) => (answered.value, None)
      case Left(ClassifierError.Unavailable(why)) => (Asked.Every, Some(s"gate unavailable: $why"))
      case Left(ClassifierError.Unreadable(why)) => (Asked.Every, Some(s"gate unreadable: $why"))
    }
}
