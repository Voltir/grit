package grit.lifecycle.settle

import grit.core.classify.{Ask, Classifier, ClassifierError, Criterion, StateJson}
import grit.core.period.{Judgement, Probability}

/** The one question a quiet period is asked: is its work finished, or is it waiting, on the
  * person or on something else, or is it unclear. Only finished can close it.
  */
object SettleQuestion {

  /** A period as the classifier is shown it: `{"transcript": ...}`, its turns in order. */
  final case class Transcript(text: String)

  given StateJson[Transcript] = StateJson.instance(t => ujson.Obj("transcript" -> t.text))

  private enum Standing {
    case Finished, OnPerson, OnOther, Unclear
  }

  private val question =
    Ask.choice[Transcript, Standing](
      "Read transcript. The conversation has gone quiet. Is the work it was about finished, " +
        "or is it waiting?",
      Criterion(
        Standing.Finished,
        "finished",
        Some("the question was answered or the task done, and nothing more is expected")
      ),
      Criterion(
        Standing.OnPerson,
        "waiting_on_person",
        Some("the assistant asked the person something, or is waiting for them to reply or act")
      ),
      Criterion(
        Standing.OnOther,
        "waiting_on_other",
        Some("it waits on something outside the conversation: a build, a review, someone else")
      ),
      Criterion(Standing.Unclear, "unclear", Some("the transcript does not say"))
    )

  /** What `classifier` makes of `transcript`: each option's probability and the model that
    * weighed them; `Unanswered`, with why, when it is unavailable or its answer does not read.
    */
  def judge(classifier: Classifier^, transcript: Transcript): Judgement =
    question match {
      case Left(Ask.DuplicateKey(key)) => Judgement.Unanswered(s"the question repeats $key")
      case Right(ask) =>
        classifier.ask(transcript, ask) match {
          case Right(answered) =>
            def p(o: Standing) = Probability.clamped(answered.value.probability(o))
            Judgement.Weighed(
              p(Standing.Finished),
              p(Standing.OnPerson),
              p(Standing.OnOther),
              p(Standing.Unclear),
              answered.model
            )
          case Left(ClassifierError.Unavailable(why)) => Judgement.Unanswered(s"unavailable: $why")
          case Left(ClassifierError.Unreadable(why)) => Judgement.Unanswered(s"unreadable: $why")
        }
    }
}
