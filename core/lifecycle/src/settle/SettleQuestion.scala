package grit.lifecycle.settle

import grit.core.classify.{Ask, Classifier, ClassifierError, Criterion, StateJson}
import grit.core.period.{Judgement, Probability}

/** The one question a quiet period is asked: is anyone waiting on anything, the person or
  * something else, or nobody. Only nobody can close it: a lookup or a recap, with nothing
  * left to do, closes as readily as a finished task.
  */
object SettleQuestion {

  /** A period as the classifier is shown it: `{"transcript": ...}`, its turns in order. */
  final case class Transcript(text: String)

  given StateJson[Transcript] = StateJson.instance(t => ujson.Obj("transcript" -> t.text))

  private enum Standing {
    case Nobody, OnPerson, OnOther
  }

  private val question =
    Ask.choice[Transcript, Standing](
      "Read transcript. The conversation has gone quiet. Is anyone waiting on anything?",
      Criterion(
        Standing.Nobody,
        "nobody",
        Some(
          "nothing is left to do or answer: the question was answered, the task done, or it was only a lookup"
        )
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
      )
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
              p(Standing.Nobody),
              p(Standing.OnPerson),
              p(Standing.OnOther),
              answered.model
            )
          case Left(ClassifierError.Unavailable(why)) => Judgement.Unanswered(s"unavailable: $why")
          case Left(ClassifierError.Unreadable(why)) => Judgement.Unanswered(s"unreadable: $why")
        }
    }
}
