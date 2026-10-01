package grit.lifecycle.triage

import grit.core.classify.{Ask, Classifier, ClassifierError, Criterion, StateJson}
import grit.core.period.Probability
import grit.core.triage.{Kind, Tags}

/** What a heard message is asked, in one classifier call: what kind of message it is, and
  * yes or no whether someone waits on a reply, whether it states something worth keeping,
  * and whether a reply from grit would help.
  */
object TriageQuestion {

  /** A heard message as the classifier is shown it: `{"new_message": ..., "author": ...,
    * "thread": ...}`, its text, who said it, and the thread before it, its last
    * [[ThreadChars]].
    */
  final case class State(message: String, author: String, thread: String)

  /** How much of the thread before the message, from its end, the classifier is shown. */
  val ThreadChars = 2_000

  given StateJson[State] = StateJson.instance(s =>
    ujson.Obj(
      "new_message" -> s.message,
      "author" -> s.author,
      "thread" -> s.thread.takeRight(ThreadChars)
    )
  )

  private val kind =
    Ask.choice[State, Kind](
      "Read new_message, said by author in a team's thread; thread is what came before it. " +
        "Nobody said it to the assistant. What kind of message is it?",
      Criterion(Kind.Question, "question", Some("it asks something")),
      Criterion(Kind.Answer, "answer", Some("it answers a question asked in the thread")),
      Criterion(Kind.Decision, "decision", Some("it settles something: a choice, a date, a plan")),
      Criterion(Kind.Announcement, "announcement", Some("it tells the team news or a change")),
      Criterion(Kind.Chatter, "chatter", Some("greetings, thanks, jokes, small talk"))
    )

  private def yesNo(instructions: String) = Ask.yesNo[State](instructions, None, None)

  private val waiting = yesNo(
    "Read new_message and thread. Is someone waiting on a reply or an action because of " +
      "new_message?"
  )

  private val durable = yesNo(
    "Read new_message and thread. Does new_message state something worth keeping for later: " +
      "a decision, a date, a name, a number, how something works, or an answer to a question? " +
      "Greetings, thanks and small talk do not count."
  )

  private val helps = yesNo(
    "Read new_message and thread. Would a short reply from an assistant that remembers the " +
      "team's past conversations help the people in this thread now?"
  )

  /** What `classifier` makes of `state`: the most probable kind and its probability, and the
    * probability of yes to each yes/no question, with the model that weighed them and what
    * the call consumed; `Unanswered`, with why, when it is unavailable or its answer does not
    * read.
    */
  def judge(classifier: Classifier^, state: State): Tags =
    kind match {
      case Left(Ask.DuplicateKey(key)) => Tags.Unanswered(s"the question repeats $key")
      case Right(k) =>
        classifier.ask(state, k.zip(waiting).zip(durable).zip(helps)) match {
          case Right(answered) =>
            val (((decision, w), d), h) = answered.value
            Tags.Weighed(
              decision.choice,
              Probability.clamped(decision.top),
              Probability.clamped(w),
              Probability.clamped(d),
              Probability.clamped(h),
              answered.model,
              answered.usage
            )
          case Left(ClassifierError.Unavailable(why)) => Tags.Unanswered(s"unavailable: $why")
          case Left(ClassifierError.Unreadable(why)) => Tags.Unanswered(s"unreadable: $why")
        }
    }
}
