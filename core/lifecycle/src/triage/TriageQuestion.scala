package grit.lifecycle.triage

import grit.core.classify.{
  Ask,
  Classifier,
  ClassifierError,
  Criterion,
  Decision,
  Request,
  StateJson
}
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

  /** The words triage's question is asked in: the kind question's instructions and what
    * each kind means, and each yes/no question's instructions. The keys the classifier
    * chooses among and the state's fields the words refer to ([[State]]) are fixed.
    */
  final case class Wording(
      kind: String,
      kinds: Wording.Kinds,
      waiting: String,
      durable: String,
      helps: String
  )

  object Wording {

    /** What each kind of message means, as the kind question describes it. */
    final case class Kinds(
        question: String,
        answer: String,
        decision: String,
        announcement: String,
        chatter: String
    )

    /** The words triage ships with. */
    val Shipped: Wording = Wording(
      kind =
        "Read new_message, said by author in a team's thread; thread is what came before it. " +
          "Nobody said it to the assistant. What kind of message is it?",
      kinds = Kinds(
        question = "it asks something",
        answer = "it answers a question asked in the thread",
        decision = "it settles something: a choice, a date, a plan",
        announcement = "it tells the team news or a change",
        chatter = "greetings, thanks, jokes, small talk"
      ),
      waiting =
        "Read new_message and thread. Is someone waiting on a reply or an action because of " +
          "new_message?",
      durable =
        "Read new_message and thread. Does new_message state something worth keeping for later: " +
          "a decision, a date, a name, a number, how something works, or an answer to a question? " +
          "Greetings, thanks and small talk do not count.",
      helps =
        "Read new_message and thread. Would a short reply from an assistant that remembers the " +
          "team's past conversations help the people in this thread now?"
    )
  }

  /** The four questions, in `wording`, asked together; why not, when the kind question
    * repeats a key.
    */
  private def questions(
      wording: Wording
  ): Either[Ask.DuplicateKey, Ask[State, (((Decision[Kind], Double), Double), Double)]] = {
    val k = wording.kinds
    def yesNo(instructions: String) = Ask.yesNo[State](instructions, None, None)
    Ask
      .choice[State, Kind](
        wording.kind,
        Criterion(Kind.Question, "question", Some(k.question)),
        Criterion(Kind.Answer, "answer", Some(k.answer)),
        Criterion(Kind.Decision, "decision", Some(k.decision)),
        Criterion(Kind.Announcement, "announcement", Some(k.announcement)),
        Criterion(Kind.Chatter, "chatter", Some(k.chatter))
      )
      .map(_.zip(yesNo(wording.waiting)).zip(yesNo(wording.durable)).zip(yesNo(wording.helps)))
  }

  /** The request [[judge]] sends for `state` in `wording`; `None` when it sends none, because
    * its kind question repeats a key.
    */
  def request(wording: Wording, state: State): Option[Request] =
    questions(wording).toOption.map(Request.of(state, _))

  /** What `classifier` makes of `state`, asked in `wording`: the most probable kind and its
    * probability, and the probability of yes to each yes/no question, with the model that
    * weighed them and what the call consumed; `Unanswered`, with why, when it is unavailable
    * or its answer does not read.
    */
  def judge(classifier: Classifier^, wording: Wording, state: State): Tags =
    questions(wording) match {
      case Left(Ask.DuplicateKey(key)) => Tags.Unanswered(s"the question repeats $key")
      case Right(asked) =>
        classifier.ask(state, asked) match {
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
