package grit.lifecycle.triage

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.classify.StateJson
import grit.core.recipe.Section
import grit.core.stitch.Stitching

/** A heard message as triage's questions are asked about it ([[TriageQuestions]]), and v1's
  * words ([[TriageQuestion.Wording]]).
  */
object TriageQuestion {

  /** A message said `at` as the classifier is shown it: `{new_message, author, earlier_exchange:
    * {opening, latest, not_shown}, thread, thread_not_shown}`, each message `{from, text, ago}`
    * ([[grit.core.stitch.Stitching.Line.json]]), every key but `thread` left out when empty;
    * then each of `sections` whose text is not empty, under its key, in order.
    */
  final case class State(
      message: String,
      author: String,
      at: Instant,
      thread: Stitching.Fitted,
      sections: VectorMap[Section, String] = VectorMap.empty
  )

  /** The most of the thread before the message, strand included, the classifier is shown
    * ([[grit.core.stitch.Stitching.fit]]).
    */
  val ThreadChars = 2_000

  given StateJson[State] = StateJson.instance { s =>
    val strand = s.thread.strand
    def said(lines: Vector[Stitching.Line]) = ujson.Arr.from(lines.map(_.json(s.at)))
    def count(key: String, n: Int) = Option.when(n > 0)(key -> ujson.Num(n))
    val earlier = strand.opening.map("opening" -> _.json(s.at)).toVector ++
      Option.when(strand.latest.nonEmpty)("latest" -> said(strand.latest)) ++
      count("not_shown", strand.left)
    ujson.Obj.from(
      Vector[(String, ujson.Value)]("new_message" -> s.message, "author" -> s.author) ++
        Option.when(earlier.nonEmpty)("earlier_exchange" -> ujson.Obj.from(earlier)) ++
        Vector("thread" -> said(s.thread.shown)) ++ count("thread_not_shown", s.thread.cut.size) ++
        s.sections.collect {
          case (section, text) if text.nonEmpty => section.key -> ujson.Str(text)
        }
    )
  }

  /** The words v1 ([[TriageQuestions.v1]]) is asked in: the kind question's instructions and
    * what each kind means, and each yes/no question's instructions. The keys the classifier
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

    /** The words v1 shipped with. */
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
}
