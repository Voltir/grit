package grit.slack.edge

import scala.collection.immutable.VectorMap

import grit.core.id.{ConversationId, PluginName}
import grit.core.review.{DocumentReview, ReviewWord, Reviews}
import grit.core.visibility.Label
import grit.prose.form.Doc
import grit.slack.event.{ChannelId, Ts}

/** One kind of review the Slack edge prompts for: its `reviews`; how its log and its post's tag
  * name a subject (`name`); the Slack message a subject's prompt is of, if any (`message`); the
  * label a subject is kept at, rechecked against the review's place as its prompt is posted and
  * kept, `None` for one whose prompt holds no text of it (`label`); its prompt as prose, given
  * its message's channel as a person reads it and permalink when it is of one, and how a
  * conversation links, `None` where it does not (`doc`); and the emoji its rater answers with,
  * as Slack names them, each the verdict it keeps (`verdicts`), added to each prompt in order.
  */
final case class Prompting[S <: caps.Pure, V <: caps.Pure](
    reviews: Reviews[S, V],
    name: S -> String,
    message: S -> Prompting.Of,
    label: S -> Option[Label],
    doc: (S, Option[(String, String)], ConversationId => Option[String]) -> Doc[Nothing],
    verdicts: VectorMap[String, V]
)

object Prompting {

  /** What Slack message a subject's prompt is of. */
  enum Of extends caps.Pure {

    /** The message `ts` in `channel` of this edge's own workspace. */
    case Message(channel: ChannelId, ts: Ts)

    /** None: its prompt links no message. */
    case NoMessage

    /** A message this edge cannot link, as of another workspace or no Slack: never posted, and
      * `said` logged each pass when given.
      */
    case Elsewhere(said: Option[String])
  }

  /** A review beside triage's, made as its edge opens from the reviews of each plugin's
    * documents put up ([[grit.core.edge.EdgeStores.documentReviews]]).
    */
  type OverDocuments = (PluginName -> Reviews[DocumentReview, ReviewWord]) -> Prompting[?, ?]

  /** `emoji` as Slack names it, a skin tone after `::` dropped: how [[Prompting.verdicts]] is
    * keyed.
    */
  def named(emoji: String): String =
    emoji.indexOf("::skin-tone-") match {
      case -1 => emoji
      case i => emoji.take(i)
    }
}
