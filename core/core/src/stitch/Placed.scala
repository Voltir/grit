package grit.core.stitch

import grit.core.id.ConversationId
import grit.core.message.Usage
import grit.core.period.Probability

/** Why an exchange was offered. */
enum Offered {

  /** Its `rank` among the exchanges most recently spoken in, from 1. */
  case Recent(rank: Int)

  /** Its best BM25 `score` for the message's text. */
  case Lexical(score: Double)
}

/** What the classifier was shown for one first message: the `state` exactly as sent, each
  * offered exchange's root, why it was offered and the probability given it (`None` when the
  * answer did not read), and the `tuning` in force.
  */
final case class Seen(state: ujson.Value, offered: Vector[Seen.Offer], tuning: Tuning)

object Seen {
  final case class Offer(root: ConversationId, why: Offered, p: Option[Probability])
}

/** What became of a first message put to the classifier, with what it `seen`. Only
  * [[Stitching.place]] and [[StitchJson]] make one, so a [[Placed.Follows]] names a root that
  * was offered, in the message's room.
  */
sealed trait Placed {
  def seen: Seen
}

object Placed {

  /** It continues the exchange `root` began: the classifier's top choice, at `p`, by `model`,
    * at `usage`.
    */
  final case class Follows private[stitch] (
      root: ConversationId,
      p: Probability,
      seen: Seen,
      model: String,
      usage: Usage
  ) extends Placed

  /** It begins something new: `p` is what the classifier gave the likeliest exchange. */
  final case class Begins private[stitch] (p: Probability, seen: Seen, model: String, usage: Usage)
      extends Placed

  /** The classifier failed or did not read, for `why`: the message is not stitched. */
  final case class Unread private[stitch] (why: String, seen: Seen) extends Placed

  /** The link `placed` makes for `conversation`, whose first message it placed. */
  def link(conversation: ConversationId, placed: Placed): Option[Link] = placed match {
    case f: Follows => Some(Link(conversation, f.root))
    case _: Begins | _: Unread => None
  }
}
