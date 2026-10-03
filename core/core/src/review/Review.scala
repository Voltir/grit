package grit.core.review

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.{ConversationId, EntryId, QuestionName, ShadowName, ShortHash}
import grit.core.speech.{Outcome, Silence}

/** Live triage's decision on a heard message, settled: held, or drafted and what became of
  * the draft.
  */
enum Settled {
  case Held(why: Silence)
  case Drafted(outcome: Outcome)
}

/** How a shadow's gate and live triage's compare on a heard message: only the shadow's
  * drafts, only live's does, both do, or neither does.
  */
enum Reason {
  case ShadowOnly, LiveOnly, Both, Neither
}

/** A heard message a review may pick: its entry and conversation, when it was said, live's
  * settled decision on it, and what `shadow` answered of it, under its questions' names.
  */
final case class Candidate(
    entry: EntryId,
    conversation: ConversationId,
    said: Instant,
    live: Settled,
    shadow: ShadowName,
    answers: VectorMap[QuestionName, Answer]
)

/** What a pick round made of a candidate. */
enum Considered {

  /** Picked for a prompt. */
  case Picked(reason: Reason)

  /** Not picked: its reason's share of the day was used, or, for `Neither`, it is outside the
    * sample.
    */
  case Passed(reason: Reason)

  /** Not picked: live's gate was never reached, or the shadow's gate could not read its
    * answers.
    */
  case Unread
}

object Review {

  /** Whether live triage's gate passed: `Some(true)` for a draft, or a hold after the gate
    * (an unanswered earlier turn, a thread, room or deployment rate, the speech cap, the
    * budget); `Some(false)` for a hold at it (`Gated`, naming someone else); `None` when it was
    * never reached (speaking off, no address, stale, untagged, `Unasked`).
    */
  def gated(live: Settled): Option[Boolean] = live match {
    case Settled.Drafted(_) => Some(true)
    case Settled.Held(why) =>
      why match {
        case Silence.Off | Silence.NoAddress | Silence.Stale(_) | Silence.Unweighed(_) |
            Silence.Unasked(_) =>
          None
        case Silence.Gated(_, _) | Silence.AskedOf(_) => Some(false)
        case Silence.Unanswered(_) | Silence.Thread(_) | Silence.Room(_) | Silence.Deployment(_) |
            Silence.OverSpeechCap(_, _) | Silence.OverBudget =>
          Some(true)
      }
  }

  /** What a pick round makes of each of `candidates`, in their order, under `reviewing`,
    * `drafts` being the shadow's gate and `earlier` the reasons of the messages picked earlier
    * the same day. Every `ShadowOnly` and `Both` is picked. `LiveOnly` and `Neither` share
    * `perDay`, its slots dealt to them in turn from `LiveOnly` (so `LiveOnly`'s share is
    * `perDay` halved rounding up, `Neither`'s rounding down); each picks its earliest said
    * (ties by entry id) until its share, less those of `earlier`, is used. A `Neither` is picked
    * only in the sample: one in `sampleOneIn` by a hash of its entry id, the same in every
    * round and process.
    */
  def pick(
      candidates: Vector[Candidate],
      drafts: VectorMap[QuestionName, Answer] -> Option[Boolean],
      earlier: Vector[Reason],
      reviewing: Reviewing
  ): Vector[(Candidate, Considered)] = {
    val reasons: Vector[Option[Reason]] = candidates.map { c =>
      for {
        live <- gated(c.live)
        shadow <- drafts(c.answers)
      } yield (shadow, live) match {
        case (true, false) => Reason.ShadowOnly
        case (false, true) => Reason.LiveOnly
        case (true, true) => Reason.Both
        case (false, false) => Reason.Neither
      }
    }
    def share(reason: Reason): Int = reason match {
      case Reason.LiveOnly => (reviewing.perDay + 1) / 2
      case Reason.Neither => reviewing.perDay / 2
      case Reason.ShadowOnly | Reason.Both => 0
    }
    def eligible(c: Candidate, reason: Reason): Boolean = reason match {
      case Reason.Neither => sampled(c.entry, reviewing.sampleOneIn)
      case Reason.LiveOnly | Reason.ShadowOnly | Reason.Both => true
    }
    val capped: Set[EntryId] = Vector(Reason.LiveOnly, Reason.Neither).flatMap { r =>
      val left = math.max(0, share(r) - earlier.count(_ == r))
      candidates
        .zip(reasons)
        .collect { case (c, Some(`r`)) if eligible(c, r) => c }
        .sortBy(c => (c.said, EntryId.value(c.entry)))
        .take(left)
        .map(_.entry)
    }.toSet
    candidates.zip(reasons).map {
      case (c, None) => (c, Considered.Unread)
      case (c, Some(r @ (Reason.ShadowOnly | Reason.Both))) => (c, Considered.Picked(r))
      case (c, Some(r)) =>
        (c, if (capped.contains(c.entry)) Considered.Picked(r) else Considered.Passed(r))
    }
  }

  /** Whether `entry` is in a one-in-`oneIn` sample: its short hash, as an unsigned number, a
    * multiple of `oneIn`.
    */
  private def sampled(entry: EntryId, oneIn: Int): Boolean =
    java.lang.Long.remainderUnsigned(
      java.lang.Long.parseUnsignedLong(ShortHash.of(EntryId.value(entry)), 16),
      oneIn.toLong
    ) == 0L
}
