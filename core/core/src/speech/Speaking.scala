package grit.core.speech

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.period.Probability
import grit.core.spend.DailyCap
import grit.core.triage.Gate

/** At most `count` in any window of `per`. */
final case class Rate private[speech] (count: Int, per: FiniteDuration)

object Rate {

  /** `count` per `per`; `None` unless `count` is at least 1 and `per` is positive. */
  def of(count: Int, per: FiniteDuration): Option[Rate] =
    Option.when(count >= 1 && per > Duration.Zero)(Rate(count, per))
}

/** Whether grit speaks where it was not addressed ([[grit.core.store.Payload.Heard]]). */
enum Speaking {

  /** Never; nothing is drafted. */
  case Off

  /** Drafts and judges as [[Within]] would, and never posts. */
  case Shadow(limits: Limits)

  /** Posts a draft that passes `limits` ([[Speech.decide]], [[Speech.post]]). */
  case Within(limits: Limits)
}

/** What a heard message and its draft must pass. A heard message: said at most `fresh` before
  * the decision; triage's answers passing `drafts` ([[Gate.check]]; a gate those answers
  * cannot decide holds the message, `Unasked`); at most `thread` posts per thread (its
  * [[ReplyTo.within]], wherever that thread's posts were recorded; its conversation where it or
  * a post names no thread), `room` per room ([[grit.core.store.Origin.room]]) and `deployment` in all; today's speech
  * spend under `spend`. A draft: its weakest judged score ([[Judged.score]]) at or above
  * `postAt`, and nobody having spoken since its root. Where it goes: its message's thread, or
  * its room's top level when `toRoom` places it there ([[Venue]]); `None` is always the thread.
  */
final case class Limits(
    drafts: Gate,
    postAt: Probability,
    fresh: FiniteDuration,
    thread: Rate,
    room: Rate,
    deployment: Rate,
    spend: DailyCap,
    toRoom: Option[ToRoom] = None
) {

  /** The earliest instant any of its rates reaches back to from `now`: what a decision at
    * `now` reads of the ledger ([[SpeechStore.spoken]]).
    */
  def from(now: Instant): Instant =
    now.minusNanos(
      (Vector(thread, room, deployment) ++ toRoom.map(_.rate))
        .foldLeft(0L)((m, r) => math.max(m, r.per.toNanos))
    )
}

object Limits {

  /** The limits a deployment starts on, with `drafts` its gate over live triage's answers and
    * `spend` a day of speech: `postAt` 0.50, fresh for 10 minutes, 1 post per thread in 6
    * hours, 2 per room in an hour and 10 in all in 24 hours.
    */
  def suggested(spend: DailyCap, drafts: Gate): Limits = Limits(
    drafts,
    Probability.clamped(0.50),
    10.minutes,
    Rate(1, 6.hours),
    Rate(2, 1.hour),
    Rate(10, 24.hours),
    spend
  )
}

/** When an unprompted draft is posted at its room's top level instead of its message's thread
  * ([[Speech.decide]]): its message has a room address ([[ReplyTo.room]]), triage's answers and
  * the cues pass `gate` ([[Gate.check]]; failing or unread is the thread), and fewer than
  * `rate.count` of its room's posts there were decided within `rate.per`, counted again when
  * the draft is posted ([[Speech.placing]]). It never holds a message.
  */
final case class ToRoom(gate: Gate, rate: Rate)

object ToRoom {

  /** The placement a deployment starts on: [[grit.core.triage.Tags.V7.toRoom]], and 1 post at
    * a room's top level in 2 hours.
    */
  val Suggested: ToRoom = ToRoom(grit.core.triage.Tags.V7.toRoom, Rate(1, 2.hours))
}
