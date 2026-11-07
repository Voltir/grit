package grit.core.period

import java.time.Instant

import grit.core.id.{CloseRef, PeriodRef, SettleRef, TurnSeq}

/** An open period as its deadline sees it: its `newest` activity (the latest of its opening
  * and its entries' times, a draft's left out: [[grit.core.store.Payload.Draft]]), its `last` turn (its first while it has no entries), its
  * latest `verdict`, how many verdicts it has had (`asked`), and how its deadline is kept
  * (`pace`).
  */
final case class Activity(
    period: PeriodRef,
    newest: Instant,
    last: TurnSeq,
    verdict: Option[Verdict],
    asked: Int,
    pace: Pace
) {

  /** When it closes under `settings` ([[Deadline.of]]). */
  def due(settings: LifecycleSettings): Due = Deadline.of(this, settings)

  /** The attempt to close it on its deadline under `settings`, as it stands. */
  def attempt(settings: LifecycleSettings): CloseRef = {
    val d = due(settings)
    CloseRef(period, d.last, d.at)
  }

  /** When the classifier is to be asked about it under `settings` ([[Deadline.ask]]). */
  def asks(settings: LifecycleSettings): Option[Instant] = Deadline.ask(this, settings)

  /** The question about it as it stands, quiet since its newest activity. */
  def question: SettleRef = SettleRef(period, last, newest)
}

/** How an open period's deadline is kept. */
enum Pace {

  /** Closed once the classifier judges nobody waiting, or after the idle window. */
  case Settles

  /** Rolled at a budget of what was said in it ([[Rolling]]), its conversation being one that
    * rolls ([[grit.core.store.Origin.rolls]]): it opened `opened`, and `spoken` is each of its
    * turns, oldest first.
    */
  case Rolls(opened: Instant, spoken: Vector[Spoken])
}

/** A turn of a rolling period: when its first entry was written, and how many characters were
  * said in it ([[grit.core.store.Payload.spoken]], summed over its entries).
  */
final case class Spoken(turn: TurnSeq, at: Instant, chars: Long)
