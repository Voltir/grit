package grit.core.period

import java.time.Instant

import grit.core.id.{CloseRef, PeriodRef, SettleRef, TurnSeq}

/** An open period as its deadline sees it: its `newest` activity (the latest of its opening
  * and its entries' times), its `last` turn (its first while it has no entries), its
  * latest `verdict`, and how many verdicts it has had (`asked`).
  */
final case class Activity(
    period: PeriodRef,
    newest: Instant,
    last: TurnSeq,
    verdict: Option[Verdict],
    asked: Int
) {

  /** When it closes under `settings` ([[Deadline.of]]). */
  def due(settings: LifecycleSettings): Due = Deadline.of(newest, last, verdict, settings)

  /** The attempt to close it on its deadline under `settings`, as it stands. */
  def attempt(settings: LifecycleSettings): CloseRef = CloseRef(period, last, due(settings).at)

  /** When the classifier is to be asked about it under `settings` ([[Deadline.ask]]). */
  def asks(settings: LifecycleSettings): Option[Instant] =
    Deadline.ask(newest, last, verdict, asked, settings)

  /** The question about it as it stands, quiet since its newest activity. */
  def question: SettleRef = SettleRef(period, last, newest)
}
