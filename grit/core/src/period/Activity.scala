package grit.core.period

import java.time.Instant

import grit.core.id.{CloseRef, PeriodRef, TurnSeq}

/** An open period as its deadline sees it: its `newest` activity (the later of its opening
  * and its newest entry's time), its `last` turn (its first while it has no entries), and
  * when someone `signalled` it done.
  */
final case class Activity(
    period: PeriodRef,
    newest: Instant,
    last: TurnSeq,
    signalled: Option[Instant]
) {

  /** When it closes under `windows` ([[Deadline.of]]). */
  def due(windows: Windows): Due = Deadline.of(newest, signalled, windows)

  /** The attempt to close it as it stands. */
  def attempt: CloseRef = CloseRef(period, last)

  /** Its signal once someone says it is done `at`: a signal given after its newest activity
    * stands, so a repeat changes nothing; otherwise `at`.
    */
  def signal(at: Instant): Instant = signalled.filter(_.isAfter(newest)).getOrElse(at)
}
