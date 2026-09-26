package grit.core.period

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

/** When a period closes, and the reason it will close for. */
final case class Due(at: Instant, reason: CloseReason)

object Deadline {

  /** When a period whose newest activity was at `activity` closes under `windows`:
    * `signalled + grace`, Resolved, when someone signalled after `activity`; otherwise
    * `activity + idle`, Lapsed.
    */
  def of(activity: Instant, signalled: Option[Instant], windows: Windows): Due =
    signalled.filter(_.isAfter(activity)) match {
      case Some(s) => Due(plus(s, windows.grace), CloseReason.Resolved)
      case None => Due(plus(activity, windows.idle), CloseReason.Lapsed)
    }

  private def plus(t: Instant, d: FiniteDuration): Instant = t.plusMillis(d.toMillis)
}
