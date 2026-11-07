package grit.core.period

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.core.id.TurnSeq

/** When a period closes, the reason it will close for, and the turn its closing runs to. */
final case class Due(at: Instant, reason: CloseReason, last: TurnSeq)

/** When a period closes, and when the classifier is asked whether anyone is waiting: the one
  * definition of each, over an open period's [[Activity]] and the settings in force.
  */
object Deadline {

  /** When the period closes, at its `last` turn unless it rolls.
    *
    * A period that settles: at the verdict's time, Resolved with its probability of nobody
    * waiting, when the verdict came after its newest activity, is about its `last` turn, and
    * weighed nobody at or above `settings.resolveAt` (which, at 1, no verdict does); otherwise
    * the idle window after its newest activity, Lapsed.
    *
    * A period that rolls, Rolled, never by a verdict nor idleness: at the turn whose characters
    * brought what was said in it to `settings.rolling.chars`, when that turn began; otherwise
    * `settings.rolling.most` after it opened, at the newest turn begun by then.
    */
  def of(activity: Activity, settings: LifecycleSettings): Due = activity.pace match {
    case Pace.Rolls(opened, spoken) =>
      val r = settings.rolling
      val reached = spoken
        .scanLeft(0L)(_ + _.chars)
        .drop(1)
        .zip(spoken)
        .collectFirst { case (sum, s) if sum >= r.chars => s }
      reached.fold {
        val most = plus(opened, r.most)
        val last = spoken.takeWhile(!_.at.isAfter(most)).lastOption.fold(activity.last)(_.turn)
        Due(most, CloseReason.Rolled, last)
      }(s => Due(s.at, CloseReason.Rolled, s.turn))
    case Pace.Settles =>
      current(activity.newest, activity.last, activity.verdict)
        .collect { case Verdict(at, _, Judgement.Weighed(nobody, _, _, _)) =>
          (at, nobody)
        }
        .filter((_, nobody) => on(settings) && nobody >= settings.resolveAt)
        .fold(
          Due(plus(activity.newest, settings.windows.idle), CloseReason.Lapsed, activity.last)
        )((at, nobody) => Due(at, CloseReason.Resolved(nobody), activity.last))
  }

  /** When the classifier is asked about the period: the settle window after its newest
    * activity, unless it has already been asked since then about its `last` turn, it has been
    * asked `settings.asks` times, or `settings.resolveAt` is 1; `None` then, and always for a
    * period that rolls.
    */
  def ask(activity: Activity, settings: LifecycleSettings): Option[Instant] =
    Option.when(
      activity.pace == Pace.Settles && on(settings) && activity.asked < settings.asks &&
        current(activity.newest, activity.last, activity.verdict).isEmpty
    )(plus(activity.newest, settings.settle))

  /** `verdict` when it judged the period as it stands: after `activity`, about `last`. */
  private def current(
      activity: Instant,
      last: TurnSeq,
      verdict: Option[Verdict]
  ): Option[Verdict] =
    verdict.filter(v => v.at.isAfter(activity) && v.last == last)

  /** Whether a verdict can close a period at all. */
  private def on(settings: LifecycleSettings): Boolean = Probability.One > settings.resolveAt

  private def plus(t: Instant, d: FiniteDuration): Instant = t.plusMillis(d.toMillis)
}
