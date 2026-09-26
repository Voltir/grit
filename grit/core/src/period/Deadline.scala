package grit.core.period

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.core.id.TurnSeq

/** When a period closes, and the reason it will close for. */
final case class Due(at: Instant, reason: CloseReason)

/** When a period closes, and when the classifier is asked whether anyone is waiting: the one
  * definition of each, over an open period's newest `activity`, its newest turn `last`, its
  * latest verdict and the settings in force.
  */
object Deadline {

  /** When the period closes: at the verdict's time, Resolved with its probability of
    * nobody waiting, when the verdict came after `activity`, is about `last`, and weighed
    * nobody at or above `settings.resolveAt` (which, at 1, no verdict does); otherwise the idle
    * window after `activity`, Lapsed.
    */
  def of(
      activity: Instant,
      last: TurnSeq,
      verdict: Option[Verdict],
      settings: LifecycleSettings
  ): Due =
    current(activity, last, verdict)
      .collect { case Verdict(at, _, Judgement.Weighed(nobody, _, _, _)) =>
        (at, nobody)
      }
      .filter((_, nobody) => on(settings) && nobody >= settings.resolveAt)
      .fold(Due(plus(activity, settings.windows.idle), CloseReason.Lapsed))((at, nobody) =>
        Due(at, CloseReason.Resolved(nobody))
      )

  /** When the classifier is asked about the period: the settle window after `activity`,
    * unless it has already been asked since then about `last`, it has been asked
    * `settings.asks` times (`asked`), or `settings.resolveAt` is 1; `None` then.
    */
  def ask(
      activity: Instant,
      last: TurnSeq,
      verdict: Option[Verdict],
      asked: Int,
      settings: LifecycleSettings
  ): Option[Instant] =
    Option.when(
      on(settings) && asked < settings.asks && current(activity, last, verdict).isEmpty
    )(plus(activity, settings.settle))

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
