package grit.core.period

import scala.concurrent.duration.*

/** The lifecycle's settings in force: the `windows` periods lapse and are purged by; how
  * many of a conversation's newest closing entries open each turn's window (`closings`);
  * how long a period is quiet before the classifier is asked whether it is finished
  * (`settle`); the probability of finished at which its answer closes the period
  * (`finishedAt`; at 1, no period is asked and none closes but by lapsing); and how many
  * times one period is asked at most (`asks`).
  */
final case class LifecycleSettings private (
    windows: Windows,
    closings: Int,
    settle: FiniteDuration,
    finishedAt: Probability,
    asks: Int
)

object LifecycleSettings {

  /** The settings, or why not: `closings` must not be negative, `settle` must be positive
    * and shorter than the idle window, `finishedAt` above 0, and `asks` at least 1.
    */
  def of(
      windows: Windows,
      closings: Int,
      settle: FiniteDuration,
      finishedAt: Probability,
      asks: Int
  ): Either[String, LifecycleSettings] =
    for {
      _ <- Either.cond(closings >= 0, (), "closings must not be negative")
      _ <- Either.cond(settle > Duration.Zero, (), "settle must be positive")
      _ <- Either.cond(settle < windows.idle, (), "settle must be shorter than idle")
      _ <- Either.cond(finishedAt > Probability.Zero, (), "finishedAt must be above 0")
      _ <- Either.cond(asks >= 1, (), "asks must be at least 1")
    } yield new LifecycleSettings(windows, closings, settle, finishedAt, asks)

  /** [[Windows.Default]], 3 closing entries, an hour to settle, finished at 0.8, 3 asks. */
  val Default: LifecycleSettings =
    new LifecycleSettings(
      Windows.Default,
      3,
      1.hour,
      // 0.8 is a probability, so the fallback is never taken.
      Probability.of(0.8).getOrElse(Probability.One),
      3
    )
}
