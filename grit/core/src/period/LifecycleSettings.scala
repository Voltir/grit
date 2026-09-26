package grit.core.period

import scala.concurrent.duration.*

/** The lifecycle's settings in force: the `windows` periods lapse and are purged by; the cap,
  * in UTF-8 bytes of its lines' text, a closing's balance is held to (`balance`,
  * [[Balance.fit]]); how long a period is quiet before the classifier is asked whether it is
  * finished (`settle`); the probability of finished at which its answer closes the period
  * (`finishedAt`; at 1, no period is asked and none closes but by lapsing); and how many
  * times one period is asked at most (`asks`).
  */
final case class LifecycleSettings private (
    windows: Windows,
    balance: Int,
    settle: FiniteDuration,
    finishedAt: Probability,
    asks: Int
)

object LifecycleSettings {

  /** The settings, or why not: `balance` must be at least 1, `settle` must be positive
    * and shorter than the idle window, `finishedAt` above 0, and `asks` at least 1.
    */
  def of(
      windows: Windows,
      balance: Int,
      settle: FiniteDuration,
      finishedAt: Probability,
      asks: Int
  ): Either[String, LifecycleSettings] =
    for {
      _ <- Either.cond(balance >= 1, (), "balance must be at least 1")
      _ <- Either.cond(settle > Duration.Zero, (), "settle must be positive")
      _ <- Either.cond(settle < windows.idle, (), "settle must be shorter than idle")
      _ <- Either.cond(finishedAt > Probability.Zero, (), "finishedAt must be above 0")
      _ <- Either.cond(asks >= 1, (), "asks must be at least 1")
    } yield new LifecycleSettings(windows, balance, settle, finishedAt, asks)

  /** [[Windows.Default]], a 4096-byte balance (about a thousand tokens), an hour to settle,
    * finished at 0.8, 3 asks.
    */
  val Default: LifecycleSettings =
    new LifecycleSettings(
      Windows.Default,
      4096,
      1.hour,
      // 0.8 is a probability, so the fallback is never taken.
      Probability.of(0.8).getOrElse(Probability.One),
      3
    )
}
