package grit.core.period

import scala.concurrent.duration.*

/** How long a period may stay quiet before it lapses (`idle`), how long a closed period's raw
  * entries and workflow histories are kept (`retention`), and how long a closing is kept once
  * the next one replaces it, and a conversation once its last period closed (`ledger`).
  */
final case class Windows private (
    idle: FiniteDuration,
    retention: FiniteDuration,
    ledger: FiniteDuration
)

object Windows {

  /** The windows, or why not: each must be positive, and `ledger` at least `retention`. */
  def of(
      idle: FiniteDuration,
      retention: FiniteDuration,
      ledger: FiniteDuration
  ): Either[String, Windows] = {
    def positive(name: String, d: FiniteDuration) =
      Either.cond(d > Duration.Zero, (), s"$name must be positive")
    for {
      _ <- positive("idle", idle)
      _ <- positive("retention", retention)
      _ <- positive("ledger", ledger)
      // Load-bearing: a period's raw entries fall due before the closing and quiet
      // tombstones that wait on them (ADR 0014).
      _ <- Either.cond(ledger >= retention, (), "ledger must be at least retention")
    } yield new Windows(idle, retention, ledger)
  }

  /** 24 hours idle, 30 days' retention, a 180-day ledger. */
  val Default: Windows = new Windows(24.hours, 30.days, 180.days)
}
