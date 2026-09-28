package grit.app.config

import java.time.ZoneId

import grit.core.spend.{Budget, DailyCap}

/** How much grit may spend a day, as a person sets it: `GRIT_DAILY_USD`. */
object Budgets {

  val CapVar = "GRIT_DAILY_USD"

  /** `grit serve`'s cap when `GRIT_DAILY_USD` is unset: $1.00 a day. */
  val ServeDefault: Option[DailyCap] = DailyCap.of("1.00").toOption

  /** The budget `env` sets, its days beginning in `zone`: the cap `GRIT_DAILY_USD` gives, else
    * `default`; why not, naming the variable, when it is not a number of dollars above zero.
    */
  def fromEnv(
      env: Map[String, String],
      zone: ZoneId,
      default: Option[DailyCap]
  ): Either[String, Budget] =
    env
      .get(CapVar)
      .fold(Right(default): Either[String, Option[DailyCap]])(raw =>
        DailyCap.of(raw).map(Some(_)).left.map(why => s"$CapVar: $why")
      )
      .map(Budget(zone, _))
}
