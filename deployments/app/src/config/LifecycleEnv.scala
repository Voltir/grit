package grit.app.config

import scala.concurrent.duration.FiniteDuration

import grit.core.period.{LifecycleSettings, Windows}

/** The lifecycle's settings as the environment declares them, read on every start; `/set`
  * changes them until the next ([[Lifecycle]]).
  */
object LifecycleEnv {

  private val IdleVar = "GRIT_IDLE"
  private val RetentionVar = "GRIT_RETENTION"
  private val LedgerVar = "GRIT_LEDGER"
  private val BalanceVar = "GRIT_BALANCE"
  private val SettleVar = "GRIT_SETTLE"
  private val ResolveAtVar = "GRIT_RESOLVE_AT"
  private val AsksVar = "GRIT_ASKS"
  private val ScopeVar = "GRIT_SCOPE"
  private val WeightVar = "GRIT_WEIGHT"

  /** Why `GRIT_SCOPE` set is refused: a deployment that set it to fence recall would otherwise
    * draw on every place without a word.
    */
  private val ScopeRefused =
    s"$ScopeVar is no longer read: a window draws on every place its turn may read; unset it"

  /** The settings the environment declares: `GRIT_IDLE`, `GRIT_RETENTION`, `GRIT_LEDGER` and
    * `GRIT_SETTLE` ([[Durations]]), `GRIT_BALANCE` and `GRIT_ASKS` (whole numbers) and `GRIT_RESOLVE_AT`
    * (a probability, as 0.8) and `GRIT_WEIGHT` (a number of at least 1), each unset one as
    * [[LifecycleSettings.Default]] has it; or why they are none, naming the variable. Refused
    * while `GRIT_SCOPE` is set: a window draws on every place its turn may read.
    */
  def of(env: Map[String, String]): Either[String, LifecycleSettings] = {
    val default = LifecycleSettings.Default
    def duration(variable: String, otherwise: FiniteDuration): Either[String, FiniteDuration] =
      env
        .get(variable)
        .fold(Right(otherwise))(Durations.read(_).left.map(why => s"$variable: $why"))
    def whole(variable: String, otherwise: Int): Either[String, Int] =
      env.get(variable) match {
        case None => Right(otherwise)
        case Some(raw) => raw.trim.toIntOption.toRight(s"$variable is not a whole number")
      }
    for {
      idle <- duration(IdleVar, default.windows.idle)
      retention <- duration(RetentionVar, default.windows.retention)
      ledger <- duration(LedgerVar, default.windows.ledger)
      settle <- duration(SettleVar, default.settle)
      balance <- whole(BalanceVar, default.balance)
      asks <- whole(AsksVar, default.asks)
      resolveAt <- env.get(ResolveAtVar) match {
        case None => Right(default.resolveAt)
        case Some(raw) => Lifecycle.probability(raw).left.map(why => s"$ResolveAtVar: $why")
      }
      _ <- env.get(ScopeVar).fold(Right(()))(_ => Left(ScopeRefused))
      weight <- env.get(WeightVar) match {
        case None => Right(default.weight)
        case Some(raw) => Lifecycle.weightOf(raw).left.map(why => s"$WeightVar: $why")
      }
      windows <- Windows
        .of(idle, retention, ledger)
        .left
        .map(why => s"$IdleVar, $RetentionVar, $LedgerVar: $why")
      settings <- LifecycleSettings
        .of(windows, balance, settle, resolveAt, asks, weight)
        .left
        .map(why => s"$BalanceVar, $SettleVar, $ResolveAtVar, $AsksVar: $why")
    } yield settings
  }
}
