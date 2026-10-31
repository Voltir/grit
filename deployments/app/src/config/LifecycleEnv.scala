package grit.app.config

import grit.chat.settings.{Durations, Lifecycle}
import grit.core.either.Traversal.traverse
import grit.core.period.LifecycleSettings

/** The lifecycle's settings as the environment declares them, read on every start; `/set`
  * changes them until the next ([[Lifecycle]]).
  */
object LifecycleEnv {

  /** Each variable with the `/set` name of the setting it declares, in the order a refusal names
    * them.
    */
  private val Settings: Vector[(String, String)] = Vector(
    "GRIT_IDLE" -> "idle",
    "GRIT_RETENTION" -> "retention",
    "GRIT_LEDGER" -> "ledger",
    "GRIT_SETTLE" -> "settle",
    "GRIT_BALANCE" -> "balance",
    "GRIT_ASKS" -> "asks",
    "GRIT_RESOLVE_AT" -> "resolve",
    "GRIT_WEIGHT" -> "weight"
  )
  private val ScopeVar = "GRIT_SCOPE"

  /** Why `GRIT_SCOPE` set is refused: a deployment that set it to fence recall would otherwise
    * draw on every place without a word.
    */
  private val ScopeRefused =
    s"$ScopeVar is no longer read: a window draws on every place its turn may read; unset it"

  /** The settings the environment declares: `GRIT_IDLE`, `GRIT_RETENTION`, `GRIT_LEDGER` and
    * `GRIT_SETTLE` ([[Durations]]), `GRIT_BALANCE` and `GRIT_ASKS` (whole numbers) and `GRIT_RESOLVE_AT`
    * (a probability, as 0.8) and `GRIT_WEIGHT` (a number of at least 1), each unset one as
    * [[LifecycleSettings.Default]] has it, each read as `/set` reads its value
    * ([[Lifecycle.Change.of]]); or why they are none, naming the variable, or every one declared
    * when together they break the settings' rules. Refused
    * while `GRIT_SCOPE` is set: a window draws on every place its turn may read.
    */
  def of(env: Map[String, String]): Either[String, LifecycleSettings] = {
    val declared = Settings.flatMap((variable, name) => env.get(variable).map((variable, name, _)))
    for {
      _ <- env.get(ScopeVar).fold(Right(()))(_ => Left(ScopeRefused))
      changes <- traverse(declared)((variable, name, raw) =>
        Lifecycle.Change.of(name, raw, variable)
      )
      settings <- Lifecycle.Change
        .applying(changes, LifecycleSettings.Default)
        .left
        .map(why => s"${declared.map(_._1).mkString(", ")}: $why")
    } yield settings
  }
}
