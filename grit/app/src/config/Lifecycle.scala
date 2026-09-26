package grit.app.config

import scala.concurrent.duration.FiniteDuration

import grit.core.period.{LifecycleSettings, Probability, Windows}
import grit.core.place.{Locality, Scope as PlaceScope, Weight as PlaceWeight}

/** The lifecycle's settings as a person writes them: seeded from the environment on first
  * start, then changed one at a time with `/set`.
  */
object Lifecycle {

  private val IdleVar = "GRIT_IDLE"
  private val RetentionVar = "GRIT_RETENTION"
  private val LedgerVar = "GRIT_LEDGER"
  private val BalanceVar = "GRIT_BALANCE"
  private val SettleVar = "GRIT_SETTLE"
  private val ResolveAtVar = "GRIT_RESOLVE_AT"
  private val AsksVar = "GRIT_ASKS"
  private val ScopeVar = "GRIT_SCOPE"
  private val WeightVar = "GRIT_WEIGHT"

  /** The settings the environment seeds: `GRIT_IDLE`, `GRIT_RETENTION`, `GRIT_LEDGER` and
    * `GRIT_SETTLE` ([[Durations]]), `GRIT_BALANCE` and `GRIT_ASKS` (whole numbers) and `GRIT_RESOLVE_AT`
    * (a probability, as 0.8), `GRIT_SCOPE` (none, everywhere, or places separated by
    * spaces, as `fs:/home/you slack:team`) and `GRIT_WEIGHT` (a number of at least 1), each
    * unset one as [[LifecycleSettings.Default]] has it; or why they are none, naming the
    * variable.
    */
  def fromEnv(env: Map[String, String]): Either[String, LifecycleSettings] = {
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
        case Some(raw) => probability(raw).left.map(why => s"$ResolveAtVar: $why")
      }
      scope <- env
        .get(ScopeVar)
        .fold(Right(default.locality.scope))(PlaceScope.read(_).left.map(why => s"$ScopeVar: $why"))
      weight <- env.get(WeightVar) match {
        case None => Right(default.locality.weight)
        case Some(raw) => weightOf(raw).left.map(why => s"$WeightVar: $why")
      }
      windows <- Windows
        .of(idle, retention, ledger)
        .left
        .map(why => s"$IdleVar, $RetentionVar, $LedgerVar: $why")
      settings <- LifecycleSettings
        .of(windows, balance, settle, resolveAt, asks, Locality(scope, weight))
        .left
        .map(why => s"$BalanceVar, $SettleVar, $ResolveAtVar, $AsksVar: $why")
    } yield settings
  }

  private def probability(raw: String): Either[String, Probability] =
    raw.trim.toDoubleOption
      .flatMap(Probability.of)
      .toRight("not a probability: write a number from 0 to 1, as 0.8")

  private def weightOf(raw: String): Either[String, PlaceWeight] =
    raw.trim.toDoubleOption
      .toRight(s"a weight is a number of at least 1, not ${raw.trim}")
      .flatMap(PlaceWeight.of)

  /** One setting changed. */
  enum Change extends caps.Pure {
    case Idle(to: FiniteDuration)
    case Retention(to: FiniteDuration)
    case Ledger(to: FiniteDuration)
    case Balance(to: Int)
    case Settle(to: FiniteDuration)
    case ResolveAt(to: Probability)
    case Asks(to: Int)
    case Scope(to: PlaceScope)
    case Weight(to: PlaceWeight)

    /** `settings` with this change, or why the result breaks their rules. */
    def applied(settings: LifecycleSettings): Either[String, LifecycleSettings] = {
      val s = settings
      val w = s.windows
      this match {
        case Idle(to) =>
          Windows
            .of(to, w.retention, w.ledger)
            .flatMap(LifecycleSettings.of(_, s.balance, s.settle, s.resolveAt, s.asks, s.locality))
        case Retention(to) =>
          Windows
            .of(w.idle, to, w.ledger)
            .flatMap(LifecycleSettings.of(_, s.balance, s.settle, s.resolveAt, s.asks, s.locality))
        case Ledger(to) =>
          Windows
            .of(w.idle, w.retention, to)
            .flatMap(LifecycleSettings.of(_, s.balance, s.settle, s.resolveAt, s.asks, s.locality))
        case Balance(to) => LifecycleSettings.of(w, to, s.settle, s.resolveAt, s.asks, s.locality)
        case Settle(to) => LifecycleSettings.of(w, s.balance, to, s.resolveAt, s.asks, s.locality)
        case ResolveAt(to) => LifecycleSettings.of(w, s.balance, s.settle, to, s.asks, s.locality)
        case Asks(to) => LifecycleSettings.of(w, s.balance, s.settle, s.resolveAt, to, s.locality)
        case Scope(to) =>
          LifecycleSettings.of(
            w,
            s.balance,
            s.settle,
            s.resolveAt,
            s.asks,
            s.locality.copy(scope = to)
          )
        case Weight(to) =>
          LifecycleSettings.of(
            w,
            s.balance,
            s.settle,
            s.resolveAt,
            s.asks,
            s.locality.copy(weight = to)
          )
      }
    }
  }

  object Change {

    /** The names `/set` takes, in the order its help lists them. */
    val Names: Vector[String] =
      Vector(
        "idle",
        "settle",
        "resolve",
        "asks",
        "retention",
        "ledger",
        "balance",
        "scope",
        "weight"
      )

    /** The change `text` writes: a name, then its value (`idle 3m`, `ledger 180d`, `resolve 0.9`,
      * `balance 300`, `scope fs:/home/you slack:team`, `scope none`, `weight 2`); or why it
      * writes none. A scope is places separated by spaces, so a path holding a space cannot
      * be written here.
      */
    def parse(text: String): Either[String, Change] = {
      val (name, value) = text.trim.span(_ != ' ')
      def duration(make: FiniteDuration -> Change): Either[String, Change] =
        if (value.isBlank) Left(s"$name takes a duration, as 30s or 3m")
        else Durations.read(value).map(make).left.map(why => s"$name: $why")
      def whole(make: Int -> Change): Either[String, Change] =
        value.trim.toIntOption.filter(_ >= 0).map(make).toRight(s"$name takes a whole number")
      name match {
        case "idle" => duration(Idle(_))
        case "retention" => duration(Retention(_))
        case "ledger" => duration(Ledger(_))
        case "settle" => duration(Settle(_))
        case "balance" => whole(Balance(_))
        case "asks" => whole(Asks(_))
        case "resolve" => probability(value).map(ResolveAt(_)).left.map(why => s"$name: $why")
        case "scope" =>
          if (value.isBlank) Left(s"$name takes none, everywhere, or places such as fs:/home/you")
          else PlaceScope.read(value).map(Scope(_)).left.map(why => s"$name: $why")
        case "weight" => weightOf(value).map(Weight(_)).left.map(why => s"$name: $why")
        case other => Left(s"no setting $other: ${Names.init.mkString(", ")} or ${Names.last}")
      }
    }
  }

  /** `settings` in one line, as `/set` reports them. */
  def describe(settings: LifecycleSettings): String = {
    val w = settings.windows
    val asking =
      if (Probability.value(settings.resolveAt) >= 1.0) "never asks whether anyone is waiting"
      else
        s"after ${written(settings.settle)} quiet, asks whether anyone is waiting (at most " +
          s"${settings.asks} times) and closes when nobody is at " +
          s"${Probability.value(settings.resolveAt)} or more"
    val l = settings.locality
    val weight = PlaceWeight.value(l.weight)
    val weighted = if (weight.isWhole) weight.toLong.toString else weight.toString
    val drawing =
      if (l.scope.prefixes.isEmpty) "draws on no other place"
      else if (l.scope == PlaceScope.Everywhere)
        s"draws on open periods everywhere, its own weighted $weighted"
      else s"draws on open periods in ${l.scope.written}, its own weighted $weighted"
    s"$asking; closes after ${written(w.idle)} idle; raw entries kept ${written(w.retention)}; " +
      s"closings kept ${written(w.ledger)} after the next; " +
      s"the balance holds ${settings.balance} bytes; $drawing"
  }

  /** `d` in the largest of [[Durations]]' units that writes it whole. */
  def written(d: FiniteDuration): String = {
    val s = d.toSeconds
    Vector(86400L -> "d", 3600L -> "h", 60L -> "m")
      .collectFirst { case (unit, name) if s % unit == 0 && s > 0 => s"${s / unit}$name" }
      .getOrElse(s"${s}s")
  }
}
