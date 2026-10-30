package grit.app.config

import scala.concurrent.duration.FiniteDuration

import grit.core.period.{LifecycleSettings, Probability, Windows}
import grit.core.place.Weight as PlaceWeight

/** The lifecycle's settings as a person writes them: changed one at a time with `/set` until
  * grit next starts, and reported in one line.
  */
object Lifecycle {

  /** `raw` as a probability, written as a number from 0 to 1; or why it is none. */
  def probability(raw: String): Either[String, Probability] =
    raw.trim.toDoubleOption
      .flatMap(Probability.of)
      .toRight("not a probability: write a number from 0 to 1, as 0.8")

  /** `raw` as a place's weight, written as a number of at least 1; or why it is none. */
  def weightOf(raw: String): Either[String, PlaceWeight] =
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
    case Weight(to: PlaceWeight)

    /** `settings` with this change, or why the result breaks their rules. */
    def applied(settings: LifecycleSettings): Either[String, LifecycleSettings] = {
      val s = settings
      val w = s.windows
      this match {
        case Idle(to) =>
          Windows
            .of(to, w.retention, w.ledger)
            .flatMap(LifecycleSettings.of(_, s.balance, s.settle, s.resolveAt, s.asks, s.weight))
        case Retention(to) =>
          Windows
            .of(w.idle, to, w.ledger)
            .flatMap(LifecycleSettings.of(_, s.balance, s.settle, s.resolveAt, s.asks, s.weight))
        case Ledger(to) =>
          Windows
            .of(w.idle, w.retention, to)
            .flatMap(LifecycleSettings.of(_, s.balance, s.settle, s.resolveAt, s.asks, s.weight))
        case Balance(to) => LifecycleSettings.of(w, to, s.settle, s.resolveAt, s.asks, s.weight)
        case Settle(to) => LifecycleSettings.of(w, s.balance, to, s.resolveAt, s.asks, s.weight)
        case ResolveAt(to) => LifecycleSettings.of(w, s.balance, s.settle, to, s.asks, s.weight)
        case Asks(to) => LifecycleSettings.of(w, s.balance, s.settle, s.resolveAt, to, s.weight)
        case Weight(to) => LifecycleSettings.of(w, s.balance, s.settle, s.resolveAt, s.asks, to)
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
        "weight"
      )

    /** The change `text` writes: a name, then its value (`idle 3m`, `ledger 180d`, `resolve 0.9`,
      * `balance 300`, `weight 2`); or why it writes none.
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
    val weight = PlaceWeight.value(settings.weight)
    val weighted = if (weight.isWhole) weight.toLong.toString else weight.toString
    val drawing = s"draws on open periods and closings it may read, its own weighted $weighted"
    s"$asking; closes after ${written(w.idle)} idle; raw entries kept ${written(w.retention)}; " +
      s"closings kept ${written(w.ledger)} after the next; " +
      s"the balance holds ${settings.balance} bytes; $drawing"
  }

  /** `settings` as `/set` reports a change to them: that it holds until grit next starts,
    * first, since the status line cuts a long line short; then [[describe]]'s line.
    */
  def changed(settings: LifecycleSettings): String =
    "set until grit restarts, when its GRIT_ variables apply again: " + describe(settings)

  /** `d` in the largest of [[Durations]]' units that writes it whole. */
  def written(d: FiniteDuration): String = {
    val s = d.toSeconds
    Vector(86400L -> "d", 3600L -> "h", 60L -> "m")
      .collectFirst { case (unit, name) if s % unit == 0 && s > 0 => s"${s / unit}$name" }
      .getOrElse(s"${s}s")
  }
}
