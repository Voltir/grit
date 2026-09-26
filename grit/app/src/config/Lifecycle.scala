package grit.app.config

import scala.concurrent.duration.FiniteDuration

import grit.core.period.{LifecycleSettings, Probability, Windows}

/** The lifecycle's settings as a person writes them: seeded from the environment on first
  * start, then changed one at a time with `/set`.
  */
object Lifecycle {

  private val IdleVar = "GRIT_IDLE"
  private val RetentionVar = "GRIT_RETENTION"
  private val ClosingsVar = "GRIT_WINDOW_K"
  private val SettleVar = "GRIT_SETTLE"
  private val FinishedAtVar = "GRIT_FINISHED_AT"
  private val AsksVar = "GRIT_ASKS"

  /** The settings the environment seeds: `GRIT_IDLE`, `GRIT_RETENTION` and `GRIT_SETTLE`
    * ([[Durations]]), `GRIT_WINDOW_K` and `GRIT_ASKS` (whole numbers) and `GRIT_FINISHED_AT`
    * (a probability, as 0.8), each unset one as [[LifecycleSettings.Default]] has it; or why
    * they are none, naming the variable.
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
      settle <- duration(SettleVar, default.settle)
      closings <- whole(ClosingsVar, default.closings)
      asks <- whole(AsksVar, default.asks)
      finishedAt <- env.get(FinishedAtVar) match {
        case None => Right(default.finishedAt)
        case Some(raw) => probability(raw).left.map(why => s"$FinishedAtVar: $why")
      }
      windows <- Windows.of(idle, retention).left.map(why => s"$IdleVar, $RetentionVar: $why")
      settings <- LifecycleSettings
        .of(windows, closings, settle, finishedAt, asks)
        .left
        .map(why => s"$ClosingsVar, $SettleVar, $FinishedAtVar, $AsksVar: $why")
    } yield settings
  }

  private def probability(raw: String): Either[String, Probability] =
    raw.trim.toDoubleOption
      .flatMap(Probability.of)
      .toRight("not a probability: write a number from 0 to 1, as 0.8")

  /** One setting changed. */
  enum Change extends caps.Pure {
    case Idle(to: FiniteDuration)
    case Retention(to: FiniteDuration)
    case Closings(to: Int)
    case Settle(to: FiniteDuration)
    case FinishedAt(to: Probability)
    case Asks(to: Int)

    /** `settings` with this change, or why the result breaks their rules. */
    def applied(settings: LifecycleSettings): Either[String, LifecycleSettings] = {
      val s = settings
      val w = s.windows
      this match {
        case Idle(to) =>
          Windows
            .of(to, w.retention)
            .flatMap(LifecycleSettings.of(_, s.closings, s.settle, s.finishedAt, s.asks))
        case Retention(to) =>
          Windows
            .of(w.idle, to)
            .flatMap(LifecycleSettings.of(_, s.closings, s.settle, s.finishedAt, s.asks))
        case Closings(to) => LifecycleSettings.of(w, to, s.settle, s.finishedAt, s.asks)
        case Settle(to) => LifecycleSettings.of(w, s.closings, to, s.finishedAt, s.asks)
        case FinishedAt(to) => LifecycleSettings.of(w, s.closings, s.settle, to, s.asks)
        case Asks(to) => LifecycleSettings.of(w, s.closings, s.settle, s.finishedAt, to)
      }
    }
  }

  object Change {

    /** The names `/set` takes, in the order its help lists them. */
    val Names: Vector[String] =
      Vector("idle", "settle", "finished", "asks", "retention", "closings")

    /** The change `text` writes: a name, then its value (`idle 3m`, `finished 0.9`,
      * `closings 2`); or why it writes none.
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
        case "settle" => duration(Settle(_))
        case "closings" => whole(Closings(_))
        case "asks" => whole(Asks(_))
        case "finished" => probability(value).map(FinishedAt(_)).left.map(why => s"$name: $why")
        case other => Left(s"no setting $other: ${Names.init.mkString(", ")} or ${Names.last}")
      }
    }
  }

  /** `settings` in one line, as `/set` reports them. */
  def describe(settings: LifecycleSettings): String = {
    val w = settings.windows
    val asking =
      if (Probability.value(settings.finishedAt) >= 1.0) "never asks whether it is finished"
      else
        s"after ${written(settings.settle)} quiet, asks whether it is finished (at most " +
          s"${settings.asks} times) and closes at ${Probability.value(settings.finishedAt)} or more"
    s"$asking; closes after ${written(w.idle)} idle; raw entries kept ${written(w.retention)}; " +
      s"${settings.closings} closings open a window"
  }

  /** `d` in the largest of [[Durations]]' units that writes it whole. */
  def written(d: FiniteDuration): String = {
    val s = d.toSeconds
    Vector(86400L -> "d", 3600L -> "h", 60L -> "m")
      .collectFirst { case (unit, name) if s % unit == 0 && s > 0 => s"${s / unit}$name" }
      .getOrElse(s"${s}s")
  }
}
