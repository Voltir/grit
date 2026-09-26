package grit.app.config

import scala.concurrent.duration.FiniteDuration

import grit.core.period.{LifecycleSettings, Windows}

/** The lifecycle's settings as a person writes them: seeded from the environment on first
  * start, then changed one at a time with `/set`.
  */
object Lifecycle {

  private val IdleVar = "GRIT_IDLE"
  private val GraceVar = "GRIT_GRACE"
  private val RetentionVar = "GRIT_RETENTION"
  private val ClosingsVar = "GRIT_WINDOW_K"

  /** The settings the environment seeds: `GRIT_IDLE`, `GRIT_GRACE` and `GRIT_RETENTION`
    * ([[Durations]]) and `GRIT_WINDOW_K` (a whole number), each unset one as
    * [[LifecycleSettings.Default]] has it; or why they are none, naming the variable.
    */
  def fromEnv(env: Map[String, String]): Either[String, LifecycleSettings] = {
    val default = LifecycleSettings.Default
    def duration(variable: String, otherwise: FiniteDuration): Either[String, FiniteDuration] =
      env
        .get(variable)
        .fold(Right(otherwise))(Durations.read(_).left.map(why => s"$variable: $why"))
    for {
      idle <- duration(IdleVar, default.windows.idle)
      grace <- duration(GraceVar, default.windows.grace)
      retention <- duration(RetentionVar, default.windows.retention)
      closings <- env.get(ClosingsVar) match {
        case None => Right(default.closings)
        case Some(raw) => raw.trim.toIntOption.toRight(s"$ClosingsVar is not a whole number")
      }
      windows <- Windows
        .of(idle, grace, retention)
        .left
        .map(why => s"$IdleVar, $GraceVar, $RetentionVar: $why")
      settings <- LifecycleSettings.of(windows, closings).left.map(why => s"$ClosingsVar: $why")
    } yield settings
  }

  /** One setting changed. */
  enum Change extends caps.Pure {
    case Idle(to: FiniteDuration)
    case Grace(to: FiniteDuration)
    case Retention(to: FiniteDuration)
    case Closings(to: Int)

    /** `settings` with this change, or why the result breaks their rules. */
    def applied(settings: LifecycleSettings): Either[String, LifecycleSettings] = {
      val w = settings.windows
      val windows = this match {
        case Idle(to) => Windows.of(to, w.grace, w.retention)
        case Grace(to) => Windows.of(w.idle, to, w.retention)
        case Retention(to) => Windows.of(w.idle, w.grace, to)
        case Closings(_) => Right(w)
      }
      val closings = this match {
        case Closings(to) => to
        case Idle(_) | Grace(_) | Retention(_) => settings.closings
      }
      windows.flatMap(LifecycleSettings.of(_, closings))
    }
  }

  object Change {

    /** The names `/set` takes, in the order its help lists them. */
    val Names: Vector[String] = Vector("idle", "grace", "retention", "closings")

    /** The change `text` writes: a name, then its value (`idle 3m`, `closings 2`); or why it
      * writes none.
      */
    def parse(text: String): Either[String, Change] = {
      val (name, value) = text.trim.span(_ != ' ')
      def duration(make: FiniteDuration -> Change): Either[String, Change] =
        if (value.isBlank) Left(s"$name takes a duration, as 30s or 3m")
        else Durations.read(value).map(make).left.map(why => s"$name: $why")
      name match {
        case "idle" => duration(Idle(_))
        case "grace" => duration(Grace(_))
        case "retention" => duration(Retention(_))
        case "closings" =>
          value.trim.toIntOption
            .filter(_ >= 0)
            .map(Closings(_))
            .toRight("closings takes a whole number")
        case other => Left(s"no setting $other: ${Names.init.mkString(", ")} or ${Names.last}")
      }
    }
  }

  /** `settings` in one line, as `/set` reports them. */
  def describe(settings: LifecycleSettings): String = {
    val w = settings.windows
    s"closes after ${written(w.idle)} idle, or ${written(w.grace)} after /done; " +
      s"raw entries kept ${written(w.retention)}; ${settings.closings} closings open a window"
  }

  /** `d` in the largest of [[Durations]]' units that writes it whole. */
  def written(d: FiniteDuration): String = {
    val s = d.toSeconds
    Vector(86400L -> "d", 3600L -> "h", 60L -> "m")
      .collectFirst { case (unit, name) if s % unit == 0 && s > 0 => s"${s / unit}$name" }
      .getOrElse(s"${s}s")
  }
}
