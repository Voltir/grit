package grit.app.main

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.chat.look.Theme
import grit.chat.settings.{Durations, Prefs}
import grit.core.files.Files
import grit.core.id.ConversationId
import grit.core.message.Tokens
import grit.kit.deployment.Offered
import grit.turn.TurnLoop

/** What a run reads from its environment besides the deployment: the window, the stub, the sweep,
  * files' expiry, the theme, the tools and the assembler.
  */
private[main] object RunChoices {

  private[main] val BudgetVar = "GRIT_WINDOW_TOKENS"

  private[main] val StubDelayVar = "GRIT_STUB_DELAY_MS"

  private[main] val TailVar = "GRIT_TAIL_TOKENS"

  private[main] val AssemblerVar = "GRIT_ASSEMBLER"

  /** The milliseconds in `variable`, or 0 when it is unset. */
  private[main] def millis(env: Map[String, String], variable: String): Either[String, Long] =
    env.get(variable) match {
      case None => Right(0L)
      case Some(raw) =>
        raw.trim.toLongOption
          .filter(_ >= 0)
          .toRight(s"$variable is not a non-negative whole number")
    }

  /** The token count in `variable`, or `default` when it is unset. */
  private[main] def tokens(
      env: Map[String, String],
      variable: String,
      default: Tokens
  ): Either[String, Tokens] =
    env.get(variable) match {
      case None => Right(default)
      case Some(raw) =>
        raw.trim.toLongOption
          .filter(_ >= 0)
          .map(Tokens(_))
          .toRight(s"$variable is not a non-negative whole number")
    }

  private[main] val SweepVar = "GRIT_SWEEP"

  /** What `grit cache-report` reports, read from `args`: `Some(Some(id))` for the conversation
    * named, `Some(None)` with no id for the one the chat holds here; `None` for any other
    * arguments.
    */
  private[main] def cacheReported(args: Vector[String]): Option[Option[ConversationId]] =
    args match {
      case Vector("cache-report", id) => Some(Some(ConversationId(id)))
      case Vector("cache-report") => Some(None)
      case _ => None
    }

  /** How often the engine sweeps: closing due periods. */
  private[main] val DefaultSweep: FiniteDuration = 30.seconds

  /** The sweep's interval from `GRIT_SWEEP`, a duration ([[Durations]]) of at least a
    * second; [[DefaultSweep]] when unset.
    */
  private[main] def sweepEvery(env: Map[String, String]): Either[String, FiniteDuration] =
    env.get(SweepVar) match {
      case None => Right(DefaultSweep)
      case Some(raw) =>
        Durations
          .read(raw)
          .left
          .map(why => s"$SweepVar: $why")
          .filterOrElse(_ >= 1.second, s"$SweepVar must be at least a second")
    }

  private[main] val FilesExpiryVar = "GRIT_FILES_EXPIRY"

  /** How long the file area keeps a file, from `GRIT_FILES_EXPIRY`, a duration ([[Durations]])
    * of at least a minute; [[Files.Expiry]] when unset.
    */
  private[main] def filesExpiry(env: Map[String, String]): Either[String, FiniteDuration] =
    env.get(FilesExpiryVar) match {
      case None => Right(Files.Expiry)
      case Some(raw) =>
        Durations
          .read(raw)
          .left
          .map(why => s"$FilesExpiryVar: $why")
          .filterOrElse(_ >= 1.minute, s"$FilesExpiryVar must be at least a minute")
    }

  private[main] val ThemeVar = "GRIT_THEME"

  /** The theme `GRIT_THEME` names; unset, the one `kept` names, if grit still has it;
    * otherwise [[Theme.Default]].
    */
  private[main] def theme(env: Map[String, String], kept: Prefs): Either[String, Theme] =
    env.get(ThemeVar) match {
      case None => Right(kept.theme.flatMap(Theme.named).getOrElse(Theme.Default))
      case Some(name) =>
        Theme.named(name).toRight(s"$ThemeVar is none of ${Theme.all.map(_.key).mkString(", ")}")
    }

  private[main] val ToolRoundsVar = "GRIT_TOOL_ROUNDS"

  /** The most model calls a turn makes, the last with tools off. */
  private[main] val DefaultToolRounds = 20

  /** The turn's budget of model calls from `GRIT_TOOL_ROUNDS`, a whole number of at least 2;
    * [[DefaultToolRounds]] when unset.
    */
  private[main] def toolRounds(env: Map[String, String]): Either[String, TurnLoop.Rounds] =
    env.get(ToolRoundsVar) match {
      case None => TurnLoop.Rounds.of(DefaultToolRounds)
      case Some(raw) =>
        raw.trim.toIntOption
          .toRight(s"$ToolRoundsVar is not a whole number")
          .flatMap(TurnLoop.Rounds.of(_).left.map(why => s"$ToolRoundsVar: $why"))
    }

  private[main] val ToolsVar = "GRIT_TOOLS"

  /** The tools `GRIT_TOOLS` names, `read` or `all`, for the chat when `chat`, else for a run
    * with arguments. Unset is [[Offered.All]] in the chat, where a person approves each
    * call that changes something, and [[Offered.Read]] in a run, where nobody can; `all`
    * in a run is refused.
    */
  private[main] def toolChoice(
      env: Map[String, String],
      chat: Boolean
  ): Either[String, Offered] =
    env.get(ToolsVar).map(_.trim) match {
      case None => Right(if (chat) Offered.All else Offered.Read)
      case Some("read") => Right(Offered.Read)
      case Some("all") if chat => Right(Offered.All)
      case Some("all") =>
        Left(s"$ToolsVar=all needs the chat, which answers what a tool asks first")
      case Some(_) => Left(s"$ToolsVar is neither read nor all")
    }

  /** Whether `GRIT_ASSEMBLER` asks for retrieval; unset is retrieval. */
  private[main] def assemblerChoice(env: Map[String, String]): Either[String, Boolean] =
    env.get(AssemblerVar).map(_.trim) match {
      case None | Some("retrieval") => Right(true)
      case Some("linear") => Right(false)
      case Some(_) => Left(s"$AssemblerVar is neither linear nor retrieval")
    }
}
