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
  * files' expiry, the theme, the tools, the assembler and the cache marks.
  */
private[main] object RunChoices {

  private[main] val BudgetVar = "GRIT_WINDOW_TOKENS"

  private[main] val StubDelayVar = "GRIT_STUB_DELAY_MS"

  private[main] val TailVar = "GRIT_TAIL_TOKENS"

  private[main] val AssemblerVar = "GRIT_ASSEMBLER"

  /** The token count in `variable`, or `default` when it is unset. */
  private[main] def tokens(
      env: Map[String, String],
      variable: String,
      default: Tokens
  ): Either[String, Tokens] =
    whole(env, variable, Tokens.value(default)).map(Tokens(_))

  /** The non-negative whole number in `variable`, or `default` when it is unset; `Left` naming
    * `variable` for any other text.
    */
  private[main] def whole(
      env: Map[String, String],
      variable: String,
      default: Long
  ): Either[String, Long] =
    env.get(variable) match {
      case None => Right(default)
      case Some(raw) =>
        raw.trim.toLongOption
          .filter(_ >= 0)
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
    duration(env, SweepVar, DefaultSweep, 1.second, "a second")

  private[main] val FilesExpiryVar = "GRIT_FILES_EXPIRY"

  /** How long the file area keeps a file, from `GRIT_FILES_EXPIRY`, a duration ([[Durations]])
    * of at least a minute; [[Files.Expiry]] when unset.
    */
  private[main] def filesExpiry(env: Map[String, String]): Either[String, FiniteDuration] =
    duration(env, FilesExpiryVar, Files.Expiry, 1.minute, "a minute")

  /** The duration ([[Durations]]) in `variable`, at least `least`, which its refusal says as
    * `leastWords`; `default` when it is unset.
    */
  private def duration(
      env: Map[String, String],
      variable: String,
      default: FiniteDuration,
      least: FiniteDuration,
      leastWords: String
  ): Either[String, FiniteDuration] =
    env.get(variable) match {
      case None => Right(default)
      case Some(raw) =>
        Durations
          .read(raw)
          .left
          .map(why => s"$variable: $why")
          .filterOrElse(_ >= least, s"$variable must be at least $leastWords")
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

  /** The most input a turn's request may reach, as its provider counts it: 500,000 tokens. A
    * backstop for a pin whose profile declares no bound below its context (the seed's
    * gemini-3.5-flash-lite); a pin's effective context and its first price rise, where its
    * profile gives them, hold it lower (`TurnLoop.inputLimit`).
    */
  private[main] val InputLimit: Tokens = Tokens(500_000)

  /** The turn's budget of model calls from `GRIT_TOOL_ROUNDS`, a whole number of at least 2;
    * [[DefaultToolRounds]] when unset; its requests' input within [[InputLimit]].
    */
  private[main] def toolRounds(env: Map[String, String]): Either[String, TurnLoop.Rounds] =
    env.get(ToolRoundsVar) match {
      case None => TurnLoop.Rounds.of(DefaultToolRounds).flatMap(_.within(InputLimit))
      case Some(raw) =>
        raw.trim.toIntOption
          .toRight(s"$ToolRoundsVar is not a whole number")
          .flatMap(TurnLoop.Rounds.of(_).left.map(why => s"$ToolRoundsVar: $why"))
          .flatMap(_.within(InputLimit))
    }

  private[main] val ToolsVar = "GRIT_TOOLS"

  /** The tools `GRIT_TOOLS` names, `read`, `all` or `allowlist`, for the chat when `chat`, else
    * for a run with arguments or `grit serve`. Unset is [[Offered.All]] in the chat, where a
    * person approves each call that changes something, and [[Offered.Read]] elsewhere, where
    * nobody can; `all` outside the chat is refused. `allowlist` is
    * [[Offered.ReferenceAllowlist]] anywhere: it asks no one (ADR 0055).
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
      case Some("allowlist") => Right(Offered.Allowlisted(Offered.ReferenceAllowlist))
      case Some(_) => Left(s"$ToolsVar is not read, all or allowlist")
    }

  /** Whether `GRIT_ASSEMBLER` asks for retrieval; unset is retrieval. */
  private[main] def assemblerChoice(env: Map[String, String]): Either[String, Boolean] =
    env.get(AssemblerVar).map(_.trim) match {
      case None | Some("retrieval") => Right(true)
      case Some("linear") => Right(false)
      case Some(_) => Left(s"$AssemblerVar is neither linear nor retrieval")
    }
}
