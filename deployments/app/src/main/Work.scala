package grit.app.main

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import grit.chat.host.{ChatHost, Replies}
import grit.core.clock.Clock
import grit.core.edge.{Console, TurnStatus}
import grit.core.id.{SourceId, TurnRef, WorkflowId}
import grit.core.identity.Account
import grit.core.inbox.TurnEnding
import grit.core.message.Message
import grit.core.store.{Origin, WentOn}
import grit.dbos.sql.DbConfig
import grit.kit.deployment.Deployment
import grit.kit.rehearse.{Rehearse, Rehearsed, Situation, StandIns}
import grit.turn.TurnStream

/** `grit work`: one text said to grit in a directory's session and its turn awaited, or the
  * turn's first model call shown dry, over the console of the deployment's engine.
  */
private[main] object Work {

  /** What `grit work` was asked: the file holding its text, and whether it is dry. */
  final case class Asked(file: Path, dry: Boolean)

  /** Why `grit work`'s arguments are refused: its form. */
  val Usage: String = "grit work takes one file: grit work <file>, or grit work --dry <file>"

  /** Why `grit work --dry` is refused while a model key is set. */
  val Keyed: String =
    "grit work --dry calls no model, so it runs only with no model key set: " +
      "unset ANTHROPIC_API_KEY and OPENROUTER_API_KEY"

  /** What a dry run prints above the rehearsal: what it does not show. */
  val Unshaped: String =
    "Shown as grit builds the request. The provider shapes it for its wire on sending, and " +
      "that is not shown: where it marks the prompt cache (after the system prompt, and on the " +
      "last message the next call repeats) and how it encodes each tool."

  /** `rest`, the arguments after `work`, as asked: a file, or `--dry` and a file; [[Usage]]
    * otherwise.
    */
  def asked(rest: Vector[String]): Either[String, Asked] = {
    def at(file: String, dry: Boolean) =
      scala.util.Try(Path.of(file)).toOption.map(Asked(_, dry)).toRight(Usage)
    rest match {
      case Vector("--dry", file) => at(file, dry = true)
      case Vector(file) if file != "--dry" => at(file, dry = false)
      case _ => Left(Usage)
    }
  }

  /** The source a text is said under: `work:` and the lower-case hex of its UTF-8 SHA-256, so
    * the same text said again in the same session names the turn it started.
    */
  def source(text: String): SourceId = {
    val digest = java.security.MessageDigest
      .getInstance("SHA-256")
      .digest(text.getBytes(StandardCharsets.UTF_8))
    SourceId("work:" + digest.map(b => f"${b & 0xff}%02x").mkString)
  }

  /** The source turn `turn` of the task `text` began is said under: [[source]] for its first,
    * then that and `:turn`.
    */
  def source(text: String, turn: Int): SourceId =
    if (turn <= 1) source(text) else SourceId(s"${SourceId.value(source(text))}:$turn")

  /** What `grit work` does after the turn `turn` of a task, its workflow named `named`, ended
    * as `ended`, given at most `cap` turns.
    */
  enum After {

    /** Go on with turn `turn`, after printing `line`. */
    case GoOn(turn: Int, line: String)

    /** Stop, after printing `line` when there is one. */
    case Stop(line: Option[String])
  }

  /** [[After]] the turn `turn`, named `named`, ended as `ended`, of at most `cap`. */
  def after(named: String, ended: TurnEnding, turn: Int, cap: Int): After = ended match {
    case TurnEnding.Otherwise | TurnEnding.Answered => After.Stop(None)
    case TurnEnding.AtLimit if turn >= cap =>
      After.Stop(
        Some(
          s"[main] $named reached its limit at turn $turn, the most $TurnsVar allows; " +
            s"raise $TurnsVar and rerun"
        )
      )
    case TurnEnding.AtLimit =>
      After.GoOn(
        turn + 1,
        s"[main] $named reached its limit; going on in a fresh turn (turn ${turn + 1} of at most $cap)"
      )
  }

  /** The most turns `grit work` gives one task: a stop for a loop that cannot end, not a spend
    * cap (`GRIT_DAILY_USD` is one).
    */
  val TurnsVar: String = "GRIT_WORK_TURNS"

  /** [[TurnsVar]] when unset. */
  val DefaultTurns: Int = 8

  /** [[TurnsVar]] from `env`, a whole number of at least 1; [[DefaultTurns]] when unset. */
  def turns(env: Map[String, String]): Either[String, Int] =
    env.get(TurnsVar).map(_.trim) match {
      case None => Right(DefaultTurns)
      case Some(raw) =>
        raw.toIntOption
          .filter(_ >= 1)
          .toRight(s"$TurnsVar is a whole number of at least 1, not $raw")
    }

  /** The text of `file`, read as UTF-8; why not, when it cannot be read or holds only
    * whitespace.
    */
  def text(file: Path): Either[String, String] =
    scala.util
      .Try(Files.readString(file, StandardCharsets.UTF_8))
      .toEither
      .left
      .map(e => s"grit work cannot read $file: ${e.getMessage}")
      .filterOrElse(_.trim.nonEmpty, s"grit work was given $file, which is empty")

  /** `step` run on the console `opener` opens, which is closed after it returns or throws; why
    * not, when it cannot be opened, or as `step` says.
    */
  def opened(
      opener: Console.Opener^
  )(step: Console^ => Either[String, Unit]): Either[String, Unit] =
    opener.open() match {
      case Left(why) => Left(why)
      case Right(console) =>
        try step(console)
        finally console.close()
    }

  /** `text` said once through `console` at `origin` as the local person, its turn started (or,
    * said before, the turn it started found) and waited for until its workflow ends; then the
    * turn's workflow id, what the workflow returned and its reply printed through `print` as one
    * line, as a run with arguments prints a message's. While a turn ends at its limit, the task
    * goes on in a new turn rooted on grit's continuation note
    * ([[grit.core.inbox.Inbox.continued]]), as
    * [[after]] says, up to `cap` turns, each said under [[source]] of its number, so a rerun
    * walks the chain to where it stopped. Why not: the inbox refused the text or a
    * continuation, or a turn left no reply.
    */
  def said(
      console: Console^,
      origin: Origin,
      text: String,
      cap: Int,
      print: String -> Unit
  ): Either[String, Unit] =
    console.inbox
      .ingest(origin, source(text), Message.User(text), Account.Local, None)
      .flatMap(turn => console.inbox.startTurn(turn).map(_ => turn)) match {
      case Left(error) => Left(ChatHost.notSent(error))
      case Right(task) =>
        def go(turn: TurnRef, n: Int): Either[String, Unit] =
          awaited(console, turn, print).flatMap { outcome =>
            after(WorkflowId.value(turn.workflowId), TurnEnding.of(outcome), n, cap) match {
              case After.Stop(line) => Right(line.foreach(print))
              case After.GoOn(next, line) =>
                print(line)
                console.inbox
                  .continued(
                    origin,
                    source(text, next),
                    task.turnSeq,
                    next,
                    WentOn.AtLimit,
                    Account.Local
                  )
                  .flatMap(t => console.inbox.startTurn(t).map(_ => t)) match {
                  case Left(error) => Left(ChatHost.notSent(error))
                  case Right(t) => go(t, next)
                }
            }
          }
        go(task, 1)
    }

  /** `turn` waited for until its workflow ends, then its workflow id, what the workflow
    * returned and its reply printed through `print` as one line; what it returned, or why not:
    * the turn left no reply.
    */
  private def awaited(
      console: Console^,
      turn: TurnRef,
      print: String -> Unit
  ): Either[String, String] = {
    // The stream's pieces end when the turn's workflow does: read through, it is the wait.
    console.stream(turn, TurnStream.Key).foreach(_ => ())
    val outcome = console.status(turn) match {
      case TurnStatus.Finished(output) => output
      case other => other.toString
    }
    val named = WorkflowId.value(turn.workflowId)
    Replies.of(console, turn) match {
      case Left(why) => Left(why)
      case Right(None) => Left(s"the turn $named left no reply: $outcome")
      case Right(Some(reply)) =>
        print(s"[main] $named -> $outcome: $reply")
        Right(outcome)
    }
  }

  /** [[Unshaped]], then what `text` said at `origin` would send first ([[rehearsed]]) whole
    * ([[Rehearsed.written]]), printed through `print`; the refusals are [[rehearsed]]'s.
    */
  def shown(
      config: DbConfig,
      deployment: Deployment,
      origin: Origin,
      text: String,
      clock: Clock^,
      print: String -> Unit
  ): Either[String, Unit] =
    rehearsed(config, deployment, origin, text, clock).map { r =>
      print(Unshaped)
      print(r.written)
    }

  /** What `text` said at `origin` would start, rehearsed dry over the database `config` names as
    * it stands, up to the turn's own first call, shown and never sent ([[Rehearse.dry]]); every
    * call answered by the stand-ins, nothing recorded. Its refusals are [[Rehearse.dry]]'s.
    */
  def rehearsed(
      config: DbConfig,
      deployment: Deployment,
      origin: Origin,
      text: String,
      clock: Clock^
  ): Either[String, Rehearsed] =
    Rehearse.dry(
      config,
      deployment,
      Situation.Said(origin, Account.Local, text),
      StandIns.Stubbed,
      clock
    )
}
