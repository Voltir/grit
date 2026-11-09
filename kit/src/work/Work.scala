package grit.kit.work

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration

import grit.core.clock.{Clock, Fresh}
import grit.core.edge.{Console, Reply, TurnStatus}
import grit.core.host.{Clipped, HostError, Ran, RunId, RunStatus, Runs}
import grit.core.id.{Sha256, SourceId, TurnRef, WorkflowId}
import grit.core.identity.Account
import grit.core.inbox.{InboxError, TurnEnding}
import grit.core.message.Message
import grit.core.store.{Origin, Payload, WentOn}
import grit.core.visibility.Subject
import grit.dbos.sql.DbConfig
import grit.host.checkout.{LocalRuns, LocalShell, Redaction}
import grit.kit.deployment.Deployment
import grit.kit.environment.Secrets
import grit.kit.rehearse.{Rehearse, Rehearsed, Situation, StandIns}
import grit.kit.run.Kit
import grit.turn.TurnStream

/** `work`: one text said to a deployment in a directory's session and its turns awaited, or
  * the turn's first model call shown dry, over the console of the deployment's engine; `grit
  * work` is grit's, and a deployment outside grit runs the same under its own program's name.
  */
object Work {

  /** What `grit work` was asked: the file holding its text, whether it is dry, and the
    * acceptance command `--check` names, when it names one.
    */
  final case class Asked(file: Path, dry: Boolean, check: Option[WentOn.Command])

  /** Why `<program> work`'s arguments are refused: its form, as `program` (`grit`) is run. */
  def usage(program: String): String =
    s"$program work takes one file: $program work [--check '<command>'] <file>, or " +
      s"$program work --dry <file>"

  /** Why `<program> work --dry` is refused while a model key is set. */
  def keyed(program: String): String =
    s"$program work --dry calls no model, so it runs only with no model key set: " +
      "unset ANTHROPIC_API_KEY and OPENROUTER_API_KEY"

  /** What a dry run prints above the rehearsal: what it does not show. */
  val Unshaped: String =
    "Shown as grit builds the request. The provider shapes it for its wire on sending, and " +
      "that is not shown: where it marks the prompt cache (after the system prompt, and on the " +
      "last message the next call repeats) and how it encodes each tool."

  /** `rest`, the arguments after `work` of `program`, as asked: a file, `--check`, a command and
    * a file, or `--dry` and a file; [[usage]] otherwise, and why a command is not one
    * ([[WentOn.Command.of]]).
    */
  def asked(program: String, rest: Vector[String]): Either[String, Asked] = {
    def at(file: String, dry: Boolean, check: Option[WentOn.Command]) =
      scala.util.Try(Path.of(file)).toOption.map(Asked(_, dry, check)).toRight(usage(program))
    rest match {
      case Vector("--dry", file) => at(file, dry = true, None)
      case Vector("--check", command, file) if !file.startsWith("--") =>
        WentOn.Command.of(command).flatMap(c => at(file, dry = false, Some(c)))
      case Vector(file) if !file.startsWith("--") => at(file, dry = false, None)
      case _ => Left(usage(program))
    }
  }

  /** The source a text is said under: `work:` and the lower-case hex of its UTF-8 SHA-256, so
    * the same text said again in the same session names the turn it started.
    */
  def source(text: String): SourceId = SourceId("work:" + Sha256.text(text).hex)

  /** The source turn `turn` of the task `text` began is said under: [[source]] for its first,
    * then that and `:turn`.
    */
  def source(text: String, turn: Int): SourceId =
    if (turn <= 1) source(text) else SourceId(s"${SourceId.value(source(text))}:$turn")

  /** What a task's acceptance command came to, run after a turn that answered. */
  enum Verdict extends caps.Pure {

    /** It exited 0: the task is done. */
    case Passed

    /** It failed, as `failed` says. */
    case Failed(failed: WentOn.CheckFailed)

    /** It could not run, for `why`: it could not start, or its run was refused. */
    case Unrun(why: String)
  }

  /** What `grit work` does after the turn `turn` of a task, its workflow named `named`, ended
    * as `ended`, given at most `cap` turns.
    */
  enum After {

    /** Go on with turn `turn`, as `reason` says, after printing `line`. */
    case GoOn(turn: Int, reason: WentOn, line: String)

    /** Stop, after printing `line` when there is one: the task is done or cannot go on. */
    case Stop(line: Option[String])

    /** Stop and fail: `grit work` ends with `why`, its status 1. */
    case Fail(why: String)
  }

  /** [[After]] the turn `turn`, named `named`, ended as `ended`, of at most `cap`; `verdict`
    * what its task's check came to, when it was checked, and `last` the check that failed
    * after the turn before, when one did. A turn that answered goes on only when its check
    * failed, below `cap`, with output other than `last`'s.
    */
  def after(
      named: String,
      ended: TurnEnding,
      turn: Int,
      cap: Int,
      verdict: Option[Verdict],
      last: Option[WentOn.CheckFailed]
  ): After = (ended, verdict) match {
    case (TurnEnding.Otherwise, _) | (TurnEnding.Answered, None) => After.Stop(None)
    case (TurnEnding.Answered, Some(Verdict.Passed)) =>
      After.Stop(Some(s"[main] $named: the check passed; the task is done"))
    case (TurnEnding.Answered, Some(Verdict.Unrun(why))) =>
      After.Fail(s"the check could not run: $why")
    case (TurnEnding.Answered, Some(Verdict.Failed(failed))) if last.contains(failed) =>
      After.Fail(
        s"$named: the check failed with the same output as last time; stopping, since another " +
          "turn is unlikely to change it"
      )
    case (TurnEnding.Answered, Some(Verdict.Failed(_))) if turn >= cap =>
      After.Fail(
        s"$named: the check still fails at turn $turn, the most $TurnsVar allows; fix it or " +
          s"raise $TurnsVar and rerun"
      )
    case (TurnEnding.Answered, Some(Verdict.Failed(failed))) =>
      After.GoOn(turn + 1, failed, wentOn(named, failed, turn + 1, cap))
    case (TurnEnding.AtLimit, _) if turn >= cap =>
      After.Stop(
        Some(
          s"[main] $named reached its limit at turn $turn, the most $TurnsVar allows; " +
            s"raise $TurnsVar and rerun"
        )
      )
    case (TurnEnding.AtLimit, _) =>
      After.GoOn(
        turn + 1,
        WentOn.AtLimit,
        s"[main] $named reached its limit; going on in a fresh turn (turn ${turn + 1} of at most $cap)"
      )
  }

  /** `check` run with `sh -c` as one of `work`'s runs, at its root, and waited for until it
    * ends, killed with what it started at `within`: exit 0 passes; any other exit, or running
    * past `within`, fails, keeping the end of its output; [[Verdict.Unrun]] when it cannot
    * start, or as `work` refuses it (its output past [[grit.core.host.Workspace.MaxFileBytes]]).
    * A check a `grit work` killed by a signal left going is ended by `work`'s next start, as
    * any run is.
    */
  def checked(
      work: Runs^,
      check: WentOn.Command,
      within: FiniteDuration = WentOn.CheckLongest
  ): Verdict =
    LocalShell.command(check.text) match {
      case Left(why) => Verdict.Unrun(why)
      case Right(argv) =>
        work.start(argv, within, within).flatMap((id, status) => ended(work, id, status)) match {
          case Right((Some(0), _)) => Verdict.Passed
          case Right((exit, output)) => Verdict.Failed(WentOn.failed(check, exit, output.text))
          case Left(refused) => Verdict.Unrun(refused.message)
        }
    }

  /** The run `id` of `work`, standing as `status`, waited for until it ends: its exit, `None`
    * when it was killed, and its output.
    */
  @tailrec private def ended(
      work: Runs^,
      id: RunId,
      status: RunStatus
  ): Either[HostError, (Option[Int], Clipped)] = status match {
    case RunStatus.Going(_, _, _) =>
      work.tail(id, Runs.Wait) match {
        case Right((_, next)) => ended(work, id, next)
        case Left(refused) => Left(refused)
      }
    case RunStatus.Ended(Ran(exit, output)) => Right((Some(exit), output))
    case RunStatus.TimedOut(_, output) => Right((None, output))
    case RunStatus.Stopped(_, output) => Right((None, output))
  }

  /** The most lines of a failed check's output `grit work` prints. */
  val PrintedLines: Int = 20

  /** What `grit work` prints when the turn named `named` failed its check `failed` and the task
    * goes on in turn `next` of at most `cap`: the failure, then the last [[PrintedLines]] lines
    * of its output, each indented under a bar.
    */
  def wentOn(named: String, failed: WentOn.CheckFailed, next: Int, cap: Int): String = {
    val how = failed.exit.fold(
      s"it ran past ${WentOn.CheckLongest.toMinutes} minutes and was stopped"
    )(code => s"exit $code")
    val lines = Clipped.lines(failed.tail).takeRight(PrintedLines).map(l => s"\n  | $l")
    s"[main] $named: the check failed ($how); going on in a fresh turn (turn $next of at most $cap)" +
      lines.mkString
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

  /** The text of `file`, read as UTF-8; why not, naming `program`'s `work`: it cannot be read,
    * or holds only whitespace.
    */
  def text(program: String, file: Path): Either[String, String] =
    scala.util
      .Try(Files.readString(file, StandardCharsets.UTF_8))
      .toEither
      .left
      .map(e => s"$program work cannot read $file: ${e.getMessage}")
      .filterOrElse(_.trim.nonEmpty, s"$program work was given $file, which is empty")

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

  /** What a deployment supplies a work run: the `deployment` run over the database `secrets`
    * names, the `origin` its text is said at, the directory `root` its check runs in under the
    * environment `env` with what it shows cleared by `redaction`, how long the stub model, when `secrets` holds no key,
    * waits before it answers (`stubDelay`), and `admit`, whether a run that is not dry may go on
    * given who runs the database's engine (`Left`: why not).
    */
  final case class Under(
      deployment: Deployment,
      secrets: Secrets,
      origin: Origin,
      root: Path,
      env: Map[String, String],
      redaction: Redaction,
      stubDelay: FiniteDuration,
      admit: Console.Runner -> Either[String, Unit]
  )

  /** `asked` run as `under` supplies it, through the console of the deployment's engine
    * ([[Kit.console]]), `here` served first and each model call printed through `print` as it
    * goes and once answered: dry, the first model call `text` said would make, shown
    * ([[shown]]) and nothing said; otherwise, once `under.admit` allows who runs the engine,
    * `text` said and its turns awaited, up to `cap` ([[said]]), `asked.check` run among runs
    * in `under.root` ([[checked]]). Why not, and then nothing is said: the database cannot be
    * reached, `under.admit` refuses (the console is never opened, and an engine lock taken here
    * is held until the process ends), or as [[shown]] and [[said]] say.
    */
  def run(
      asked: Asked,
      text: String,
      cap: Int,
      under: Under,
      here: Console.Edge^,
      clock: Clock^,
      fresh: Fresh^,
      print: String -> Unit
  ): Either[String, Unit] = {
    // The same composition either way: only the last step differs.
    val step: Console^ => Either[String, Unit] = console =>
      if (asked.dry)
        shown(under.secrets.database, under.deployment, under.origin, text, clock, print)
      else {
        val checks: LocalRuns^ = new LocalRuns(under.root, under.env, under.redaction, clock, fresh)
        try said(console, under.origin, text, cap, asked.check, checks, print)
        finally checks.close()
      }
    Kit.console(
      under.deployment,
      under.secrets,
      under.stubDelay,
      clock,
      here,
      announced = true
    ) match {
      case Left(refused) => Left(refused.message)
      case Right(opener) =>
        val admitted = if (asked.dry) Right(()) else under.admit(opener.runner)
        admitted.flatMap(_ => opened(opener)(step))
    }
  }

  /** `text` said once through `console` at `origin` as the local person, its turn started (or,
    * said before, the turn it started found) and waited for until its workflow ends; then the
    * turn's workflow id, what the workflow returned and its reply printed through `print` as one
    * line, as a run with arguments prints a message's. While a turn ends at its limit, or
    * answers and its task's acceptance command `check` then fails ([[checked]] over `work`),
    * the task goes on in a new turn rooted on grit's continuation note
    * ([[grit.core.inbox.Inbox.continued]]), as [[after]] says, up to `cap` turns, each said
    * under [[source]] of its number, so a rerun walks the chain to where it stopped: a turn
    * that answered and was followed by a recorded turn is not checked again, its failure
    * printed as recorded. Why not: the inbox refused the text or a continuation, a turn left
    * no reply, or as [[After.Fail]] says.
    */
  def said(
      console: Console^,
      origin: Origin,
      text: String,
      cap: Int,
      check: Option[WentOn.Command],
      work: Runs^,
      print: String -> Unit
  ): Either[String, Unit] =
    console.inbox
      .ingest(origin, source(text), Message.User(text), Account.Local, None)
      .flatMap(turn => console.inbox.startTurn(turn).map(_ => turn)) match {
      case Left(error) => Left(InboxError.told(error))
      case Right(task) =>
        def started(next: Either[grit.core.inbox.InboxError, TurnRef]) =
          next.flatMap(t => console.inbox.startTurn(t).map(_ => t)).left.map(InboxError.told)
        def go(turn: TurnRef, n: Int, last: Option[WentOn.CheckFailed]): Either[String, Unit] =
          awaited(console, turn, print).flatMap { outcome =>
            val named = WorkflowId.value(turn.workflowId)
            val ended = TurnEnding.of(outcome)
            val answered = ended == TurnEnding.Answered && check.nonEmpty
            val recorded =
              if (answered) checkedBefore(console, origin, source(text, n + 1))
              else Right(None)
            recorded.flatMap {
              case Some((next, failed)) =>
                print(wentOn(named, failed, n + 1, cap))
                started(Right(next)).flatMap(go(_, n + 1, Some(failed)))
              case None =>
                val verdict = check.filter(_ => answered).map { c =>
                  print(s"[main] $named ended its turn; checking: ${c.text}")
                  checked(work, c)
                }
                after(named, ended, n, cap, verdict, last) match {
                  case After.Stop(line) => Right(line.foreach(print))
                  case After.Fail(why) => Left(why)
                  case After.GoOn(next, reason, line) =>
                    print(line)
                    val failed = reason match {
                      case f: WentOn.CheckFailed => Some(f)
                      case WentOn.AtLimit => None
                    }
                    started(
                      console.inbox.continued(
                        origin,
                        source(text, next),
                        task.turnSeq,
                        next,
                        reason,
                        Account.Local
                      )
                    ).flatMap(go(_, next, failed))
                }
            }
          }
        go(task, 1, None)
    }

  /** The turn recorded at `origin` under `source` and the failed check it went on after, when
    * one is: a check a rerun need not run again. Why not: the store cannot be read.
    */
  private def checkedBefore(
      console: Console^,
      origin: Origin,
      source: SourceId
  ): Either[String, Option[(TurnRef, WentOn.CheckFailed)]] =
    console.inbox.ingested(origin, source).left.map(InboxError.told).flatMap {
      case None => Right(None)
      case Some(next) =>
        console.db
          .read(Subject.Turn(next))(console.entries.ofTurn(next))
          .left
          .map(e => s"unreadable: $e")
          .map(_.map(_.payload).collectFirst {
            case Payload.Continuation(_, _, f: WentOn.CheckFailed) => (next, f)
          })
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
    Reply.of(console, turn) match {
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
