package grit.core.host

import scala.concurrent.duration.FiniteDuration

/** The commands a coding tool's `run` may start, and for how long: a value a deployment declares
  * (ADR 0021). It names what `run` starts, not what a started program does: a listed build runs
  * the checkout's code, with the network, and a file `write` changes may be what a listed command
  * runs next (ADR 0055).
  */
final case class Allowlist private (rules: Vector[Allowlist.Rule], longest: FiniteDuration)
    extends caps.Pure {

  /** Whether a rule lets `argv` run. */
  def permits(argv: Argv): Boolean = rules.exists(_.permits(argv))

  /** What must follow `argv` for a rule to let it run, when `argv` is a following rule's
    * command with nothing after it; `None` otherwise.
    */
  def wanting(argv: Argv): Option[String] =
    rules.collectFirst { case Allowlist.Rule.Following(command, what) if command == argv => what }
}

object Allowlist {

  /** One command shape an allowlist permits. Its program is matched exactly as written:
    * `/usr/bin/git` is not `git`.
    */
  enum Rule extends caps.Pure {

    /** `argv` alone, its arguments neither more nor fewer. */
    case Exactly(argv: Argv)

    /** `argv`'s program and arguments, followed by any more or none. */
    case Leading(argv: Argv)

    /** `argv`'s program and arguments, followed by at least one more: `what` says what they are,
      * as the model reads it (`the suites or modules to check`).
      */
    case Following(argv: Argv, what: String)

    /** Whether this rule lets `candidate` run. */
    def permits(candidate: Argv): Boolean = this match {
      case Exactly(argv) => candidate == argv
      case Leading(argv) =>
        candidate.program == argv.program && candidate.args.startsWith(argv.args)
      case Following(argv, _) =>
        candidate.program == argv.program && candidate.args.startsWith(argv.args) &&
        candidate.args.size > argv.args.size
    }

    /** As the model is told of it: `git status`, or `git diff …` for a rule that takes more. */
    def written: String = this match {
      case Exactly(argv) => argv.written
      case Leading(argv) => s"${argv.written} …"
      case Following(argv, _) => s"${argv.written} …"
    }

    /** Its line in the run tool's list: [[written]] in backticks, and for a following rule, what
      * must follow it, as "`scripts/check …` (at least one argument: the suites or modules to
      * check)".
      */
    def listed: String = this match {
      case Following(_, what) => s"`$written` (at least one argument: $what)"
      case _ => s"`$written`"
    }
  }

  /** `rules`, each command they permit run for at most `longest`, or why not, refused at the
    * deployment's start: no rules, or `longest` under a second or over [[Runs.Longest]].
    */
  def of(rules: Vector[Rule], longest: FiniteDuration): Either[String, Allowlist] =
    if (rules.isEmpty) Left("an allowlist names no command")
    else if (
      longest < FiniteDuration(1, java.util.concurrent.TimeUnit.SECONDS) || longest > Runs.Longest
    )
      Left(
        s"an allowlist's longest run must be at least a second and at most ${Runs.Longest}, not $longest"
      )
    else Right(new Allowlist(rules, longest))
}

/** Running at the checkout's root only what one [[Allowlist]] permits: the narrowed view of
  * [[Runs]] an allowlisted coding tool is given.
  */
sealed trait Allowlisted extends caps.SharedCapability {

  /** What it permits. */
  def allowlist: Allowlist

  /** `argv` started as [[Runs.start]] starts it, for `timeout` but never past
    * `allowlist.longest`. When `allowlist` does not permit it, nothing starts:
    * `Left(HostError.Unfinished)` when it is a following rule's command with nothing after it,
    * else `Left(HostError.Unlisted)`. Otherwise as [[Runs.start]].
    */
  def start(
      argv: Argv,
      timeout: FiniteDuration,
      wait: FiniteDuration
  ): Either[HostError, (id: RunId, status: RunStatus)]

  /** As [[Runs.tail]]. */
  def tail(id: RunId, wait: FiniteDuration): Either[HostError, (argv: Argv, status: RunStatus)]

  /** As [[Runs.stop]]. */
  def stop(id: RunId): Either[HostError, (argv: Argv, status: RunStatus)]
}

object Allowlisted {

  /** `runs` narrowed to what `allowlist` permits. */
  def over(runs: Runs^, allowlist: Allowlist): Allowlisted^ = {
    val permitted = allowlist
    new Allowlisted {
      def allowlist: Allowlist = permitted
      def start(
          argv: Argv,
          timeout: FiniteDuration,
          wait: FiniteDuration
      ): Either[HostError, (id: RunId, status: RunStatus)] =
        if (!permitted.permits(argv))
          Left(
            permitted.wanting(argv).fold(HostError.Unlisted(argv))(HostError.Unfinished(argv, _))
          )
        else runs.start(argv, timeout.min(permitted.longest), wait)
      def tail(
          id: RunId,
          wait: FiniteDuration
      ): Either[HostError, (argv: Argv, status: RunStatus)] =
        runs.tail(id, wait)
      def stop(id: RunId): Either[HostError, (argv: Argv, status: RunStatus)] = runs.stop(id)
    }
  }
}
