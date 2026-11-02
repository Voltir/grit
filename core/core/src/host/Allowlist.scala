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

    /** Whether this rule lets `candidate` run. */
    def permits(candidate: Argv): Boolean = this match {
      case Exactly(argv) => candidate == argv
      case Leading(argv) =>
        candidate.program == argv.program && candidate.args.startsWith(argv.args)
    }

    /** As the model is told of it: `git status`, or `git diff …` for a leading rule. */
    def written: String = this match {
      case Exactly(argv) => argv.written
      case Leading(argv) => s"${argv.written} …"
    }
  }

  /** `rules`, each command they permit run for at most `longest`, or why not, refused at the
    * deployment's start: no rules, or `longest` under a second or over [[Shell.Longest]].
    */
  def of(rules: Vector[Rule], longest: FiniteDuration): Either[String, Allowlist] =
    if (rules.isEmpty) Left("an allowlist names no command")
    else if (
      longest < FiniteDuration(1, java.util.concurrent.TimeUnit.SECONDS) || longest > Shell.Longest
    )
      Left(
        s"an allowlist's longest run must be at least a second and at most ${Shell.Longest}, not $longest"
      )
    else Right(new Allowlist(rules, longest))
}

/** Running at the checkout's root only what one [[Allowlist]] permits: the narrowed view of
  * [[Processes]] an allowlisted coding tool is given.
  */
sealed trait Allowlisted extends caps.SharedCapability {

  /** What it permits. */
  def allowlist: Allowlist

  /** Runs `argv` at the root as [[Processes.run]] does, for `timeout` but never past
    * `allowlist.longest`. `Left(HostError.Unlisted)` when `allowlist` does not permit it, and
    * nothing starts; else as [[Processes.run]].
    */
  def run(argv: Argv, timeout: FiniteDuration): Either[HostError, Ran]
}

object Allowlisted {

  /** `processes` narrowed to what `allowlist` permits, each command run at its root. */
  def over(processes: Processes^, allowlist: Allowlist): Allowlisted^ = {
    val permitted = allowlist
    new Allowlisted {
      def allowlist: Allowlist = permitted
      def run(argv: Argv, timeout: FiniteDuration): Either[HostError, Ran] =
        if (!permitted.permits(argv)) Left(HostError.Unlisted(argv))
        else processes.run(argv, RelPath.Root, timeout.min(permitted.longest))
    }
  }
}
