package grit.core.host

import scala.concurrent.duration.*

import utest.*

/** [[Allowlist]]'s rules and bounds, and [[Allowlisted]] over a fake [[Runs]]: what it lets
  * reach the runs, and for how long.
  */
object AllowlistTests extends TestSuite {

  /** Runs that note each start's command and timeout, and answer as [[ScriptedRuns]] over
    * `scripts` does. A fake of a stateful capability,
    * so it may hold its log (STYLE rule 7).
    */
  final class Noting(scripts: Map[Argv, ScriptedRuns.Script]) extends Runs {
    private val runs = new ScriptedRuns(scripts)

    // Replaced whole by the calls the test makes on its one thread.
    @caps.unsafe.untrackedCaptures
    var log = Vector.empty[(Argv, FiniteDuration)]

    def start(
        argv: Argv,
        timeout: FiniteDuration,
        wait: FiniteDuration
    ): Either[HostError, (id: RunId, status: RunStatus)] = {
      log = log :+ ((argv, timeout))
      runs.start(argv, timeout, wait)
    }

    def tail(id: RunId, wait: FiniteDuration): Either[HostError, (argv: Argv, status: RunStatus)] =
      runs.tail(id, wait)

    def stop(id: RunId): Either[HostError, (argv: Argv, status: RunStatus)] = runs.stop(id)
  }

  private val instant = ScriptedRuns.Script("", "", Some(Duration.Zero), 0)

  private def argv(words: String*): Argv =
    Argv.of(words.head, words.tail*).fold(e => throw new java.lang.AssertionError(e), identity)

  private def allowlist(longest: FiniteDuration, rules: Allowlist.Rule*): Allowlist =
    Allowlist.of(rules.toVector, longest).fold(e => throw new java.lang.AssertionError(e), identity)

  private val gitStatus = Allowlist.Rule.Exactly(argv("git", "status"))
  private val gitDiff = Allowlist.Rule.Leading(argv("git", "diff"))

  val tests = Tests {
    test("Exactly permits its argv alone; Leading its argv and any more; the program as written") {
      val list = allowlist(1.minute, gitStatus, gitDiff)
      Vector(
        argv("git", "status"),
        argv("git", "status", "-s"),
        argv("/usr/bin/git", "status"),
        argv("git"),
        argv("git", "diff"),
        argv("git", "diff", "--stat", "HEAD"),
        argv("git", "dif"),
        argv("git", "log", "diff")
      ).map(list.permits) ==> Vector(true, false, false, false, true, true, false, false)
    }

    test("of refuses no rules, and a longest under a second or past Runs.Longest") {
      (
        Allowlist.of(Vector.empty, 1.minute),
        Allowlist.of(Vector(gitStatus), 999.millis),
        Allowlist.of(Vector(gitStatus), Runs.Longest + 1.second),
        Allowlist.of(Vector(gitStatus), Shell.Longest + 1.second).map(_.longest),
        Allowlist.of(Vector(gitStatus), Runs.Longest).map(_.longest)
      ) ==> (
        Left("an allowlist names no command"),
        Left(
          "an allowlist's longest run must be at least a second and at most 60 minutes, not 999 milliseconds"
        ),
        Left(
          "an allowlist's longest run must be at least a second and at most 60 minutes, not 3601 seconds"
        ),
        Right(Shell.Longest + 1.second),
        Right(Runs.Longest)
      )
    }

    test(
      "an unlisted command is refused and never reaches the runs; a listed one starts there"
    ) {
      val noted = new Noting(Map(argv("git", "diff", "--stat") -> instant))
      val runs = Allowlisted.over(noted, allowlist(1.minute, gitStatus, gitDiff))
      val unlisted = argv("rm", "-rf", "x")
      (
        runs.start(unlisted, 5.seconds, 1.second),
        runs.start(argv("git", "status", "--porcelain"), 5.seconds, 1.second),
        runs.start(argv("git", "diff", "--stat"), 5.seconds, 1.second).map(_.status)
      ) ==> (
        Left(HostError.Unlisted(unlisted)),
        Left(HostError.Unlisted(argv("git", "status", "--porcelain"))),
        Right(RunStatus.Ended(Ran(0, Clipped.tail("", _ => None))))
      )
      noted.log ==> Vector((argv("git", "diff", "--stat"), 5.seconds))
    }

    test("a listed command runs for its timeout, but never past the allowlist's longest") {
      val noted = new Noting(Map(argv("git", "diff") -> instant))
      val runs = Allowlisted.over(noted, allowlist(2.minutes, gitDiff))
      val _ = runs.start(argv("git", "diff"), 30.seconds, 1.second)
      val _ = runs.start(argv("git", "diff"), 20.minutes, 1.second)
      noted.log.map(_._2) ==> Vector(30.seconds, 2.minutes)
    }

    test("written single-quotes a word holding a space, a quote, a shell character, or nothing") {
      argv("git", "commit", "-m", "a b", "", "it's", "$HOME", "x|y", "--amend").written ==>
        """git commit -m 'a b' '' 'it'\''s' '$HOME' 'x|y' --amend"""
    }

    test(
      "Following permits its argv only with more after it; bare, it is refused saying what to add"
    ) {
      val check = Allowlist.Rule.Following(argv("scripts/check"), "the suites or modules to check")
      val noted = new Noting(Map(argv("scripts/check", "grit.core") -> instant))
      val runs = Allowlisted.over(noted, allowlist(1.minute, gitStatus, check))
      (
        check.permits(argv("scripts/check")),
        check.permits(argv("scripts/check", "grit.core")),
        runs.start(argv("scripts/check"), 5.seconds, 1.second),
        runs.start(argv("scripts/check", "grit.core"), 5.seconds, 1.second).isRight,
        check.listed,
        gitStatus.listed
      ) ==> (
        false,
        true,
        Left(HostError.Unfinished(argv("scripts/check"), "the suites or modules to check")),
        true,
        "`scripts/check …` (at least one argument: the suites or modules to check)",
        "`git status`"
      )
    }

    test("a command run without the arguments its rule needs is refused, saying which to name") {
      // What the model reads, word for word.
      HostError.Unfinished(argv("scripts/check"), "the suites or modules to check").message ==>
        "`scripts/check` runs only with at least one argument after it, the suites or modules " +
        "to check, so nothing ran. Name them, as the run tool's description lists."
    }

    test("an unlisted command's refusal names it and says nothing ran") {
      // What the model reads, word for word.
      HostError.Unlisted(argv("rm", "-rf", "my dir")).message ==>
        "`rm -rf 'my dir'` is not on this checkout's command list, so nothing ran. Only the " +
        "commands the run tool's description lists may run."
    }
  }
}
