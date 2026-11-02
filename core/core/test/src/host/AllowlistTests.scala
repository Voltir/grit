package grit.core.host

import scala.concurrent.duration.*

import utest.*

/** [[Allowlist]]'s rules and bounds, and [[Allowlisted]] over a fake [[Processes]]: what it lets
  * reach the processes, and for how long.
  */
object AllowlistTests extends TestSuite {

  /** Processes that note each run and answer it with `ran`. A fake of a stateful capability,
    * so it may hold its log (STYLE rule 7).
    */
  final class Noting(ran: Ran = Ran(0, Clipped.tail("", _ => None))) extends Processes {
    // Replaced whole by the calls the test makes on its one thread.
    @caps.unsafe.untrackedCaptures
    var log = Vector.empty[(Argv, RelPath, FiniteDuration)]

    def run(argv: Argv, dir: RelPath, timeout: FiniteDuration): Either[HostError, Ran] = {
      log = log :+ ((argv, dir, timeout))
      Right(ran)
    }

    def started[A](argv: Argv, dir: RelPath)(body: Started^ => A): Either[HostError, A] =
      Left(HostError.Failed("not started here"))
  }

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

    test("of refuses no rules, and a longest under a second or past Shell.Longest") {
      (
        Allowlist.of(Vector.empty, 1.minute),
        Allowlist.of(Vector(gitStatus), 999.millis),
        Allowlist.of(Vector(gitStatus), Shell.Longest + 1.second).isLeft,
        Allowlist.of(Vector(gitStatus), Shell.Longest).map(_.longest)
      ) ==> (
        Left("an allowlist names no command"),
        Left(
          "an allowlist's longest run must be at least a second and at most 10 minutes, not 999 milliseconds"
        ),
        true,
        Right(Shell.Longest)
      )
    }

    test(
      "an unlisted command is refused and never reaches the processes; a listed one runs at the root"
    ) {
      val processes = new Noting
      val runs = Allowlisted.over(processes, allowlist(1.minute, gitStatus, gitDiff))
      val unlisted = argv("rm", "-rf", "x")
      (
        runs.run(unlisted, 5.seconds),
        runs.run(argv("git", "status", "--porcelain"), 5.seconds),
        runs.run(argv("git", "diff", "--stat"), 5.seconds).map(_.exit)
      ) ==> (
        Left(HostError.Unlisted(unlisted)),
        Left(HostError.Unlisted(argv("git", "status", "--porcelain"))),
        Right(0)
      )
      processes.log ==> Vector((argv("git", "diff", "--stat"), RelPath.Root, 5.seconds))
    }

    test("a listed command runs for its timeout, but never past the allowlist's longest") {
      val processes = new Noting
      val runs = Allowlisted.over(processes, allowlist(2.minutes, gitDiff))
      val _ = runs.run(argv("git", "diff"), 30.seconds)
      val _ = runs.run(argv("git", "diff"), 20.minutes)
      processes.log.map(_._3) ==> Vector(30.seconds, 2.minutes)
    }

    test("written single-quotes a word holding a space, a quote, a shell character, or nothing") {
      argv("git", "commit", "-m", "a b", "", "it's", "$HOME", "x|y", "--amend").written ==>
        """git commit -m 'a b' '' 'it'\''s' '$HOME' 'x|y' --amend"""
    }

    test("an unlisted command's refusal names it and says nothing ran") {
      // What the model reads, word for word.
      HostError.Unlisted(argv("rm", "-rf", "my dir")).message ==>
        "`rm -rf 'my dir'` is not on this checkout's command list, so nothing ran. Only the " +
        "commands the run tool's description lists may run."
    }
  }
}
