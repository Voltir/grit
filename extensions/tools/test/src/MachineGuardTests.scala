package grit.tools

import grit.core.classify.ClassifierError
import grit.core.host.{Allowlist, Argv, Stashes}
import grit.tools.MachineGuard.Verdict

import utest.*

/** [[MachineGuard]] over [[GuardFixtures]]' checkout `/home/coder/work/co`: what code clears, what
  * it asks, how an answer reads, and the stash.
  */
object MachineGuardTests extends TestSuite {
  import GuardFixtures.*

  private def argv(words: String*): Argv =
    Argv.of(words.head, words.tail*).fold(e => throw new java.lang.AssertionError(e), identity)

  private val clean: Vector[Double] = Vector(0.01, 0.01, 0.01, 0.01)

  /** What `words` sent the classifier as `outside`, judged by one finding no harm. */
  private def outside(words: String*): String = {
    val jev = new Judging(Right(clean))
    val _ = guard(Right(jev)).judge(argv(words*))
    jev.states.map(_("outside").str).mkString
  }

  private val unasked =
    "`git status …`, `git diff …`, `git log …`, `git show …`, `git add …`, `git commit …`, " +
      "`git stash push …`, `git stash list …`, `git stash show …`, `git stash apply …`, `ls …`, " +
      "`cat …`, `head …`, `tail …`, `wc …`, `grep …`, `diff …`, `git stash`"

  private def shared(branch: Option[String], entries: Option[String]*): Held =
    new Held(Right(Stashes.Held(true, branch, entries.toVector)))

  val tests = Tests {
    test("a command of a cleared shape whose every argument leads inside starts, nothing asked") {
      val jev = new Judging(Right(Vector(0.9, 0.9, 0.9, 0.9)))
      val build = Allowlist.Rule.Leading(argv("./mill"))
      val g = guard(Right(jev), cleared = Vector(build))
      val verdicts = Vector(
        argv("git", "diff", "src", "--stat"),
        argv("cat", "link-in/x"),
        argv("ls", "a/../src"),
        argv("git", "commit", "-m", "fix ~ and $5"),
        argv("./mill", "grit.core.test")
      ).map(g.judge)
      (verdicts, jev.states) ==> (Vector.fill(5)(Verdict.Cleared), Vector.empty)
    }

    test("an option's value is a word of its own: after `=`, or glued from its first `/` or `~`") {
      (
        outside("git", "diff", "--output=/tmp/x"),
        outside("git", "log", "--output=../x"),
        outside("git", "-C/elsewhere", "status"),
        outside("tar", "-xf/a", "-o../x"),
        outside("grep", "-r", "x", "--include=~/y")
      ) ==> (
        "`--output=/tmp/x` leads to /tmp/x",
        "`--output=../x` leads to /home/coder/work/x",
        "`-C/elsewhere` leads to /elsewhere",
        "`-xf/a` leads to /a\n`-o../x` leads to /home/coder/work/x",
        "`--include=~/y` leads to /home/coder/y"
      )
    }

    test(
      "`~` and `$HOME` read as the home directory, a link out is followed, another variable is unread"
    ) {
      (
        outside("cat", "~"),
        outside("cat", "$HOME/.ssh/k", "${HOME}"),
        outside("cat", "link-out/x"),
        outside("cat", "$FOO/x", "~bob/y")
      ) ==> (
        "`~` leads to /home/coder",
        "`$HOME/.ssh/k` leads to /home/coder/.ssh/k\n`${HOME}` leads to /home/coder",
        "`link-out/x` leads to /home/coder/x",
        "`$FOO/x` names a variable that cannot be read\n`~bob/y` names a variable that cannot be read"
      )
    }

    test(
      "a command whose every argument leads inside is not asked whether it changes files outside, nor shown where they lead"
    ) {
      val jev = new Judging(Right(Vector(0.9, 0.01, 0.01, 0.01)))
      val g = guard(Right(jev))
      val inside = g.judge(argv("rm", "-rf", "a/../b"))
      val out = g.judge(argv("rm", "-rf", "../b"))
      (inside.why, out.why.isDefined, jev.states.map(_.obj.keySet.toSet), jev.asked) ==> (
        None,
        true,
        Vector(Set("command", "checkout"), Set("command", "checkout", "outside")),
        Vector(3, 4)
      )
    }

    test(
      "a harm at Refuses is refused in words naming it and where the command led; just under, it starts"
    ) {
      val at = guard(Right(new Judging(Right(Vector(0.3, 0.01, 0.01, 0.3)))))
      val under = guard(Right(new Judging(Right(Vector(0.29, 0.29, 0.29, 0.29)))))
      val rm = argv("rm", "-rf", "~/x")
      (at.judge(rm).why, under.judge(rm).why) ==> (
        Some(
          "Nothing ran: the check that guards this machine judged that this command would change " +
            "files outside the checkout (`~/x` leads to /home/coder/x) and send or change " +
            "something on another machine. Commands here may change only what is inside the " +
            "checkout /home/coder/work/co: rewrite it to stay inside, or leave that step to a person."
        ),
        None
      )
    }

    test(
      "a command needing a classifier that gives no answer is refused: try again when unavailable, not at all when there is none"
    ) {
      val find = argv("find", "src", "-name", "x")
      val unavailable =
        guard(Right(new Judging(Left(ClassifierError.Unavailable("529 overloaded"))))).judge(find)
      val none = guard(Left("JEV_API_KEY is not set")).judge(find)
      val head =
        s"Nothing ran: the check that guards this machine clears by itself only $unasked, each " +
          "with every argument inside the checkout, and this command is not one of them."
      (unavailable, none) ==> (
        Verdict.Unjudged(
          Some(ClassifierError.Unavailable("529 overloaded")),
          s"$head The check could not be asked just now (unavailable: 529 overloaded). Try " +
            "again shortly, or use one of those."
        ),
        Verdict.Unjudged(
          None,
          s"$head No check is configured here to ask (JEV_API_KEY is not set), so it will be " +
            "refused again: do not retry it."
        )
      )
    }

    test("in a clone of its own, a stash's pop, drop and clear start, nothing asked") {
      val jev = new Judging(Right(Vector(0.9, 0.9, 0.9, 0.9)))
      val g = guard(Right(jev))
      (
        Vector(
          argv("git", "stash", "pop"),
          argv("git", "stash", "drop", "stash@{3}"),
          argv("git", "stash", "clear")
        )
          .map(g.judge),
        jev.states
      ) ==> (Vector.fill(3)(Verdict.Cleared), Vector.empty)
    }

    test(
      "in a shared clone, pop and drop start for this branch's entry, and refuse another's naming this branch's own"
    ) {
      val stash = shared(Some("mine"), Some("other"), Some("mine"), None)
      val g = guard(Right(new Judging(Right(clean))), stash)
      val apply = "`git stash apply` restores an entry without taking it off."
      val lead =
        "Nothing ran: the check that guards this machine refused it: this checkout shares its " +
          "git stash with the repository's other checkouts, so it may take off only entries " +
          "made on its own branch, `mine`."
      (
        g.judge(argv("git", "stash", "pop", "--index", "stash@{1}")),
        g.judge(argv("git", "stash", "drop", "1")),
        g.judge(argv("git", "stash", "pop")).why,
        g.judge(argv("git", "stash", "drop", "stash@{2}")).why,
        g.judge(argv("git", "stash", "pop", "stash@{7}")).why
      ) ==> (
        Verdict.Cleared,
        Verdict.Cleared,
        Some(s"$lead `stash@{0}` was made on `other`. This branch's entries: stash@{1}. $apply"),
        Some(s"$lead `stash@{2}` names no branch. This branch's entries: stash@{1}. $apply"),
        Some(s"$lead There is no `stash@{7}`. This branch's entries: stash@{1}. $apply")
      )
    }

    test("in a shared clone, clear is refused, and so is any entry taken on no branch") {
      val clear = guard(Right(new Judging(Right(clean))), shared(Some("mine"), Some("other")))
        .judge(argv("git", "stash", "clear"))
      val detached = guard(Right(new Judging(Right(clean))), shared(None, None))
        .judge(argv("git", "stash", "pop"))
      val lead =
        "Nothing ran: the check that guards this machine refused it: this checkout shares its " +
          "git stash with the repository's other checkouts, so it may take off only entries " +
          "made on its own branch"
      (clear.why, detached.why) ==> (
        Some(
          s"$lead, `mine`, and `git stash clear` would remove every checkout's entries. This " +
            "branch has no entries. Drop them one at a time with `git stash drop stash@{n}`."
        ),
        Some(
          s"$lead, and it is on no branch. `git stash apply` restores an entry without taking it off."
        )
      )
    }
  }
}
