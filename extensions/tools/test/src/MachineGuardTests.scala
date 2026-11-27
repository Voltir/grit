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
    "`git status …`, `git diff …`, `git log …`, `git show …`, `ls …`, `cat …`, `head …`, " +
      "`tail …`, `wc …`, `grep …`, `diff …`, `find …`, `sed -n …`, `git add …`, " +
      "`git commit …`, `git restore …`, `git checkout -- …`, `git stash push …`, " +
      "`git stash list …`, `git stash show …`, `git stash apply …`, `git stash`"

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
        outside("tar", "--file=/tmp/x"),
        outside("tar", "--file=../x"),
        outside("git", "-C/elsewhere", "status"),
        outside("tar", "-xf/a", "-o../x"),
        outside("touch", "--reference=~/y")
      ) ==> (
        "`--file=/tmp/x` leads to /tmp/x",
        "`--file=../x` leads to /home/coder/work/x",
        "`-C/elsewhere` leads to /elsewhere",
        "`-xf/a` leads to /a\n`-o../x` leads to /home/coder/work/x",
        "`--reference=~/y` leads to /home/coder/y"
      )
    }

    test(
      "`~` and `$HOME` read as the home directory, a link out is followed, another variable is unread"
    ) {
      (
        outside("touch", "~"),
        outside("touch", "$HOME/.ssh/k", "${HOME}"),
        outside("touch", "link-out/x"),
        outside("touch", "$FOO/x", "~bob/y")
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
      val rm = argv("rm", "out/x")
      val unavailable =
        guard(Right(new Judging(Left(ClassifierError.Unavailable("529 overloaded"))))).judge(rm)
      val none = guard(Left("JEV_API_KEY is not set")).judge(rm)
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

    test(
      "a read leading outside the checkout, naming a variable or a secrets file is refused by code, nothing asked"
    ) {
      val jev = new Judging(Right(clean))
      val g = guard(Right(jev))
      val lead =
        "Nothing ran: the check that guards this machine keeps reads inside the checkout, and"
      val inside =
        "Read what is inside the checkout, with `read`, `list` and `search` or these programs."
      (
        Vector(
          argv("cat", "~/.gitconfig"),
          argv("git", "diff", "--no-index", "~/.ssh/id_rsa", "/dev/null"),
          argv("git", "log", "--", "/outside"),
          argv("ls", "$FOO"),
          argv("grep", "-r", "key", "config/.env.local"),
          argv("head", "link-in/../.env")
        ).map(g.judge(_).why),
        jev.states
      ) ==> (
        Vector(
          Some(
            s"$lead `cat '~/.gitconfig'` reads outside it: `~/.gitconfig` leads to " +
              s"/home/coder/.gitconfig. $inside"
          ),
          Some(
            s"$lead `git diff --no-index '~/.ssh/id_rsa' /dev/null` reads outside it: " +
              s"`~/.ssh/id_rsa` leads to /home/coder/.ssh/id_rsa, `/dev/null` leads to /dev/null. $inside"
          ),
          Some(
            s"$lead `git log -- /outside` reads outside it: `/outside` leads to /outside. $inside"
          ),
          Some(s"$lead `ls '$$FOO'` names a variable the check cannot read: `$$FOO`. $inside"),
          Some(
            s"$lead `grep -r key config/.env.local` reads a secrets file: `config/.env.local` " +
              "names a secrets file (.env or .env.*), which tools may not touch."
          ),
          Some(
            s"$lead `head link-in/../.env` reads a secrets file: `link-in/../.env` names a " +
              "secrets file (.env or .env.*), which tools may not touch."
          )
        ),
        Vector.empty
      )
    }

    test("a read's pattern, script and name tests are no paths, and a read inside starts unasked") {
      val jev = new Judging(Right(Vector(0.9, 0.9, 0.9, 0.9)))
      val g = guard(Right(jev))
      (
        Vector(
          argv("grep", "-rn", "/api/v1", "src"),
          argv("grep", "-e", "~x", "src/a.scala"),
          argv("sed", "-n", "/^def /p", "src/a.scala"),
          argv("sed", "-n", "10,40p", "src/a.scala"),
          argv("find", "src", "-path", "/x/*", "-name", "*.scala"),
          argv("git", "log", "--grep=/fix", "-S/api"),
          argv("git", "restore", "--source", "HEAD~1", "src/a"),
          argv("git", "checkout", "--", "src/a")
        ).map(g.judge),
        jev.states
      ) ==> (Vector.fill(8)(Verdict.Cleared), Vector.empty)
    }

    test(
      "a find that writes or runs a program, and a sed but `sed -n` printing lines, are refused by code, naming what runs"
    ) {
      val jev = new Judging(Right(clean))
      val g = guard(Right(jev))
      val find =
        "Nothing ran: the check that guards this machine runs `find` only to list what it finds: " +
          "with no `-exec`, `-execdir`, `-ok`, `-okdir`, `-delete`, `-fprint`, `-fprint0`, " +
          "`-fprintf` or `-fls`. Change files with `edit` and `write`."
      val sed =
        "Nothing ran: the check that guards this machine runs `sed` only as `sed -n <lines>p " +
          "<files>`, printing a line range (`10,40p`, `5p`, `$p`) or the lines a pattern " +
          "matches (`/x/p`, `/x/,/y/p`). Change files with `edit` and `write`."
      (
        Vector(
          argv("find", ".", "-name", "*.tmp", "-delete"),
          argv("find", "src", "-execdir", "rm", "{}", ";"),
          argv("find", ".", "-fprintf", "out/x", "%p"),
          argv("sed", "-i", "s/a/b/", "src/x"),
          argv("sed", "-n", "s/a/b/w out/x", "src/y"),
          argv("sed", "-n", "-e", "1p", "src/y")
        ).map(g.judge(_).why),
        jev.states
      ) ==> (
        Vector(Some(find), Some(find), Some(find), Some(sed), Some(sed), Some(sed)),
        Vector.empty
      )
    }

    test(
      "ask puts its questions about a command to the guard's classifier, shown with no outside; with none, Unavailable saying why"
    ) {
      val jev = new Judging(Right(Vector(0.8)))
      val q = grit.core.classify.Ask.yesNo[MachineGuard.State]("Is it?", None, None)
      (
        guard(Right(jev)).ask(argv("cat", "a/B.scala"), q).map(_.value),
        jev.states,
        guard(Left("JEV_API_KEY is not set")).ask(argv("cat", "a"), q).map(_.value)
      ) ==> (
        Right(0.8),
        Vector(ujson.Obj("command" -> "cat a/B.scala", "checkout" -> "/home/coder/work/co")),
        Left(ClassifierError.Unavailable("JEV_API_KEY is not set"))
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
