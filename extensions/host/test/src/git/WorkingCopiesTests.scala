package grit.host.git

import java.nio.file.{Files, Path}
import java.time.Instant

import scala.jdk.CollectionConverters.*

import grit.core.clock.SetClock
import grit.core.host.HostError
import grit.host.git.GitFixture.*

import utest.*

/** [[WorkingCopies]] over a real git repository. */
object WorkingCopiesTests extends TestSuite {

  private val start = Instant.parse("2026-11-01T00:00:00Z")

  /** Copies of `repo`'s clone on `clock`. */
  private def over(repo: Repo^, clock: SetClock): WorkingCopies^ =
    new WorkingCopies(repository, repo.source, repo.root, repo.processes, clock)

  /** `copies`' copy of `name`, which must be made. */
  private def made(copies: WorkingCopies^, name: String): Path =
    copies.opened(work(name)) match {
      case Right(p) => p
      case Left(e) => throw new java.lang.AssertionError(s"$name: ${e.message}")
    }

  /** `grit/{name}` in `repo`'s clone, `commits` empty commits past main, the clone left on main:
    * its tip.
    */
  private def branched(repo: Repo^, name: String, commits: Int): String = {
    val _ = git(repo, repo.source, "checkout", "--quiet", "-B", s"grit/$name", "main")
    for (_ <- 1 to commits) {
      val _ = git(
        repo,
        repo.source,
        "-c",
        "user.name=t",
        "-c",
        "user.email=t@example.com",
        "commit",
        "--quiet",
        "--allow-empty",
        "-m",
        name
      )
    }
    val tip = git(repo, repo.source, "rev-parse", "HEAD").output.text.trim
    val _ = git(repo, repo.source, "checkout", "--quiet", "main")
    tip
  }

  /** `grit/{name}` in `repo`'s clone, one commit past main committed at `epoch` seconds: its tip.
    */
  private def dated(repo: Repo^, name: String, epoch: Long): String = {
    val tree = git(repo, repo.source, "rev-parse", "main^{tree}").output.text.trim
    val parent = git(repo, repo.source, "rev-parse", "main").output.text.trim
    val made = repo.root.resolveSibling(s"commit-$name")
    Files.writeString(
      made,
      s"tree $tree\nparent $parent\nauthor t <t@example.com> $epoch +0000\n" +
        s"committer t <t@example.com> $epoch +0000\n\n$name\n"
    )
    val tip =
      git(repo, repo.source, "hash-object", "-t", "commit", "-w", made.toString).output.text.trim
    val _ = git(repo, repo.source, "update-ref", s"refs/heads/grit/$name", tip)
    tip
  }

  /** Copies of `repo`'s clone on `clock`, pushed and offered on `remote`, its sweep's lines
    * said through `note`.
    */
  private def swept(
      repo: Repo^,
      remote: FakeRemoteRepository,
      clock: SetClock,
      note: String -> Unit = _ => ()
  ): WorkingCopies^ =
    new WorkingCopies(
      repository,
      repo.source,
      repo.root,
      repo.processes,
      clock,
      Some(branchesOf(repo, remote)),
      note
    )

  /** The names of the copies under `repo`'s root, sorted. */
  private def present(repo: Repo^): Vector[String] = {
    val listing = Files.list(repo.root.resolve("grit"))
    try
      listing.iterator.asScala
        .filter(Files.isDirectory(_))
        .map(_.getFileName.toString)
        .toVector
        .sorted
    finally listing.close()
  }

  /** `names`, each held by `copies` around the next, `body` run inside the last. */
  private def allHeld[A](copies: WorkingCopies^, names: List[String])(body: => A): A =
    names match {
      case Nil => body
      case n :: rest =>
        copies.held(work(n))(_ => allHeld(copies, rest)(body)) match {
          case Right(a) => a
          case Left(e) => throw new java.lang.AssertionError(s"$n: ${e.message}")
        }
    }

  /** Whether the kernel has `lock` locked by this process: a hold the JVM still records but the
    * system has dropped would let another process remove the copy.
    */
  private def lockedHere(lock: Path): Boolean = {
    val pid = ProcessHandle.current.pid.toString
    val inode = Files.getAttribute(lock, "unix:ino").toString
    Files.readAllLines(Path.of("/proc/locks")).asScala.exists { line =>
      val fields = line.trim.split("\\s+")
      fields.length > 5 && fields(4) == pid && fields(5).endsWith(s":$inode")
    }
  }

  /** `files` committed at the clone's root, pushed to origin's default branch and fetched. */
  private def published(repo: Repo^, files: (String, String)*): Unit = {
    for ((name, text) <- files) {
      val _ = Files.writeString(repo.source.resolve(name), text)
      val _ = git(repo, repo.source, "add", name)
    }
    val commit = Seq("-c", "user.name=g", "-c", "user.email=g@example.invalid")
    val _ = git(repo, repo.source, (commit ++ Seq("commit", "-qm", "instructions"))*)
    val _ = git(repo, repo.source, "push", "--quiet", "origin", "main")
    val _ = git(repo, repo.source, "fetch", "--quiet", "origin")
  }

  /** What [[WorkingCopies.instructionFile]] read: its name, text and whether it was cut. */
  private def instructed(repo: Repo^): Either[HostError, Option[(String, String, Boolean)]] =
    over(repo, new SetClock(start)).instructionFile().map(_.map(f => (f.name, f.text, f.cut)))

  private val eight = (0 until WorkingCopies.Most).toList.map(i => s"run-$i")

  val tests = Tests {
    test(
      "the first call removes a copy unused for Kept, its branch kept; one used since, or holding changes, stays"
    ) {
      inRepository { repo =>
        val clock = new SetClock(start)
        val earlier = over(repo, clock)
        val _ = made(earlier, "old")
        Files.writeString(made(earlier, "draft").resolve("draft.txt"), "unsaved")
        clock.at = start.plusSeconds(6 * 86400)
        val _ = made(earlier, "recent")
        clock.at = start.plusMillis(WorkingCopies.Kept.toMillis).plusSeconds(3600)
        val _ = made(over(repo, clock), "next")
        (present(repo), hasBranch(repo, repo.source, "grit/old")) ==> (
          Vector("draft", "next", "recent"),
          true
        )
      }
    }

    test(
      "the first call removes a copy holding changes never committed once unused past Unpublished, said first"
    ) {
      inRepository { repo =>
        val clock = new SetClock(start)
        val noted = scala.collection.mutable.ArrayBuffer.empty[String]
        def copies(): WorkingCopies^ =
          new WorkingCopies(
            repository,
            repo.source,
            repo.root,
            repo.processes,
            clock,
            None,
            noted += _
          )
        val draft = made(copies(), "draft")
        Files.writeString(draft.resolve("draft.txt"), "unsaved")
        // The write stamps the copy with the machine's time; its last use is the clock's.
        val _ = Files.setLastModifiedTime(draft, java.nio.file.attribute.FileTime.from(start))
        clock.at = start.plusMillis(WorkingCopies.Unpublished.toMillis).minusSeconds(3600)
        val _ = made(copies(), "a")
        val before = present(repo)
        clock.at = start.plusMillis(WorkingCopies.Unpublished.toMillis).plusSeconds(3600)
        val _ = made(copies(), "b")
        (before, present(repo), hasBranch(repo, repo.source, "grit/draft"), noted.toVector) ==> (
          Vector("a", "draft"),
          Vector("a", "b"),
          true,
          Vector(
            s"[main] removing $draft, its changes never committed, unused past ${WorkingCopies.Unpublished}"
          )
        )
      }
    }

    test("the first call removes a directory left without its lock") {
      inRepository { repo =>
        val stray = Files.createDirectories(repo.root.resolve("grit").resolve("stray"))
        Files.writeString(stray.resolve("left.txt"), "left")
        val _ = made(over(repo, new SetClock(start)), "next")
        present(repo) ==> Vector("next")
      }
    }

    test("a run's own directory left without its lock is made again as its copy") {
      inRepository { repo =>
        val copies = over(repo, new SetClock(start))
        val _ = made(copies, "first")
        val left = Files.createDirectories(repo.root.resolve("grit").resolve("fix-a"))
        Files.writeString(left.resolve("left.txt"), "left")
        val again = made(copies, "fix-a")
        val branch = git(repo, again, "rev-parse", "--abbrev-ref", "HEAD").output.text.trim
        (branch, Files.exists(again.resolve("left.txt"))) ==> ("grit/fix-a", false)
      }
    }

    test("at Most copies, the least recently used one is removed for a new copy, its branch kept") {
      inRepository { repo =>
        val clock = new SetClock(start)
        val copies = over(repo, clock)
        eight.foreach { n =>
          val _ = made(copies, n); clock.at = clock.at.plusSeconds(60)
        }
        val _ = made(copies, "run-0")
        clock.at = clock.at.plusSeconds(60)
        val ninth = copies.opened(work("run-8"))
        (ninth.isRight, present(repo), hasBranch(repo, repo.source, "grit/run-1")) ==> (
          true,
          (eight.filterNot(_ == "run-1") :+ "run-8").sorted.toVector,
          true
        )
      }
    }

    test("when every copy is held, none is removed, a new one is refused, and every hold stands") {
      inRepository { repo =>
        val copies = over(repo, new SetClock(start))
        val (refused, standing) = allHeld(copies, eight) {
          val r = copies.opened(work("run-8"))
          (r, eight.filter(n => lockedHere(repo.root.resolve("grit").resolve(s"$n.lock"))))
        }
        (refused, present(repo), standing) ==> (
          Left(HostError.NoCopyFree(WorkingCopies.Most)),
          eight.toVector,
          eight
        )
      }
    }

    test("a copy holding changes never committed is not removed for a new copy") {
      inRepository { repo =>
        val copies = over(repo, new SetClock(start))
        eight.foreach(n => Files.writeString(made(copies, n).resolve("draft.txt"), "unsaved"))
        val refused = copies.opened(work("run-8"))
        (refused, present(repo)) ==> (
          Left(HostError.NoCopyFree(WorkingCopies.Most)),
          eight.toVector
        )
      }
    }

    test(
      "the repository's instruction file is origin's default branch's, never the clone's working tree or unpushed commits"
    ) {
      inRepository { repo =>
        published(repo, "AGENTS.md" -> "committed\n")
        val commit = Seq("-c", "user.name=g", "-c", "user.email=g@example.invalid")
        Files.writeString(repo.source.resolve("AGENTS.md"), "unpushed\n")
        val _ = git(repo, repo.source, (commit ++ Seq("commit", "-qam", "mine"))*)
        Files.writeString(repo.source.resolve("AGENTS.md"), "uncommitted\n")
        instructed(repo) ==> Right(Some(("AGENTS.md", "committed\n", false)))
      }
    }

    test(
      "a repository whose AGENTS.md is committed as a link gives its CLAUDE.md; one with only a link is refused, saying so"
    ) {
      inRepository { repo =>
        val _ = Files.createSymbolicLink(repo.source.resolve("AGENTS.md"), Path.of("CLAUDE.md"))
        val _ = git(repo, repo.source, "add", "AGENTS.md")
        published(repo, "CLAUDE.md" -> "the claude file\n")
        val both = instructed(repo)
        val _ = git(repo, repo.source, "rm", "--quiet", "CLAUDE.md")
        published(repo)
        (both, instructed(repo)) ==> (
          Right(Some(("CLAUDE.md", "the claude file\n", false))),
          Left(
            HostError.Failed(
              "AGENTS.md is committed at the root of grit only as a link, which is not followed."
            )
          )
        )
      }
    }

    test(
      "a repository's instruction file over MaxBytes is cut there before a character it would split, and says so"
    ) {
      inRepository { repo =>
        val most = grit.core.host.Instructions.MaxBytes
        // "é" is two bytes, its first the cap's last.
        published(repo, "AGENTS.md" -> ("a" * (most - 1) + "é and more\n"))
        instructed(repo) ==> Right(Some(("AGENTS.md", "a" * (most - 1), true)))
      }
    }

    test(
      "a repository with no instruction file at its root gives none; one holding NUL is refused as not text"
    ) {
      inRepository { repo =>
        val _ = Files.createDirectories(repo.source.resolve("sub"))
        published(repo, "sub/AGENTS.md" -> "below the root\n")
        val none = instructed(repo)
        published(repo, "CLAUDE.md" -> "half\u0000binary\n")
        (none, instructed(repo)) ==> (
          Right(None),
          Left(
            HostError.Failed("CLAUDE.md at the root of grit is not UTF-8 text, so it is not read.")
          )
        )
      }
    }

    test("a clone whose origin/HEAD names no default branch gives no instruction file, saying so") {
      inRepository { repo =>
        published(repo, "AGENTS.md" -> "committed\n")
        val _ = git(repo, repo.source, "remote", "set-head", "origin", "--delete")
        instructed(repo) ==> Left(
          HostError.Failed(
            s"origin's default branch is not named in the clone at ${repo.source.toAbsolutePath.normalize}" +
              " (it has no refs/remotes/origin/HEAD), so no instruction file is read."
          )
        )
      }
    }

    test(
      "a new copy is cut from origin's default branch as fetched, never the clone's unpushed commits"
    ) {
      inRepository { repo =>
        val commit = Seq("-c", "user.name=g", "-c", "user.email=g@example.invalid")
        // Upstream moved on since the clone last fetched; the clone holds a commit never pushed.
        val upstream = Files.createDirectories(repo.source.resolveSibling("upstream"))
        val _ = git(repo, upstream.getParent, "clone", "--quiet", repo.origin.toString, "upstream")
        val _ = git(repo, upstream, (commit ++ Seq("commit", "--allow-empty", "-qm", "theirs"))*)
        val _ = git(repo, upstream, "push", "--quiet", "origin", "main")
        val _ = git(repo, repo.source, (commit ++ Seq("commit", "--allow-empty", "-qm", "mine"))*)
        val copy = made(over(repo, new SetClock(start)), "fix-a")
        val subject = git(repo, copy, "log", "-1", "--format=%s").output.text.trim
        val mine = git(repo, copy, "log", "--format=%s").output.text.linesIterator.toVector
        (subject, mine.contains("mine")) ==> ("theirs", false)
      }
    }
    test(
      "with a target, a copy is cut when origin is its remote; another origin is refused, shown without its user, and nothing is made"
    ) {
      inRepository { repo =>
        val target = branchesOf(repo, new FakeRemoteRepository(repo.origin.toString))
        val copies =
          new WorkingCopies(
            repository,
            repo.source,
            repo.root,
            repo.processes,
            new SetClock(start),
            Some(target)
          )
        val cut = copies.opened(work("fix-a")).isRight
        val _ = git(
          repo,
          repo.source,
          "remote",
          "set-url",
          "origin",
          "https://x:ghs_secret@example.com/other.git"
        )
        val refused = copies.opened(work("fix-b")).left.map(_.message)
        (
          cut,
          refused,
          hasBranch(repo, repo.source, "grit/fix-b"),
          Files.exists(repo.root.resolve("grit").resolve("fix-b"))
        ) ==> (
          true,
          Left(
            s"The clone at ${repo.source.toAbsolutePath.normalize} fetches from " +
              s"https://example.com/other.git, not from the declared ${repo.origin}, and a run's " +
              "branch is cut only from the declared repository. Point the clone's origin there " +
              s"(git -C ${repo.source.toAbsolutePath.normalize} remote set-url origin ${repo.origin}), " +
              "or declare the repository it fetches from."
          ),
          false,
          false
        )
      }
    }
    test("an origin whose insteadOf sends git elsewhere is not the target's remote") {
      inRepository { repo =>
        val target = branchesOf(repo, new FakeRemoteRepository(repo.origin.toString))
        val _ = git(
          repo,
          repo.source,
          "config",
          "url./srv/elsewhere/.insteadOf",
          s"${repo.origin.getParent}/"
        )
        val opened =
          new WorkingCopies(
            repository,
            repo.source,
            repo.root,
            repo.processes,
            new SetClock(start),
            Some(target)
          )
            .opened(work("fix-a"))
        (
          opened.left
            .map(_.message)
            .left
            .exists(_.contains("fetches from /srv/elsewhere/origin.git")),
          hasBranch(repo, repo.source, "grit/fix-a")
        ) ==> (true, false)
      }
    }
    test("a run's copy is removed, its branch kept, unless it holds changes never committed") {
      inRepository { repo =>
        val copies = over(repo, new SetClock(start))
        val done = made(copies, "fix-a")
        val dirty = made(copies, "fix-b")
        Files.writeString(dirty.resolve("README.md"), "changed\n")
        (
          copies.removed(work("fix-a")),
          Files.exists(done),
          hasBranch(repo, repo.source, "grit/fix-a"),
          copies.removed(work("fix-b")),
          Files.exists(dirty)
        ) ==> (
          WorkingCopies.Removed.Gone,
          false,
          true,
          WorkingCopies.Removed.Uncommitted(dirty),
          true
        )
      }
    }
    test(
      "the sweep deletes a branch with no copy whose pull request was merged or closed at its tip, and keeps every other"
    ) {
      inRepository { repo =>
        val remote = new FakeRemoteRepository(repo.origin.toString)
        val merged = branched(repo, "merged", 1)
        val closed = branched(repo, "closed", 1)
        val _ = branched(repo, "past", 1)
        val _ = branched(repo, "open", 1)
        val _ = git(repo, repo.source, "branch", "other", "main")
        val _ = made(over(repo, new SetClock(start)), "held")
        val held = git(repo, repo.source, "rev-parse", "grit/held").output.text.trim
        val _ = git(repo, repo.source, "push", "--quiet", repo.origin.toString, "grit/merged")
        for (b <- Vector("merged", "closed", "past", "open", "held"))
          remote.open(s"grit/$b", "Fix", "Fixed.")
        remote.merge("grit/merged", merged)
        remote.close("grit/closed", closed)
        remote.close("grit/past", "0000000000000000000000000000000000000000")
        remote.close("grit/held", held)
        val _ = made(swept(repo, remote, new SetClock(start)), "fresh")
        (
          Vector("merged", "closed", "past", "open", "held").filter(b =>
            hasBranch(repo, repo.source, s"grit/$b")
          ),
          hasBranch(repo, repo.source, "other"),
          hasBranch(repo, repo.origin, "grit/merged")
        ) ==> (Vector("past", "open", "held"), true, true)
      }
    }
    test(
      "a branch never offered is deleted when origin's default holds all of it, or once its tip is past Unpublished, said first"
    ) {
      inRepository { repo =>
        val remote = new FakeRemoteRepository(repo.origin.toString)
        val _ = branched(repo, "empty", 0)
        val young = branched(repo, "young", 1)
        val now = Instant.now()
        val noted = scala.collection.mutable.ArrayBuffer.empty[String]
        val _ = made(swept(repo, remote, new SetClock(now)), "a")
        val first =
          (hasBranch(repo, repo.source, "grit/empty"), hasBranch(repo, repo.source, "grit/young"))
        val later = new SetClock(now.plusSeconds(WorkingCopies.Unpublished.toSeconds + 3600))
        val _ = made(swept(repo, remote, later, noted += _), "b")
        (first, hasBranch(repo, repo.source, "grit/young"), noted.map(_.split(" made ").head)) ==> (
          (false, true),
          false,
          Vector(s"[main] deleting grit/young, never offered, its last commit $young")
        )
      }
    }
    test(
      "the sweep stops at the first answer the remote repository cannot give, asks at most SweepAsks, and deletes nothing for another origin"
    ) {
      inRepository { repo =>
        val n = WorkingCopies.SweepAsks + 2
        val tips = (1 to n).map(k => dated(repo, s"e$k", start.getEpochSecond - 1000 + k))
        def closedAll(remote: FakeRemoteRepository): Unit =
          for ((tip, k) <- tips.zipWithIndex) {
            val _ = remote.open(s"grit/e${k + 1}", "Fix", "Fixed.")
            remote.close(s"grit/e${k + 1}", tip)
          }
        def left = (1 to n).count(k => hasBranch(repo, repo.source, s"grit/e$k"))
        val failing = new FakeRemoteRepository(repo.origin.toString)
        closedAll(failing)
        failing.answers.set(1)
        val _ = swept(repo, failing, new SetClock(start)).opened(work("a"))
        val afterFailing = (left, failing.asked.size)
        val counting = new FakeRemoteRepository(repo.origin.toString)
        closedAll(counting)
        val _ = swept(repo, counting, new SetClock(start)).opened(work("b"))
        val afterCounting = (left, counting.asked.size)
        val _ = git(repo, repo.source, "remote", "set-url", "origin", "/srv/elsewhere.git")
        val elsewhere = new FakeRemoteRepository(repo.origin.toString)
        val _ = swept(repo, elsewhere, new SetClock(start)).opened(work("c"))
        (afterFailing, afterCounting, (left, elsewhere.asked.size)) ==> (
          (n - 1, 2),
          (1, WorkingCopies.SweepAsks),
          (1, 0)
        )
      }
    }
    test("the sweep asks oldest first, so a branch past SweepAsks unheld ones is reached at once") {
      inRepository { repo =>
        val n = WorkingCopies.SweepAsks + 5
        val remote = new FakeRemoteRepository(repo.origin.toString)
        val now = start.getEpochSecond
        val oldest = dated(repo, "old", now - 10000)
        for (k <- 1 until n) { val _ = dated(repo, s"y$k", now - 1000 + k) }
        val _ = remote.open("grit/old", "Fix", "Fixed.")
        remote.close("grit/old", oldest)
        val _ = swept(repo, remote, new SetClock(start)).opened(work("a"))
        (hasBranch(repo, repo.source, "grit/old"), remote.asked.size, remote.asked.get(0)) ==>
          (false, WorkingCopies.SweepAsks, "grit/old")
      }
    }
    test("each sweep asks on from where the last stopped, so every branch is reached within two") {
      inRepository { repo =>
        val n = WorkingCopies.SweepAsks + 5
        val now = start.getEpochSecond
        val names = (1 to n).map { k =>
          val _ = dated(repo, s"y$k", now - 1000 + k)
          s"grit/y$k"
        }
        val first = new FakeRemoteRepository(repo.origin.toString)
        val _ = swept(repo, first, new SetClock(start)).opened(work("a"))
        val second = new FakeRemoteRepository(repo.origin.toString)
        val _ = swept(repo, second, new SetClock(start)).opened(work("b"))
        val asked = first.asked.asScala.toVector ++ second.asked.asScala.toVector
        (asked.toSet == names.toSet, second.asked.get(0)) ==>
          (true, s"grit/y${WorkingCopies.SweepAsks + 1}")
      }
    }
    test(
      "a branch origin's default holds all of is deleted without asking, and a tag of its name changes nothing"
    ) {
      inRepository { repo =>
        val n = WorkingCopies.SweepAsks + 5
        for (k <- 1 to n) { val _ = git(repo, repo.source, "branch", s"grit/e$k", "main") }
        val _ = git(repo, repo.source, "tag", "grit/e1", "main")
        val remote = new FakeRemoteRepository(repo.origin.toString)
        val _ = swept(repo, remote, new SetClock(start)).opened(work("a"))
        ((1 to n).count(k => hasBranch(repo, repo.source, s"grit/e$k")), remote.asked.size) ==> (
          0,
          0
        )
      }
    }
    test("when origin cannot be fetched, no copy and no branch are made") {
      inRepository { repo =>
        Files.move(repo.origin, repo.origin.resolveSibling("moved.git"))
        val opened = over(repo, new SetClock(start)).opened(work("fix-a"))
        (
          opened.left.map(_.message).left.exists(_.contains("fetch")),
          hasBranch(repo, repo.source, "grit/fix-a"),
          Files.exists(repo.root.resolve("grit").resolve("fix-a"))
        ) ==> (true, false, false)
      }
    }
    test("a run's copy is made on grit/{work}, and the next opened gives the same copy back") {
      inRepository { repo =>
        val copies = over(repo, new SetClock(start))
        val first = made(copies, "fix-a")
        Files.writeString(first.resolve("note.txt"), "kept")
        val branch = git(repo, first, "rev-parse", "--abbrev-ref", "HEAD").output.text.trim
        val again = copies.opened(work("fix-a"))
        (first, branch, again, Files.exists(first.resolve("note.txt"))) ==> (
          repo.root.resolve("grit").resolve("fix-a"),
          "grit/fix-a",
          Right(first),
          true
        )
      }
    }

    test(
      "a run whose copy is gone and whose branch remains is held again on its branch, its commits there"
    ) {
      inRepository { repo =>
        val copies = over(repo, new SetClock(start))
        val first = made(copies, "fix-a")
        Files.writeString(first.resolve("note.txt"), "kept")
        val _ = git(repo, first, "add", "note.txt")
        val _ = git(
          repo,
          first,
          "-c",
          "user.name=grit",
          "-c",
          "user.email=grit@example.invalid",
          "-c",
          "commit.gpgsign=false",
          "commit",
          "--quiet",
          "-m",
          "note"
        )
        val committed = git(repo, first, "rev-parse", "HEAD").output.text.trim
        val _ = git(repo, repo.source, "worktree", "remove", "--force", first.toString)
        copies.held(work("fix-a")) { copy =>
          (
            git(repo, copy, "rev-parse", "--abbrev-ref", "HEAD").output.text.trim,
            git(repo, copy, "rev-parse", "HEAD").output.text.trim,
            Files.exists(copy.resolve("note.txt"))
          )
        } ==> Right(("grit/fix-a", committed, true))
      }
    }
  }
}
