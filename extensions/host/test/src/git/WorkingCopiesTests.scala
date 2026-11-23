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
        val target = PushTarget.of(new FakeRemoteRepository(repo.origin.toString)) match {
          case Right(t) => t
          case Left(why) => throw new java.lang.AssertionError(why)
        }
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
        val target = PushTarget.of(new FakeRemoteRepository(repo.origin.toString)) match {
          case Right(t) => t
          case Left(why) => throw new java.lang.AssertionError(why)
        }
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
