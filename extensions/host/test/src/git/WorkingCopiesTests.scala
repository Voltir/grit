package grit.host.git

import java.nio.file.{Files, Path}
import java.time.Instant

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

  val tests = Tests {
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
  }
}
