package grit.host.git

import java.nio.file.Files

import grit.core.store.CheckoutState
import grit.host.checkout.{LocalProcesses, Redaction}
import grit.host.git.GitFixture.*

import utest.*

/** [[BranchDiff]] over the fixture's clone, its origin's default branch `origin/main`. */
object BranchDiffTests extends TestSuite {

  private def read(repo: Repo^) =
    BranchDiff.read(new LocalProcesses(repo.source, repo.environment, Redaction.of(Map.empty)))

  private val commit =
    Seq(
      "-c",
      "user.name=grit",
      "-c",
      "user.email=grit@example.invalid",
      "-c",
      "commit.gpgsign=false"
    )

  val tests = Tests {
    test(
      "a checkout's diff is its changes since origin's default branch, committed or not, and its untracked files, ignored ones left out"
    ) {
      inRepository { repo =>
        Files.writeString(repo.source.resolve("a.txt"), "one\n")
        Files.writeString(repo.source.resolve(".gitignore"), "*.log\n")
        val _ = git(repo, repo.source, "add", "a.txt", ".gitignore")
        val _ = git(repo, repo.source, (commit ++ Seq("commit", "--quiet", "-m", "a"))*)
        Files.writeString(repo.source.resolve("a.txt"), "one\ntwo\n")
        Files.writeString(repo.source.resolve("new.txt"), "new\n")
        Files.writeString(repo.source.resolve("run.log"), "noise\n")
        read(repo) ==> Right(
          CheckoutState.Diff.of(
            "origin/main",
            " .gitignore | 1 +\n a.txt      | 2 ++\n 2 files changed, 3 insertions(+)\n",
            Vector("new.txt")
          )
        )
      }
    }

    test("a diff of more files than its bound names its first, then git's ' ...' and its totals") {
      inRepository { repo =>
        (1 to CheckoutState.StatFiles + 5).foreach(i =>
          Files.writeString(repo.source.resolve(f"f$i%02d.txt"), "x\n")
        )
        val _ = git(repo, repo.source, "add", ".")
        val lines = read(repo).map(_.stat.split("\n").toVector)
        (
          lines.map(_.size),
          lines.map(_.lift(CheckoutState.StatFiles)),
          lines.map(_.lastOption)
        ) ==> (
          Right(CheckoutState.StatFiles + 2),
          Right(Some(CheckoutState.Elided)),
          Right(Some(s" ${CheckoutState.StatFiles + 5} files changed, 35 insertions(+)"))
        )
      }
    }

    test("a checkout naming no default branch of origin has no diff") {
      inRepository { repo =>
        val _ = git(repo, repo.source, "remote", "set-head", "origin", "--delete")
        read(repo).left.map(_.message) ==> Left(
          "The checkout names no default branch of origin (refs/remotes/origin/HEAD)."
        )
      }
    }
  }
}
