package grit.host.git

import java.nio.file.{Files, Path}

import grit.core.host.{RelPath, Uncommitted}
import grit.host.checkout.Redaction

import utest.*

/** [[LocalUncommitted]] over [[GitFixture]]'s clone. */
object LocalUncommittedTests extends TestSuite {
  import GitFixture.*

  private val who: Vector[String] =
    Vector("-c", "user.name=grit", "-c", "user.email=grit@example.invalid")

  private def read(repo: Repo^, at: Path): Either[String, Uncommitted.Files] =
    new LocalUncommitted(at, repo.environment, Redaction.of(Map.empty)).read.left.map(_.message)

  private def written(at: Path, name: String): Unit = {
    val _ = Files.createDirectories(at.resolve(name).getParent)
    val _ = Files.writeString(at.resolve(name), name)
  }

  private def paths(names: String*): Vector[RelPath] =
    names.toVector.flatMap(RelPath.of(_).toOption)

  val tests = Tests {
    test(
      "a commit leaves out an unstaged change, a deletion and a renamed file changed again, and " +
        "every untracked file however deep; never a staged change or an ignored file"
    ) {
      inRepository { repo =>
        val at = repo.source
        Seq("a.txt", "b.txt", "c.txt").foreach(written(at, _))
        val _ = Files.writeString(at.resolve(".gitignore"), "ignored.log\n")
        val _ = git(repo, at, "add", "-A")
        val _ = git(repo, at, (who ++ Vector("commit", "--quiet", "-m", "files"))*)
        val _ = git(repo, at, "mv", "a.txt", "renamed.txt")
        val _ = Files.writeString(at.resolve("renamed.txt"), "changed after the rename")
        Files.delete(at.resolve("b.txt"))
        val _ = Files.writeString(at.resolve("c.txt"), "staged")
        val _ = git(repo, at, "add", "c.txt")
        written(at, "ignored.log")
        written(at, "dir/sub/n.txt")
        assert(
          read(repo, at) == Right(
            Uncommitted.Files(paths("b.txt", "renamed.txt"), paths("dir/sub/n.txt"))
          )
        )
      }
    }
    test("a checkout whose every change is staged leaves nothing out") {
      inRepository { repo =>
        written(repo.source, "new.txt")
        val _ = git(repo, repo.source, "add", "new.txt")
        assert(read(repo, repo.source) == Right(Uncommitted.Files(Vector.empty, Vector.empty)))
      }
    }
    test("a directory that is not a repository cannot be read") {
      inRepository { repo =>
        val failed = read(repo, repo.root)
        assert(failed.left.exists(_.contains("git status exited 128")))
      }
    }
    test("an untracked secrets file cannot be read, naming it") {
      inRepository { repo =>
        written(repo.source, ".env")
        val failed = read(repo, repo.source)
        assert(failed.left.exists(_.startsWith("git status names `.env`")))
      }
    }
  }
}
