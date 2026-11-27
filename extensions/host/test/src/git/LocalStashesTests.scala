package grit.host.git

import java.nio.file.{Files, Path}

import grit.core.host.Stashes
import grit.host.checkout.Redaction

import utest.*

/** [[LocalStashes]] over [[GitFixture]]'s clone and a worktree added to it. */
object LocalStashesTests extends TestSuite {
  import GitFixture.*

  private val who: Vector[String] =
    Vector("-c", "user.name=grit", "-c", "user.email=grit@example.invalid")

  /** A new untracked file stashed in `in`, with `message` or as a WIP entry. */
  private def stashed(repo: Repo^, in: Path, name: String, message: Option[String]): Unit = {
    val _ = Files.writeString(in.resolve(name), name)
    val _ = git(
      repo,
      in,
      (who ++ Vector("stash", "push", "-u") ++ message.toVector.flatMap(m => Vector("-m", m)))*
    )
  }

  private def held(repo: Repo^, at: Path): Either[String, Stashes.Held] =
    new LocalStashes(at, repo.environment, Redaction.of(Map.empty)).held.left.map(_.message)

  val tests = Tests {
    test("a clone's own stash is not shared; each entry names the branch it was made on") {
      inRepository { repo =>
        stashed(repo, repo.source, "a", None)
        stashed(repo, repo.source, "b", Some("second"))
        assert(
          held(repo, repo.source) == Right(
            Stashes.Held(false, Some("main"), Vector(Some("main"), Some("main")))
          )
        )
      }
    }

    test("a linked worktree shares its clone's stash, and names its own branch") {
      inRepository { repo =>
        stashed(repo, repo.source, "a", None)
        val side = repo.root.resolve("side")
        val _ =
          git(repo, repo.source, "worktree", "add", "--quiet", "-b", "side", side.toString, "main")
        stashed(repo, side, "b", Some("mine"))
        assert(
          held(repo, side) == Right(
            Stashes.Held(true, Some("side"), Vector(Some("side"), Some("main")))
          )
        )
      }
    }

    test("branchOf reads both labels git writes, and nothing else") {
      assert(
        Vector(
          "stash@{0}: WIP on main: 1a2b3c4 first",
          "stash@{1}: On grit/work-3: a message: with a colon",
          "stash@{2}: autostash"
        ).map(Stashes.branchOf) == Vector(Some("main"), Some("grit/work-3"), None)
      )
    }
  }
}
