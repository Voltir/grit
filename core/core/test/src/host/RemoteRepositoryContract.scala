package grit.core.host

import utest.*

/** The contract every [[RemoteRepository]] keeps, run against `FakeRemoteRepository` in
  * grit.host and `GitHubRepository` over a fake of GitHub's REST API in grit.mcp.
  */
abstract class RemoteRepositoryContract extends TestSuite {

  /** A repository with no pull request, and how a person ends one there. */
  protected trait Held {

    /** The repository under test. */
    val repository: RemoteRepository^

    /** `branch`'s open pull request closed unmerged, its head at `head`. */
    def close(branch: String, head: String): Unit

    /** `branch`'s open pull request merged, its head at `head`. */
    def merge(branch: String, head: String): Unit

    /** How many pull requests were opened there. */
    def opened: Int
  }

  /** `body` over a repository with no pull request. */
  protected def fresh[A](body: Held^ => A): A

  private val head = "0123456789abcdef0123456789abcdef01234567"

  val tests = Tests {
    test("a branch none was opened from has no pull request") {
      fresh(held => held.repository.pullRequest("grit/w1") ==> Right(None))
    }
    test("opened, a branch's pull request is open at the address open answered") {
      fresh { held =>
        val at = held.repository.open("grit/w1", "Fix it", "Fixed.")
        (
          at.map(a => Some(RemoteRepository.PullRequest.Open(a))),
          held.repository.pullRequest("grit/w1")
        ) match {
          case (Right(expected), got) => got ==> Right(expected)
          case (Left(why), _) => throw new java.lang.AssertionError(why.message)
        }
      }
    }
    test("opened again while open, it answers the same address and opens no second") {
      fresh { held =>
        val first = held.repository.open("grit/w1", "Fix it", "Fixed.")
        val second = held.repository.open("grit/w1", "Fix it again", "Again.")
        (first.isRight, first == second, held.opened) ==> (true, true, 1)
      }
    }
    test("closed, it stands Closed at its address with its head; merged, Merged") {
      fresh { held =>
        val closed = held.repository.open("grit/w1", "Fix it", "Fixed.")
        val merged = held.repository.open("grit/w2", "Fix that", "Fixed.")
        held.close("grit/w1", head)
        held.merge("grit/w2", head)
        (held.repository.pullRequest("grit/w1"), held.repository.pullRequest("grit/w2")) ==> (
          closed.map(at => Some(RemoteRepository.PullRequest.Closed(at, head))),
          merged.map(at => Some(RemoteRepository.PullRequest.Merged(at, head)))
        )
      }
    }
    test("another branch's pull request is not this branch's") {
      fresh { held =>
        val _ = held.repository.open("grit/w2", "Fix that", "Fixed.")
        held.repository.pullRequest("grit/w1") ==> Right(None)
      }
    }
  }
}
