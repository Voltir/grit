package grit.core.host

import java.net.URI

/** A repository a run's branch is pushed to, outside this machine, and its pull requests. */
trait RemoteRepository extends caps.SharedCapability {

  /** The address git pushes to it at, as written. */
  def pushedTo: String

  /** A pull request from `branch` into the repository's default branch, opened with `title` and
    * `body`: its address; when one from `branch` is open already, that one's.
    */
  def open(branch: String, title: String, body: String): Either[HostError, URI]

  /** The pull request from this repository's `branch`: one open when there is one, else the
    * newest of those merged or closed; `None` when none was opened.
    */
  def pullRequest(branch: String): Either[HostError, Option[RemoteRepository.PullRequest]]
}

object RemoteRepository {

  /** A pull request, as its repository says it stands. */
  enum PullRequest derives CanEqual {

    /** Open at `at`. */
    case Open(at: URI)

    /** Merged, at `at`, its head the commit `head`. */
    case Merged(at: URI, head: String)

    /** Closed unmerged, at `at`, its head the commit `head`. */
    case Closed(at: URI, head: String)
  }
}
