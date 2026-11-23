package grit.core.host

import java.net.URI

/** One run's branch, kept in the clone its working copy was made from. */
trait Branch extends caps.SharedCapability {

  /** The branch pushed to its declared repository, never forced, and its pull request's
    * address, opened with `title` and `body` when none is open. Offered again, it pushes what
    * was committed since and gives the same address. A push the repository refuses (one that
    * is not a fast-forward) opens nothing. `Failed`, nothing pushed, when its pull request was
    * merged or closed: grit does not open another.
    */
  def offer(title: String, body: String): Either[HostError, URI]

  /** How the branch stands in its declared repository. */
  def stands: Either[HostError, Branch.Stands]
}

object Branch {

  /** How a run's branch stands. */
  enum Stands derives CanEqual {

    /** The repository lacks the branch's last commit, and no pull request from it was merged
      * or closed.
      */
    case Unpushed

    /** Pushed as it stands, with no pull request opened. */
    case Pushed

    /** Pushed as it stands, its pull request open at `at`. */
    case Open(at: URI)

    /** Its pull request was merged, at `at`, whatever was committed since. */
    case Merged(at: URI)

    /** Its pull request was closed unmerged, at `at`, whatever was committed since. */
    case Closed(at: URI)
  }
}
