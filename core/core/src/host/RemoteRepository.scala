package grit.core.host

import java.net.URI

/** A repository a run's branch is pushed to, outside this machine, and its pull requests. */
trait RemoteRepository extends caps.SharedCapability {

  /** A pull request from `branch` into the repository's default branch, opened with `title` and
    * `body`: its address.
    */
  def open(branch: String, title: String, body: String): Either[HostError, URI]

  /** The open pull request from `branch`; `None` when none. */
  def opened(branch: String): Either[HostError, Option[URI]]
}
