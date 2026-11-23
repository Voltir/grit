package grit.core.host

import java.net.URI

/** A forge's pull requests for one repository. */
trait Forge extends caps.SharedCapability {

  /** A pull request from `branch` into the repository's default branch, opened with `title` and
    * `body`: its address.
    */
  def open(branch: String, title: String, body: String): Either[HostError, URI]

  /** The open pull request from `branch`; `None` when none. */
  def opened(branch: String): Either[HostError, Option[URI]]
}
