package grit.host.git

import grit.core.host.RemoteRepository

/** Where a clone's runs are pushed and offered: the remote repository `repository`, pushed to
  * at its own address, `remote`.
  */
final class PushTarget private (val remote: Remote, val repository: RemoteRepository^)
    extends caps.SharedCapability

object PushTarget {

  /** `repository`, pushed to at [[RemoteRepository.pushedTo]]; why not, as [[Remote.of]]
    * refuses that address.
    */
  def of(repository: RemoteRepository^): Either[String, PushTarget^] =
    Remote.of(repository.pushedTo) match {
      case Right(remote) => Right(new PushTarget(remote, repository))
      case Left(why) => Left(why)
    }
}
