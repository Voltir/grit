package grit.kit.run

import grit.core.host.{Branch, Handing, Processes, RemoteRepository}
import grit.core.place.{Place, WorkName}
import grit.core.secret.Secret
import grit.core.tool.Writing
import grit.host.git.{LocalBranch, Remote}
import grit.tools.Versioning

/** How a repository's runs offer their branches: each pushed to `to` with `secret`, git run in
  * the clone by `processes` and handed the secret by `handing` (both at the clone), and offered
  * on `remoteRepository`, whose place is `described`'s one destination.
  */
final class Pushing private (
    to: Remote,
    processes: Processes^,
    handing: Handing^,
    secret: Secret,
    remoteRepository: RemoteRepository^,
    val described: Writing[Versioning.OfferArgs, Place]
) extends caps.SharedCapability {

  /** The run `work`'s branch. */
  def branch(work: WorkName): Branch^ =
    new LocalBranch(to, work, processes, handing, secret, remoteRepository)
}

object Pushing {

  /** Branches pushed to `to` and offered on `remoteRepository`, the remote repository named `named` at
    * `place` ([[Versioning.hosted]]); why not, as that refuses.
    */
  def of(
      to: Remote,
      processes: Processes^,
      handing: Handing^,
      secret: Secret,
      remoteRepository: RemoteRepository^,
      place: Place,
      named: String
  ): Either[String, Pushing^] =
    Versioning.hosted(place, named) match {
      case Right(described) =>
        Right(new Pushing(to, processes, handing, secret, remoteRepository, described))
      case Left(why) => Left(why)
    }
}
