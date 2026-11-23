package grit.kit.run

import java.nio.file.Path

import grit.core.host.Branch
import grit.core.place.{Place, WorkName}
import grit.core.secret.Secret
import grit.core.tool.Writing
import grit.host.checkout.Redaction
import grit.host.git.{Branches, PushTarget}
import grit.tools.Versioning

/** How a repository's runs offer their branches, `branches`; `described` is the tool whose one
  * destination is the remote repository's place.
  */
final class Pushing private (
    val branches: Branches^,
    val described: Writing[Versioning.OfferArgs, Place]
) extends caps.SharedCapability {

  /** The run `work`'s branch. */
  def branch(work: WorkName): Branch^ = branches.of(work)
}

object Pushing {

  /** Branches of the clone at `clone` pushed and offered as `target` says, the remote
    * repository named `named` at `place` ([[Versioning.hosted]]), each pushed with `secret`:
    * git run in `clone` alone, under `environment` (the process's own, never one `secret` was
    * read from with others), what it shows cleared by `redaction`. Why not, as
    * [[Versioning.hosted]] refuses.
    */
  def of(
      clone: Path,
      target: PushTarget^,
      secret: Secret,
      environment: Map[String, String],
      redaction: Redaction,
      place: Place,
      named: String
  ): Either[String, Pushing^] =
    Versioning.hosted(place, named) match {
      case Right(described) =>
        Right(
          new Pushing(Branches.at(clone, target, secret, environment, redaction), described)
        )
      case Left(why) => Left(why)
    }
}
