package grit.kit.run

import java.nio.file.Path

import grit.core.host.{Branch, Handing, Processes}
import grit.core.place.{Place, WorkName}
import grit.core.secret.Secret
import grit.core.tool.Writing
import grit.host.checkout.{LocalHanding, LocalProcesses, Redaction}
import grit.host.git.{LocalBranch, PushTarget}
import grit.tools.Versioning

/** How a repository's runs offer their branches: each pushed and offered as `target` says, with
  * `secret`, git run in the clone by `processes` and handed the secret by `handing`; `described`
  * is the tool whose one destination is the remote repository's place.
  */
final class Pushing private (
    target: PushTarget^,
    processes: Processes^,
    handing: Handing^,
    secret: Secret,
    val described: Writing[Versioning.OfferArgs, Place]
) extends caps.SharedCapability {

  /** The run `work`'s branch. */
  def branch(work: WorkName): Branch^ = new LocalBranch(target, work, processes, handing, secret)
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
          new Pushing(
            target,
            new LocalProcesses(clone, environment, redaction),
            new LocalHanding(clone, environment, redaction),
            secret,
            described
          )
        )
      case Left(why) => Left(why)
    }
}
