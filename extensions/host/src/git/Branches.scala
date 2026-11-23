package grit.host.git

import java.nio.file.Path

import grit.core.host.{Handing, Processes}
import grit.core.place.WorkName
import grit.core.secret.Secret
import grit.host.checkout.{LocalHanding, LocalProcesses, Redaction}

/** The branches `grit/{work}` of one clone, each pushed with its secret and offered as `target`
  * says, git run in the clone alone.
  */
final class Branches private[git] (
    val target: PushTarget^,
    processes: Processes^,
    handing: Handing^,
    secret: Secret
) extends caps.SharedCapability {

  /** The run `work`'s branch. */
  def of(work: WorkName): LocalBranch^ = new LocalBranch(target, work, processes, handing, secret)
}

object Branches {

  /** The branches of the clone at `clone`, pushed with `secret` and offered as `target` says:
    * git run there under `environment`, what it shows cleared by `redaction`.
    */
  def at(
      clone: Path,
      target: PushTarget^,
      secret: Secret,
      environment: Map[String, String],
      redaction: Redaction
  ): Branches^ =
    new Branches(
      target,
      new LocalProcesses(clone, environment, redaction),
      new LocalHanding(clone, environment, redaction),
      secret
    )
}
