package grit.kit.deployment

/** A deployment's own version: letters, digits, `.` and `-`, at most 32. Today nothing a
  * deployment changes needs a new one: it adds no workflow or step, and its jobs and plugins
  * carry their own versions. Change it only for a change of its own that a workflow in flight
  * could not survive; every workflow in flight under the old one is then never resumed
  * (ADR 0004).
  */
opaque type DeploymentVersion = String

object DeploymentVersion {

  /** The longest a version may be: 32 characters. */
  val Longest: Int = 32

  /** `raw` as a version; `VersionMalformed` when empty, over [[Longest]], or holding any other
    * character, `+` included: `+` joins it to grit's epoch in [[Deployment.runsUnder]].
    */
  def of(raw: String): Either[DeploymentRefusal, DeploymentVersion] =
    Either.cond(
      raw.nonEmpty && raw.length <= Longest &&
        raw.forall(c => (c.isLetterOrDigit && c < 128) || c == '.' || c == '-'),
      raw,
      DeploymentRefusal.VersionMalformed(raw)
    )

  /** The version a deployment has until it changes one: `1`. */
  val First: DeploymentVersion = "1"

  def value(version: DeploymentVersion): String = version
}
