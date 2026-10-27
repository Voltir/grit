package grit.projects.name

/** A project's key: the first part of each of its artifacts' keys. */
opaque type ProjectKey = String

object ProjectKey {

  /** 40: the most characters a project's key holds. */
  val Longest: Int = 40

  /** `s` as a project's key, or why not ([[Slug.check]], at most [[Longest]] characters). */
  def of(s: String): Either[String, ProjectKey] = Slug.check("a project's key", s, Longest)
  def value(k: ProjectKey): String = k
}

/** An artifact's key, `<project>/<artifact>`: its id, never its title, so a rename moves no
  * reference to it.
  */
final case class ArtifactKey private (project: ProjectKey, local: String) extends caps.Pure {
  def value: String = s"$project/$local"
}

object ArtifactKey {

  /** 80: the most characters an artifact's part of its key holds. */
  val Longest: Int = 80

  /** The key of the artifact `local` of `project`, or why not ([[Slug.check]], at most
    * [[Longest]] characters).
    */
  def of(project: ProjectKey, local: String): Either[String, ArtifactKey] =
    Slug.check("an artifact's part of its key", local, Longest).map(new ArtifactKey(project, _))

  /** `s` as [[ArtifactKey.value]] writes a key, or why not: no `/`, or a part
    * [[ProjectKey.of]] or [[of]] refuses.
    */
  def read(s: String): Either[String, ArtifactKey] = s.indexOf('/') match {
    case -1 => Left(s"an artifact's key has no '/': $s")
    case i => ProjectKey.of(s.take(i)).flatMap(of(_, s.drop(i + 1)))
  }
}

/** The method version a project pinned: its name, its version and the digest it had then. */
final case class Pin private (method: MethodName, version: Int, digest: String) extends caps.Pure

object Pin {

  /** These as a pin, or why not: `version` under 1, or `digest` not 64 lowercase hex digits. */
  def of(method: MethodName, version: Int, digest: String): Either[String, Pin] =
    if (version < 1) Left(s"a pinned version is under 1: $version")
    else if (
      digest.length != 64 || !digest.forall(c => c >= '0' && c <= '9' || c >= 'a' && c <= 'f')
    )
      Left(s"a pinned digest is not 64 lowercase hex digits: $digest")
    else Right(new Pin(method, version, digest))
}
