package grit.projects.name

import grit.core.id.{Sha256, Slug}

/** A project's key: the first part of each of its artifacts' keys. */
opaque type ProjectKey = String

object ProjectKey {

  /** 40: the most characters a project's key holds. */
  val Longest: Int = 40

  /** `s` as a project's key, or why not ([[Slug.check]], at most [[Longest]] characters). */
  def of(s: String): Either[String, ProjectKey] =
    Slug.check("a project's key", s, Some(Longest), leadingDigit = true)
  def value(k: ProjectKey): String = k
}

/** An artifact's part of its key, `<artifact>` in `<project>/<artifact>`: the same name in any
  * project.
  */
opaque type LocalKey = String

object LocalKey {

  /** 80: the most characters a local key holds. */
  val Longest: Int = 80

  /** `s` as a local key, or why not ([[Slug.check]], at most [[Longest]] characters). */
  def of(s: String): Either[String, LocalKey] =
    Slug.check("an artifact's part of its key", s, Some(Longest), leadingDigit = true)

  def value(k: LocalKey): String = k
}

/** An artifact's key, `<project>/<artifact>`: its id, never its title, so a rename moves no
  * reference to it.
  */
final case class ArtifactKey private (project: ProjectKey, local: LocalKey) extends caps.Pure {
  def value: String = s"$project/$local"
}

object ArtifactKey {

  /** The key of the artifact `local` of `project`, or why not, as [[LocalKey.of]] refuses. */
  def of(project: ProjectKey, local: String): Either[String, ArtifactKey] =
    LocalKey.of(local).map(new ArtifactKey(project, _))

  /** `s` as [[ArtifactKey.value]] writes a key, or why not: no `/`, or a part
    * [[ProjectKey.of]] or [[of]] refuses.
    */
  def read(s: String): Either[String, ArtifactKey] = s.indexOf('/') match {
    case -1 => Left(s"an artifact's key has no '/': $s")
    case i => ProjectKey.of(s.take(i)).flatMap(of(_, s.drop(i + 1)))
  }
}

/** The method version a project pinned: its name, its version and the digest it had then. */
final case class Pin private (method: MethodName, version: Int, digest: Sha256) extends caps.Pure

object Pin {

  /** These as a pin, or why not: `version` under 1. */
  def of(method: MethodName, version: Int, digest: Sha256): Either[String, Pin] =
    if (version < 1) Left(s"a pinned version is under 1: $version")
    else Right(new Pin(method, version, digest))

  /** These as a pin, the digest as written ([[Sha256.read]]), or why not: `version` under 1, or
    * `digest` not 64 lowercase hex digits.
    */
  def read(method: MethodName, version: Int, digest: String): Either[String, Pin] =
    Sha256
      .read(digest)
      .toRight(s"a pinned digest is not 64 lowercase hex digits: $digest")
      .flatMap(of(method, version, _))
}
