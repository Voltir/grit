package grit.projects.artifact

import grit.core.id.Sha256
import grit.projects.name.MethodName

/** The method version a project pinned, and the digest it had then. */
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
