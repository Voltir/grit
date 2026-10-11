package grit.knowledge.card

/** A unit's version: the git SHA of what it was when read (a file's blob, a directory's tree),
  * 40 lowercase hex digits, or 64 in a repository using SHA-256.
  */
opaque type UnitVersion = String

object UnitVersion {

  /** `text` as a version, or why not: not 40 or 64 lowercase hex digits. */
  def of(text: String): Either[String, UnitVersion] =
    Either.cond(Sha.matches(text), text, s"a git SHA is 40 or 64 lowercase hex digits: $text")

  def value(version: UnitVersion): String = version

  private val Sha = "[0-9a-f]{40}|[0-9a-f]{64}".r
}
