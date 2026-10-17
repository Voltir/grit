package grit.knowledge.card

import java.time.Instant
import java.time.format.DateTimeParseException

/** A unit's version: the git SHA of what it was when read (a file's blob, a directory's tree), or
  * when a file last changed, as a source with no SHAs gives it.
  */
sealed trait UnitVersion extends caps.Pure

object UnitVersion {

  /** A git SHA: 40 lowercase hex digits, or 64 in a repository using SHA-256. */
  final case class Sha private[UnitVersion] (hex: String) extends UnitVersion

  /** When the unit last changed. */
  final case class Changed(at: Instant) extends UnitVersion

  /** `text` as a SHA, or why not: not 40 or 64 lowercase hex digits. */
  def sha(text: String): Either[String, UnitVersion] =
    Either.cond(Hex.matches(text), Sha(text), s"a git SHA is 40 or 64 lowercase hex digits: $text")

  /** `text`, a version's written form ([[value]]), as that version, or why not: neither 40 or 64
    * lowercase hex digits nor an ISO-8601 instant.
    */
  def of(text: String): Either[String, UnitVersion] =
    sha(text).left.flatMap { _ =>
      try Right(Changed(Instant.parse(text)))
      catch {
        case _: DateTimeParseException =>
          Left(s"a version is 40 or 64 lowercase hex digits or an ISO-8601 instant: $text")
      }
    }

  /** Its written form: a SHA's hex digits, or an instant in ISO-8601 (`2026-10-14T09:30:00Z`). */
  def value(version: UnitVersion): String = version match {
    case Sha(hex) => hex
    case Changed(at) => at.toString
  }

  private val Hex = "[0-9a-f]{40}|[0-9a-f]{64}".r
}
