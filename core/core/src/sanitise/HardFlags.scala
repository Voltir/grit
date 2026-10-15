package grit.core.sanitise

import scala.util.matching.Regex

import grit.core.context.SectionTag

/** What code finds in a text before the classifier is asked: each of [[Reason.Hidden]],
  * [[Reason.Encoded]], [[Reason.Marker]] and [[Reason.OverBound]] that fired, in that order.
  */
final case class HardFlags private (fired: Vector[Reason]) extends caps.Pure

object HardFlags {

  /** What fires in `text`. */
  def of(text: String): HardFlags =
    new HardFlags(
      Vector(
        Option.when(hidden(text))(Reason.Hidden),
        Option.when(encoded(text))(Reason.Encoded),
        Option.when(SectionTag.values.exists(t => startsALine(text, t.tag)))(Reason.Marker),
        Option.when(text.length > IntegrityTree.Bound)(Reason.OverBound)
      ).flatten
    )

  /** Zero-width and invisible formatting characters, bidirectional controls, the byte order
    * mark, the soft hyphen, and Unicode's tag characters.
    */
  private val Invisible: Regex =
    "[\\x{200B}-\\x{200F}\\x{2060}-\\x{2064}\\x{FEFF}\\x{202A}-\\x{202E}\\x{2066}-\\x{2069}\\x{00AD}\\x{E0000}-\\x{E007F}]".r

  private val Base64: Regex = "(?<![A-Za-z0-9+/])[A-Za-z0-9+/]{32,}={0,2}(?![A-Za-z0-9+/=])".r

  private val Hex: Regex = "(?<![0-9A-Fa-f])[0-9A-Fa-f]{65,}(?![0-9A-Fa-f])".r

  private val Word: Regex = "\\p{L}{3,}".r

  /** Whether a line of `text` begins with `tag`, as a line grit writes does: one inside a line
    * imitates none.
    */
  private def startsALine(text: String, tag: String): Boolean =
    text.startsWith(tag) || text.contains("\n" + tag)

  private def hidden(text: String): Boolean =
    Invisible.findFirstIn(text).nonEmpty || Word.findAllIn(text).exists(mixed)

  /** Whether `word` mixes Latin letters with Cyrillic or Greek ones: a homoglyph's mark. */
  private def mixed(word: String): Boolean = {
    val scripts = word.codePoints.toArray.toSet.map(Character.UnicodeScript.of)
    scripts.contains(Character.UnicodeScript.LATIN) &&
    (scripts.contains(Character.UnicodeScript.CYRILLIC) ||
      scripts.contains(Character.UnicodeScript.GREEK))
  }

  private def encoded(text: String): Boolean =
    Base64
      .findAllIn(text)
      .exists(s => s.exists(_.isLower) && s.exists(_.isUpper) && s.exists(_.isDigit)) ||
      Hex.findFirstIn(text).nonEmpty
}
