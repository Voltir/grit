package grit.outline.locate

import java.util.regex.Pattern

import scala.util.matching.Regex

/** A glob over one word of source text: each `*` any run of identifier characters (`\w` and `$`), the rest literal and
  * case-sensitive, matched only as a whole word.
  */
final class WordGlob private (regex: Regex, piece: String) {

  /** Whether `text` holds a whole word this glob matches. */
  def foundIn(text: String): Boolean =
    // Every match holds each literal piece, so a text lacking the longest skips the backtracking regex.
    text.contains(piece) && regex.findFirstIn(text).isDefined

  /** Whether `word` is, as a whole, a word this glob matches. */
  def matches(word: String): Boolean = regex.matches(word)
}

object WordGlob {

  /** The glob `segment` spells, as in `*Shelf*` or `Turn*Env`. */
  def apply(segment: String): WordGlob = {
    val pieces = segment.split("\\*", -1).toVector
    new WordGlob(
      ("(?<![\\w$])" + pieces.map(Pattern.quote).mkString("[\\w$]*") + "(?![\\w$])").r,
      pieces.maxByOption(_.length).getOrElse("")
    )
  }
}
