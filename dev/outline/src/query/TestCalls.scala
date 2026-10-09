package grit.outline.query

import scala.annotation.tailrec
import scala.util.matching.Regex

import grit.outline.model.{Lines, TestCase}

object TestCalls {

  /** The character offsets from the start of line `lines.start` to the end of line `lines.end` in `src`, the end exclusive. */
  def within(src: String, lines: Lines): (Int, Int) = {
    val all = src.split("\n", -1).toVector
    val start = all.take(lines.start - 1).map(_.length + 1).sum
    val end = start + all.slice(lines.start - 1, lines.end).map(_.length + 1).sum - 1
    (start, end)
  }

  /** The calls `call("<name>")` in `src` that start at a character offset in `from` until `to`, in source order. Each spans from its own line to the line closing the first `{` on that line, or only its own line when no `{` follows it there; braces inside string literals, character literals and `//` comments do not count. `text` is the verbatim source of those whole lines. */
  def find(src: String, from: Int, to: Int, call: String): Vector[TestCase] = {
    val pattern: Regex = (raw"\b" + Regex.quote(call) + raw"""\("((?:[^"\\]|\\.)*)"\)""").r
    val lines = src.split("\n", -1).toVector
    pattern
      .findAllMatchIn(src)
      .filter(m => m.start >= from && m.start < to)
      .map { m =>
        val first = lineOf(src, m.start)
        val last = closingLine(src, m.end, first)
        TestCase(m.group(1), Lines(first, last), lines.slice(first - 1, last).mkString("\n"))
      }
      .toVector
  }

  /** The 1-based line holding character offset `index`. */
  private def lineOf(src: String, index: Int): Int = src.take(index).count(_ == '\n') + 1

  /** The line holding the `}` that closes the first `{` after `after` on the line `first` is on; `first` itself when that line has no `{` past `after`, or when the source ends before the body does. */
  private def closingLine(src: String, after: Int, first: Int): Int = {
    val newline = src.indexOf('\n', after)
    val lineEnd = if (newline < 0) src.length else newline
    val open = src.indexOf('{', after)
    if (open < 0 || open >= lineEnd) first
    else
      closeAt(src, open + 1, 1) match {
        case Some(close) => lineOf(src, close)
        case None => lineOf(src, math.max(src.length - 1, 0))
      }
  }

  /** The offset of the `}` that brings `depth` back to zero from `i`, or None when the source ends first. */
  @tailrec private def closeAt(src: String, i: Int, depth: Int): Option[Int] =
    if (i >= src.length) None
    else if (src.startsWith("\"\"\"", i)) closeAt(src, skipTriple(src, i + 3), depth)
    else
      src.charAt(i) match {
        case '"' => closeAt(src, skipString(src, i + 1), depth)
        case '\'' if i + 3 < src.length && src.charAt(i + 1) == '\\' && src.charAt(i + 3) == '\'' =>
          closeAt(src, i + 4, depth)
        case '\'' if i + 2 < src.length && src.charAt(i + 2) == '\'' => closeAt(src, i + 3, depth)
        case '/' if i + 1 < src.length && src.charAt(i + 1) == '/' =>
          src.indexOf('\n', i) match {
            case -1 => None
            case newline => closeAt(src, newline, depth)
          }
        case '{' => closeAt(src, i + 1, depth + 1)
        case '}' if depth == 1 => Some(i)
        case '}' => closeAt(src, i + 1, depth - 1)
        case _ => closeAt(src, i + 1, depth)
      }

  /** The offset just past the closing quote of a string literal whose body starts at `i`. */
  @tailrec private def skipString(src: String, i: Int): Int =
    if (i >= src.length) i
    else
      src.charAt(i) match {
        case '\\' => skipString(src, i + 2)
        case '"' => i + 1
        case _ => skipString(src, i + 1)
      }

  /** The offset just past the closing `"""` of a triple-quoted literal whose body starts at `i`. */
  private def skipTriple(src: String, i: Int): Int =
    src.indexOf("\"\"\"", i) match {
      case -1 => src.length
      case close => close + 3
    }
}
