package grit.core.host

/** The lines of a file nearest a passage it does not hold, for a model to copy an edit's
  * `oldText` from: `lines`, the first of them line `first`, counting from 1.
  */
final case class Nearest private (first: Int, lines: Vector[String])

object Nearest {

  /** The most lines a [[Nearest]] holds. */
  val Most = 12

  /** The characters a line of a [[Nearest]] keeps; a longer one is cut there and ends in `…`. */
  val Widest = 200

  /** The lines of `text` around the run of as many lines as `passage` has that holds the most
    * of `passage`'s lines, the earliest of equals, with one line either side. Lines compare with
    * the spaces at their ends ignored, and only lines with a letter or digit count when
    * `passage` has any, so that a `}` alone does not draw the answer. Line breaks are `\n`.
    * `None` when no line of `passage` is in `text`.
    */
  def in(text: String, passage: String): Option[Nearest] = {
    val lines = text.split("\n", -1).toVector match {
      case init :+ "" => init
      case all => all
    }
    val olds = passage.split("\n").toVector.map(_.strip).filter(_.nonEmpty)
    val keyed = olds.filter(_.exists(_.isLetterOrDigit))
    val wanted = (if (keyed.nonEmpty) keyed else olds).toSet
    val hits = lines.map(l => wanted(l.strip))
    if (!hits.contains(true)) None
    else {
      val span = passage.split("\n", -1).length.max(1)
      // `upTo(i)`: the hits among the first `i` lines.
      val upTo = hits.scanLeft(0)((n, hit) => if (hit) n + 1 else n)
      def held(start: Int) = upTo((start + span).min(lines.size)) - upTo(start)
      val start = lines.indices.maxBy(s => (held(s), -s))
      val within = (start until (start + span).min(lines.size)).filter(hits)
      val from = (within.headOption.getOrElse(start) - 1).max(0)
      val to = (within.lastOption.getOrElse(start) + 1).min(lines.size - 1)
      val shown = lines.slice(from, (to + 1).min(from + Most))
      Some(Nearest(from + 1, shown.map(l => if (l.length > Widest) l.take(Widest) + "…" else l)))
    }
  }
}
