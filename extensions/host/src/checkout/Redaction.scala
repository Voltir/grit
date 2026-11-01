package grit.host.checkout

import java.util.regex.{Matcher, Pattern}

import grit.core.secret.{Named, Variable}

/** The secrets a deployment holds, as text a program or file shows a tool is cleared of them.
  * Shows how many variables it redacts, never a value.
  */
final class Redaction private (
    pieces: Map[String, Variable],
    val variables: Vector[Variable],
    val unguarded: Vector[Variable]
) extends caps.Pure {

  /** Longest first, so of two pieces where one holds the other the longer is found. */
  private val found: Option[Pattern] =
    Option.when(pieces.nonEmpty)(
      Pattern.compile(pieces.keys.toVector.sortBy(-_.length).map(Pattern.quote).mkString("|"))
    )

  /** `text` with each value held replaced by `[redacted secret: VARIABLE]`: a one-line value
    * whole, longest first; a multi-line value line by line, so the text keeps its line count; a
    * value's suffix `text` starts with and its prefix `text` ends with, as a cut leaves them.
    * Only exact text of at least [[Redaction.Shortest]] characters is found: an encoded, split
    * or shorter part is left as it is.
    */
  def apply(text: String): String =
    found.fold(text) { pattern =>
      val start =
        edge(
          piece => Redaction.overlaps(text.take(piece.length - 1), piece),
          (_, n) => text.take(n)
        )
      val end = edge(
        piece => Redaction.overlaps(piece.take(piece.length - 1), text.takeRight(piece.length - 1)),
        _.take(_)
      )
        .filter((n, _) => n + start.fold(0)(_._1) <= text.length)
      val middle = text.substring(start.fold(0)(_._1), text.length - end.fold(0)(_._1))
      val replaced = pattern
        .matcher(middle)
        .replaceAll(m =>
          Matcher.quoteReplacement(pieces.get(m.group()).fold(m.group())(Redaction.mark))
        )
      start.fold("")((_, v) => Redaction.mark(v)) + replaced + end.fold("")((_, v) =>
        Redaction.mark(v)
      )
    }

  /** The length of the longest `part` of a piece, of the lengths `lengths` gives for it (shorter
    * than it, longest first) at least [[Redaction.Shortest]] long, with its variable. A part that
    * is itself a whole piece is left to the whole pieces' search, which names its own variable.
    */
  private def edge(
      lengths: String => Vector[Int],
      part: (String, Int) => String
  ): Option[(Int, Variable)] =
    pieces.toVector
      .flatMap((piece, v) =>
        lengths(piece).iterator
          .takeWhile(_ >= Redaction.Shortest)
          .find(n => !(pieceLengths.contains(n) && pieces.contains(part(piece, n))))
          .map(n => (n, v))
      )
      .maxByOption(_._1)

  private val pieceLengths: Set[Int] = pieces.keySet.map(_.length)

  /** One line, for the log at start-up: the variables redacted, and those too short to be. */
  def described: String = {
    def names(vs: Vector[Variable]): String = vs.map(Variable.value).mkString(", ")
    val redacting =
      if (variables.isEmpty) "Redacting no values from tool output"
      else s"Redacting the values of ${names(variables)} from tool output"
    if (unguarded.isEmpty) s"$redacting."
    else s"$redacting; too short to redact: ${names(unguarded)}."
  }

  override def toString: String = s"Redaction(${variables.size} variables)"
}

object Redaction {

  /** Every value of `env` whose name [[Named.variable]] holds of, trimmed; each line of a
    * multi-line one is its own piece. A value with no line of at least [[Shortest]] characters
    * is [[Redaction.unguarded]].
    */
  def of(env: Map[String, String]): Redaction = {
    val held = env.toVector
      .filter((name, value) => Named.variable(name) && value.trim.nonEmpty)
      .sortBy(_._1)
      .map((name, value) =>
        (
          Variable(name),
          value.trim.split("\n").toVector.map(_.stripSuffix("\r")).filter(_.length >= Shortest)
        )
      )
    val pieces = held.flatMap((v, ps) => ps.map(_ -> v)).foldLeft(Map.empty[String, Variable]) {
      case (m, (p, v)) => if (m.contains(p)) m else m.updated(p, v)
    }
    new Redaction(
      pieces,
      held.collect { case (v, ps) if ps.nonEmpty => v },
      held.collect { case (v, ps) if ps.isEmpty => v }
    )
  }

  /** 8: the fewest characters redacted; a shorter value, such as `grit`, would clear common
    * words.
    */
  val Shortest: Int = 8

  /** Whether `text` holds a mark [[Redaction.apply]] leaves in a value's place. */
  def marked(text: String): Boolean = text.contains(Mark)

  private val Mark = "[redacted secret: "

  /** The lengths of the prefixes of `a` that are suffixes of `b`, longest first, none empty: a
    * Knuth–Morris–Pratt match of `a` over `b`, in time linear in both.
    */
  private def overlaps(a: String, b: String): Vector[Int] =
    if (a.isEmpty) Vector.empty
    else {
      // border(i): the longest proper prefix of `a.take(i + 1)` that is also its suffix.
      val border = new Array[Int](a.length)
      var k = 0
      var i = 1
      while (i < a.length) {
        while (k > 0 && a.charAt(i) != a.charAt(k)) k = border(k - 1)
        if (a.charAt(i) == a.charAt(k)) k += 1
        border(i) = k
        i += 1
      }
      var q = 0
      b.foreach { c =>
        if (q == a.length) q = border(q - 1)
        while (q > 0 && c != a.charAt(q)) q = border(q - 1)
        if (c == a.charAt(q)) q += 1
      }
      Vector.unfold(q)(n => Option.when(n > 0)((n, border(n - 1))))
    }

  private def mark(v: Variable): String = s"$Mark${Variable.value(v)}]"
}
