package grit.core.host

import java.nio.charset.StandardCharsets.UTF_8

/** Text cut to what the model is shown: at most [[Clipped.MaxLines]] lines and
  * [[Clipped.MaxBytes]] bytes of UTF-8. `hint`, when there is one, tells the model what was
  * left out and how to see it ("Use offset=2001 to continue.").
  */
final case class Clipped private (text: String, hint: Option[String]) {

  /** What the model reads: `text`, then the hint in brackets after a blank line. */
  def show: String = hint.fold(text)(h => if (text.isEmpty) s"[$h]" else s"$text\n\n[$h]")
}

object Clipped {

  /** The most lines a clipped text keeps. */
  val MaxLines = 2000

  /** The most bytes of UTF-8 a clipped text keeps, newlines counted. */
  val MaxBytes: Int = 50 * 1024

  /** 40 lines within 4 KB: the most of a long text's end shown, by [[ends]] and by a run's
    * answer.
    */
  val TailLines = 40
  val TailBytes: Int = 4 * 1024

  /** `text` as its first lines and its last when it does not [[fits]] whole: a head within
    * [[MaxLines]] less [[TailLines]] lines and [[MaxBytes]] less [[TailBytes]] bytes, then a
    * tail within [[TailLines]] lines and [[TailBytes]] bytes, and what lies between; a first or
    * last line alone over its end's share is cut at a character. `None` when it fits.
    */
  def ends(text: String): Option[Ends] =
    Option.when(!fits(text)) {
      val all = lines(text)
      val n = all.size
      // Where each line starts in `text`, as `String` indexes it.
      val starts = all.scanLeft(0)((at, l) => at + l.length + 1)
      val headed = fitting(all, MaxBytes - TailBytes, MaxLines - TailLines)
      // A text that does not fit has more past a whole-line head than the tail holds, so the
      // line after the head is never in it; past a cut first line, all the rest may be.
      val rest = all.drop(headed.max(1))
      val tailed =
        fitting(rest.reverse, TailBytes, TailLines).min(rest.size - (if (headed > 0) 1 else 0))
      val (head, from) =
        if (headed > 0) (all.take(headed).mkString("\n"), starts(headed))
        else {
          val start = prefix(all.headOption.getOrElse(""), MaxBytes - TailBytes)
          (start, start.length)
        }
      val (tail, to) =
        if (tailed > 0) (all.takeRight(tailed).mkString("\n"), starts(n - tailed) - 1)
        else {
          val last = all.lastOption.getOrElse("")
          val end = endBytes(last, TailBytes)
          (end, starts(n - 1) + last.length - end.length)
        }
      val whole = Option.when(headed > 0 && tailed > 0)(Ends.Lines(headed + 1, n - tailed, n))
      Ends(head, Ends.Span(from, bytes(text.substring(from, to)), text.length, whole), tail)
    }

  /** `text` whole when it fits in [[MaxLines]] lines and `bytes` bytes ([[MaxBytes]] by
    * default), else its first whole lines that do: none when its first line alone is over
    * `bytes`. `hint` is told what was kept, `None` when nothing was cut, and gives the hint.
    */
  def head(text: String, hint: Option[Kept] -> Option[String], bytes: Int = MaxBytes): Clipped =
    clip(text, hint, fromEnd = false, bytes)

  /** Whether `text` is whole within the limits: [[head]] keeps it as it is only then. */
  def fits(text: String): Boolean = head(text, _ => None).text == text

  /** The greatest `k` from 0 to `n` whose `text(k)` [[fits]], where `text(k)` grows with `k`;
    * 0 when none does.
    */
  def most(n: Int)(text: Int -> String): Int = {
    var lo = 0
    var hi = n
    while (lo < hi) {
      val mid = (lo + hi + 1) / 2
      if (fits(text(mid))) lo = mid else hi = mid - 1
    }
    lo
  }

  /** What an answer cut by [[head]] says of what it kept: how many of its lines, or that its
    * first line alone was too long; `None` when nothing was cut.
    */
  def cutHint(kept: Option[Kept]): Option[String] =
    kept.map { k =>
      if (k.lines == 0)
        s"The answer's first line alone is over $MaxBytes bytes, so none of it is shown."
      else s"The answer was cut to its first ${k.lines} of ${k.total} lines."
    }

  /** `text` whole when it fits in `lines` lines ([[MaxLines]] by default) and `bytes` bytes
    * ([[MaxBytes]] by default), else its last lines that do; when its last line alone is over
    * `bytes`, the end of that line, cut at a character. `hint` as for [[head]].
    */
  def tail(
      text: String,
      hint: Option[Kept] -> Option[String],
      bytes: Int = MaxBytes,
      lines: Int = MaxLines
  ): Clipped =
    clip(text, hint, fromEnd = true, bytes, lines)

  /** `text`'s lines: split at `\n`, a final `\n` ending the last line rather than starting
    * another.
    */
  def lines(text: String): Vector[String] =
    if (text.isEmpty) Vector.empty
    else {
      val all = text.split("\n", -1).toVector
      if (text.endsWith("\n")) all.dropRight(1) else all
    }

  /** `text` less its ANSI escape sequences and its control characters other than newline and
    * tab: a carriage return is removed, never read as a newline.
    */
  def plain(text: String): String =
    text.replaceAll(Escape, "").replaceAll("[\\p{Cc}&&[^\n\t]]", "")

  private val Escape =
    "\u001b(?:\\[[0-?]*[ -/]*[@-~]|\\][^\u0007\u001b]*(?:\u0007|\u001b\\\\)|[@-Z\\\\-_])"

  private def bytes(s: String): Int = s.getBytes(UTF_8).length

  private def clip(
      text: String,
      hint: Option[Kept] -> Option[String],
      fromEnd: Boolean,
      max: Int,
      maxLines: Int = MaxLines
  ): Clipped = {
    val all = lines(text)
    if (all.size <= maxLines && bytes(text) <= max) Clipped(text, hint(None))
    else {
      val ordered = if (fromEnd) all.reverse else all
      val n = fitting(ordered, max, maxLines)
      val kept = ordered.take(n)
      if (n == 0 && fromEnd) {
        val last = all.lastOption.getOrElse("")
        val end = endBytes(last, max)
        Clipped(end, hint(Some(Kept(1, all.size, partial = true))))
      } else {
        val shown = (if (fromEnd) kept.reverse else kept).mkString("\n")
        Clipped(shown, hint(Some(Kept(n, all.size, partial = false))))
      }
    }
  }

  /** How many of `ordered`'s first lines, at most `maxLines`, fit in `max` bytes with the
    * newlines joining them.
    */
  private def fitting(ordered: Vector[String], max: Int, maxLines: Int): Int = {
    var used = 0
    var n = 0
    var fits = true
    while (fits && n < ordered.size && n < maxLines) {
      val size = bytes(ordered(n)) + (if (n > 0) 1 else 0)
      if (used + size <= max) { used += size; n += 1 }
      else fits = false
    }
    n
  }

  /** The start of `s` in at most `max` bytes of UTF-8, ending on a character. */
  def prefix(s: String, max: Int): String = {
    val b = s.getBytes(UTF_8)
    var end = b.length.min(max.max(0))
    while (end > 0 && end < b.length && (b(end) & 0xc0) == 0x80) end -= 1
    new String(b, 0, end, UTF_8)
  }

  /** The end of `s` in at most `max` bytes of UTF-8, starting on a character. */
  private def endBytes(s: String, max: Int): String = {
    val b = s.getBytes(UTF_8)
    var start = (b.length - max).max(0)
    while (start < b.length && (b(start) & 0xc0) == 0x80) start += 1
    new String(b, start, b.length - start, UTF_8)
  }
}

/** What [[Clipped]] kept of a text: `lines` of its `total`, from the head or the tail;
  * `partial` when the one line kept is only the end of the text's last line.
  */
final case class Kept(lines: Int, total: Int, partial: Boolean)

/** What [[Clipped.ends]] kept of a long text: its `head` and its `tail`, and the `span` left out
  * between them.
  */
final case class Ends private[host] (head: String, span: Ends.Span, tail: String)

object Ends {

  /** What [[Clipped.ends]] left out of a text of `chars` characters: `bytes` of UTF-8 from
    * character `offset`, characters counted as `String` indexes the text; `lines` when it is
    * whole lines.
    */
  final case class Span private[host] (offset: Int, bytes: Int, chars: Int, lines: Option[Lines])

  /** Lines `from` to `to`, counted from 1, of the text's `total`. */
  final case class Lines private[host] (from: Int, to: Int, total: Int)
}
