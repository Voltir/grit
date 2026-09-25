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

  /** `text` whole when it fits, else its first whole lines that do: none when its first
    * line alone is over [[MaxBytes]]. `hint` is told what was kept, `None` when nothing was
    * cut, and gives the hint.
    */
  def head(text: String, hint: Option[Kept] -> Option[String]): Clipped =
    clip(text, hint, fromEnd = false)

  /** `text` whole when it fits, else its last lines that do; when its last line alone is
    * over [[MaxBytes]], the end of that line, cut at a character. `hint` as for [[head]].
    */
  def tail(text: String, hint: Option[Kept] -> Option[String]): Clipped =
    clip(text, hint, fromEnd = true)

  /** `text`'s lines: split at `\n`, a final `\n` ending the last line rather than starting
    * another.
    */
  def lines(text: String): Vector[String] =
    if (text.isEmpty) Vector.empty
    else {
      val all = text.split("\n", -1).toVector
      if (text.endsWith("\n")) all.dropRight(1) else all
    }

  private def bytes(s: String): Int = s.getBytes(UTF_8).length

  private def clip(
      text: String,
      hint: Option[Kept] -> Option[String],
      fromEnd: Boolean
  ): Clipped = {
    val all = lines(text)
    if (all.size <= MaxLines && bytes(text) <= MaxBytes) Clipped(text, hint(None))
    else {
      val ordered = if (fromEnd) all.reverse else all
      // The most lines, from the kept end, whose bytes and joining newlines fit.
      var used = 0
      var n = 0
      var fits = true
      while (fits && n < ordered.size && n < MaxLines) {
        val size = bytes(ordered(n)) + (if (n > 0) 1 else 0)
        if (used + size <= MaxBytes) { used += size; n += 1 }
        else fits = false
      }
      val kept = ordered.take(n)
      if (n == 0 && fromEnd) {
        val last = all.lastOption.getOrElse("")
        val end = endBytes(last, MaxBytes)
        Clipped(end, hint(Some(Kept(1, all.size, partial = true))))
      } else {
        val shown = (if (fromEnd) kept.reverse else kept).mkString("\n")
        Clipped(shown, hint(Some(Kept(n, all.size, partial = false))))
      }
    }
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
