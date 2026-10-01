package grit.tui.model.text

import grit.tui.model.surface.Style

/** A style applied to `[from, to)` of some text.
  *
  * **Offsets are into the logical, unwrapped text** -- the same coordinate
  * [[Row.startOffset]] and `DocPos.offset` already speak. That is what keeps styled text
  * cheap: wrapping never slices a span, the wrap cache never learns about styles, and
  * `Selection.columnsOn` is untouched. A span is projected onto a wrapped row only when
  * that row is laid out, by [[Span.rebase]].
  */
final case class Span(from: Int, to: Int, style: Style) {

  /** True when this span covers no characters. */
  def isEmpty: Boolean = to <= from
}

object Span {

  /** `spans` clipped to the row starting at `start` and running `length` characters,
    * with their offsets shifted to be row-local. Empty spans are dropped.
    *
    * The one operation that moves a span from document coordinates into row
    * coordinates. Row-local offsets are still *char* offsets, not display columns:
    * turning one into the other is [[Width.columnAtOffset]], the same projection both
    * selection bounds go through (rule 5), and it happens at paint time against the
    * row's own text.
    */
  def rebase(spans: Vector[Span], start: Int, length: Int): Vector[Span] = {
    if (spans.isEmpty) Vector.empty
    else {
      val end = start + length
      val out = Vector.newBuilder[Span]
      var i = 0
      while (i < spans.length) {
        val s = spans(i)
        val from = math.max(s.from, start)
        val to = math.min(s.to, end)
        if (to > from) out += Span(from - start, to - start, s.style)
        i += 1
      }
      out.result()
    }
  }
}

/** Text with styles attached to ranges of it.
  *
  * The composable unit an app builds a styled block out of: `StyledText.of` glues
  * pieces together and does the offset arithmetic, which is the part that is easy to get
  * wrong by hand and impossible to get wrong twice.
  */
final case class StyledText(text: String, spans: Vector[Span] = Vector.empty) {

  /** This text followed by `more`, whose spans are shifted past it. */
  def ++(more: StyledText): StyledText = {
    val shift = text.length
    val moved = Vector.newBuilder[Span]
    var i = 0
    while (i < more.spans.length) {
      val s = more.spans(i)
      moved += Span(s.from + shift, s.to + shift, s.style)
      i += 1
    }
    StyledText(text + more.text, spans ++ moved.result())
  }

  /** This text followed by unstyled `s`. */
  def ++(s: String): StyledText = StyledText(text + s, spans)

  /** The whole of this text given `style`, layered *under* the spans already on it, so
    * a piece that named its own colour keeps it.
    */
  def under(style: Style): StyledText =
    if (style == Style.plain) this
    else StyledText(text, Span(0, text.length, style) +: spans)

  def length: Int = text.length

  def isEmpty: Boolean = text.isEmpty
}

object StyledText {

  val empty: StyledText = StyledText("")

  /** Unstyled text. */
  def apply(text: String): StyledText = StyledText(text, Vector.empty)

  /** `text` entirely in `style`. */
  def styled(text: String, style: Style): StyledText =
    StyledText(
      text,
      if (style == Style.plain) Vector.empty else Vector(Span(0, text.length, style))
    )

  /** `pieces` concatenated, each keeping its own styles. */
  def of(pieces: StyledText*): StyledText = {
    val v = pieces.toVector
    var acc = empty
    var i = 0
    while (i < v.length) { acc = acc ++ v(i); i += 1 }
    acc
  }

  /** `pieces` joined by `sep`, which is unstyled. */
  def join(sep: String, pieces: Vector[StyledText]): StyledText = {
    var acc = empty
    var i = 0
    while (i < pieces.length) {
      if (i > 0) acc = acc ++ sep
      acc = acc ++ pieces(i)
      i += 1
    }
    acc
  }
}
