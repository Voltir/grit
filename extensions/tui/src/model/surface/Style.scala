package grit.tui.model.surface

/** A 24-bit colour.
  *
  * Truecolor only: grit.tui emits `38;2;r;g;b` and never probes the terminal for a depth.
  * A palette is the app's to own -- these are the sixteen names every terminal already
  * agrees on, given as RGB so nothing downstream has to know two encodings.
  */
final case class Color(r: Int, g: Int, b: Int)

object Color {

  /** `#rrggbb`, with or without the hash. */
  def hex(s: String): Color = {
    val h = if (s.startsWith("#")) s.substring(1) else s
    val v = java.lang.Integer.parseInt(h, 16)
    Color((v >> 16) & 0xff, (v >> 8) & 0xff, v & 0xff)
  }

  val Black: Color = Color(0, 0, 0)
  val Red: Color = Color(0xcd, 0x31, 0x31)
  val Green: Color = Color(0x0d, 0xbc, 0x79)
  val Yellow: Color = Color(0xe5, 0xe5, 0x10)
  val Blue: Color = Color(0x24, 0x72, 0xc8)
  val Magenta: Color = Color(0xbc, 0x3f, 0xbc)
  val Cyan: Color = Color(0x11, 0xa8, 0xcd)
  val White: Color = Color(0xe5, 0xe5, 0xe5)

  val BrightBlack: Color = Color(0x66, 0x66, 0x66)
  val BrightRed: Color = Color(0xf1, 0x4c, 0x4c)
  val BrightGreen: Color = Color(0x23, 0xd1, 0x8b)
  val BrightYellow: Color = Color(0xf5, 0xf5, 0x43)
  val BrightBlue: Color = Color(0x3b, 0x8e, 0xea)
  val BrightMagenta: Color = Color(0xd6, 0x70, 0xd6)
  val BrightCyan: Color = Color(0x29, 0xb8, 0xdb)
  val BrightWhite: Color = Color(0xff, 0xff, 0xff)
}

/** The attributes a cell is drawn with.
  *
  * A total description of one cell, and also the thing that layers over another: an
  * unset colour means *inherit*, not "terminal default", so a mask can add a foreground
  * without erasing the background the row underneath already carries. See [[over]].
  */
final case class Style(
    fg: Option[Color] = None,
    bg: Option[Color] = None,
    bold: Boolean = false,
    dim: Boolean = false,
    italic: Boolean = false,
    underline: Boolean = false,
    reverse: Boolean = false
) {

  /** This style layered on top of `under`: set colours win, attributes accumulate.
    *
    * Layering can never *clear* an attribute, which is what a mask wants -- the
    * selection's reverse and the modal's dim are applied over cells whose own styles
    * they must not know about. `copy` remains how an attribute is turned off.
    *
    * Associative, with [[Style.plain]] as its identity on both sides.
    */
  def over(under: Style): Style = Style(
    fg = if (fg.isDefined) fg else under.fg,
    bg = if (bg.isDefined) bg else under.bg,
    bold = bold || under.bold,
    dim = dim || under.dim,
    italic = italic || under.italic,
    underline = underline || under.underline,
    reverse = reverse || under.reverse
  )

  /** `on` layered on top of this -- [[over]] read left to right, which is the order a
    * style is usually built in: `Style.fg(c) + Style.Bold`.
    */
  def +(on: Style): Style = on.over(this)

  /** This style with `c` as its foreground. */
  def withFg(c: Color): Style = copy(fg = Some(c))

  /** This style with `c` as its background. */
  def withBg(c: Color): Style = copy(bg = Some(c))
}

object Style {

  /** No colour and no attributes: the identity of [[Style.over]]. */
  val plain: Style = Style()

  val Bold: Style = Style(bold = true)
  val Dim: Style = Style(dim = true)
  val Italic: Style = Style(italic = true)
  val Underline: Style = Style(underline = true)
  val Reverse: Style = Style(reverse = true)

  /** A foreground colour and nothing else. */
  def fg(c: Color): Style = Style(fg = Some(c))

  /** A background colour and nothing else. */
  def bg(c: Color): Style = Style(bg = Some(c))
}

/** One character cell: a glyph and the attributes it is drawn with. */
final case class Cell(ch: Char, style: Style = Style.plain)

object Cell {
  val blank: Cell = Cell(' ', Style.plain)
}
