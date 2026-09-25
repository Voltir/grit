package grit.tui.wire.paint

import grit.tui.model.surface.*

/** A minimal VT interpreter: just enough terminal to verify the painter by painting --
  * CUP, SGR, EL, ED, CR, LF, deferred wrap -- the port of the ~40-line emu.py prototype
  * from ../tui-spike-layoutz (FINDINGS 5). It tracks glyph AND style per cell, so tests
  * can sweep the full painted grid, including the reverse-video mask that caught the
  * selection bug next door.
  *
  * Deferred wrap is xterm's, not pyte's: a glyph written into the last column leaves the
  * cursor *on* that column with a wrap pending, so a following `ESC[K` erases the glyph
  * just written (design rule 2's hazard) and a following glyph wraps first. pyte and tmux
  * park the cursor one past the edge instead, where the erase reaches nothing, so neither
  * is a reference for this.
  */
final class Vt(val rows: Int, val cols: Int) {
  private var grid: Vector[Vector[Cell]] = Vector.fill(rows, cols)(Cell.blank)
  private var row: Int = 0
  private var col: Int = 0
  private var wrapPending: Boolean = false
  private var style: Style = Style.plain
  private var modes: Map[String, Boolean] = Map.empty

  def cells: Vector[Vector[Cell]] = grid

  /** The painted glyphs, one string per row. */
  def text: Vector[String] = grid.map(_.map(_.ch).mkString)

  /** The reverse-video mask, one string per row: 'x' where the cell is reverse. */
  def reverseMask: Vector[String] = grid.map(_.map(c => if (c.style.reverse) 'x' else '.').mkString)

  /** The painted styles, one row per row -- the full attribute grid the painter claims
    * it produced, read back off the bytes it actually sent.
    */
  def styles: Vector[Vector[Style]] = grid.map(_.map(_.style))

  /** A mask over `p`, one string per row: 'x' where the cell's style satisfies it. The
    * general form of [[reverseMask]]; `fgMask(c)` and `bgMask(c)` are the colour cases.
    */
  def mask(p: Style => Boolean): Vector[String] =
    grid.map(_.map(c => if (p(c.style)) 'x' else '.').mkString)

  /** Where `c` is the foreground. */
  def fgMask(c: Color): Vector[String] = mask(_.fg.contains(c))

  /** Where `c` is the background. */
  def bgMask(c: Color): Vector[String] = mask(_.bg.contains(c))

  def cursor: Pos = Pos(row, col)

  def flag(name: String): Boolean = modes.getOrElse(name, false)

  def feed(s: String): Unit = {
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      if (c == 0x1b && i + 1 < s.length && s.charAt(i + 1) == '[')
        i = csi(s, i + 1)
      else if (c == '\n') { wrapPending = false; lineFeed() }
      else if (c == '\r') { wrapPending = false; col = 0 }
      else put(c)
      i += 1
    }
  }

  /** A glyph at the cursor. In the last column the cursor stays put with a wrap pending,
    * and the next glyph wraps before it is written.
    */
  private def put(c: Char): Unit = {
    if (wrapPending) { wrapPending = false; col = 0; lineFeed() }
    grid = grid.updated(row, grid(row).updated(col, Cell(c, style)))
    if (col == cols - 1) wrapPending = true else col += 1
  }

  private def lineFeed(): Unit = {
    row += 1
    if (row >= rows) { scroll(); row = rows - 1 }
  }

  private def scroll(): Unit =
    grid = grid.tail :+ Vector.fill(cols)(Cell.blank)

  /** Parses a CSI sequence whose '[' is at `start`; returns the index after it. */
  private def csi(s: String, start: Int): Int = {
    var i = start + 1
    val body = new StringBuilder
    while (i < s.length && !(s.charAt(i) >= 0x40 && s.charAt(i) <= 0x7e)) {
      body += s.charAt(i)
      i += 1
    }
    if (i >= s.length) return s.length
    val params = body.result()
    s.charAt(i) match {
      case 'H' => cup(params)
      case 'm' => sgr(params)
      case 'K' => eraseLine()
      case 'J' => if (params == "2") grid = Vector.fill(rows, cols)(Cell.blank)
      case 'h' => setMode(params, true)
      case 'l' => setMode(params, false)
      case _ => ()
    }
    i // feed's i += 1 steps past the final byte
  }

  private def cup(params: String): Unit = {
    val p = params.split(";").map(p => if (p.isEmpty) 1 else p.toInt)
    row = math.max(0, math.min(rows - 1, p(0) - 1))
    col = math.max(0, math.min(cols - 1, if (p.length > 1) p(1) - 1 else 0))
    wrapPending = false
  }

  /** SGR, including the extended colour forms.
    *
    * The loop advances by more than one param on `38`/`48`: `38;2;r;g;b` is five and
    * `38;5;n` is three. Consuming them one at a time would desync the whole sequence and
    * silently turn a colour's blue channel into an attribute -- the reason this is a
    * counted walk and not a `foreach`.
    */
  private def sgr(params: String): Unit = {
    val codes = if (params.isEmpty) Array("0") else params.split(";")
    def num(i: Int): Int = if (i < codes.length) codes(i).toIntOption.getOrElse(0) else 0
    var i = 0
    while (i < codes.length) {
      var step = 1
      codes(i) match {
        case "0" => style = Style.plain
        case "1" => style = style.copy(bold = true)
        case "2" => style = style.copy(dim = true)
        case "3" => style = style.copy(italic = true)
        case "4" => style = style.copy(underline = true)
        case "7" => style = style.copy(reverse = true)
        case "39" => style = style.copy(fg = None)
        case "49" => style = style.copy(bg = None)
        case "38" | "48" =>
          val fore = codes(i) == "38"
          num(i + 1) match {
            case 2 =>
              val c = Color(num(i + 2), num(i + 3), num(i + 4))
              style = if (fore) style.copy(fg = Some(c)) else style.copy(bg = Some(c))
              step = 5
            case 5 =>
              // grit.tui never emits indexed colour; parsed only so it cannot desync a stream.
              step = 3
            case _ => step = 2
          }
        case _ => ()
      }
      i += step
    }
  }

  /** EL 0, with background colour erase: the erased cells take the current background
    * and nothing else, as xterm and its descendants do. With a wrap pending it erases
    * from the last column, the glyph just written there, and the wrap is cancelled.
    */
  private def eraseLine(): Unit = {
    wrapPending = false
    val erased = Cell(' ', Style(bg = style.bg))
    grid = grid.updated(row, grid(row).patch(col, Vector.fill(cols - col)(erased), cols - col))
  }

  private def setMode(params: String, on: Boolean): Unit =
    if (params.startsWith("?")) modes = modes.updated(params.tail, on)
}
