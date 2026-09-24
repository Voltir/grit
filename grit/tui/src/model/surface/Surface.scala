package grit.tui.model.surface

/** An immutable grid of cells -- the app's model of the screen.
  *
  * Writes off the edge are clipped, never wrapped: wrapping is a layout decision that
  * belongs to whoever produced the text, not to the grid.
  *
  * `cells` holds exactly `size.rows * size.cols` entries, row-major. `blank` and `filled`
  * are the constructors.
  */
final case class Surface(size: Size, cells: Vector[Cell]) {

  private def index(row: Int, col: Int): Int = row * size.cols + col

  private def inside(row: Int, col: Int): Boolean =
    row >= 0 && row < size.rows && col >= 0 && col < size.cols

  /** The cell at (row, col), or blank if outside the grid. */
  def at(row: Int, col: Int): Cell =
    if (inside(row, col)) cells(index(row, col)) else Cell.blank

  /** `cell` at (row, col); a no-op outside the grid. */
  def put(row: Int, col: Int, cell: Cell): Surface =
    if (inside(row, col)) copy(cells = cells.updated(index(row, col), cell)) else this

  /** `text` drawn at (row, col) with `style`, clipped at the right edge and at
    * negative columns.
    */
  def write(row: Int, col: Int, text: String, style: Style = Style.plain): Surface = {
    if (row < 0 || row >= size.rows) this
    else {
      val start = math.max(col, 0)
      val skip = start - col
      val n = math.min(text.length - skip, size.cols - start)
      (0 until math.max(0, n)).foldLeft(this) { (s, i) =>
        s.put(row, start + i, Cell(text.charAt(skip + i), style))
      }
    }
  }

  /** Every cell of `rect` set to `cell`; the rect is clipped to the grid. */
  def fill(rect: Rect, cell: Cell): Surface = {
    val r = rect.clip(size)
    (r.top until r.bottom).foldLeft(this) { (s, row) =>
      (r.left until r.right).foldLeft(s) { (s2, col) => s2.put(row, col, cell) }
    }
  }

  /** Every cell of `rect` with its style rewritten by `f`; the rect is clipped to the
    * grid and glyphs are untouched.
    *
    * Selection highlighting is a mask applied over painted cells, not a style threaded
    * through the component that painted them: that is what lets one projection highlight
    * a transcript and a modal without either knowing a selection exists.
    */
  def restyle(rect: Rect, f: Style => Style): Surface = {
    val r = rect.clip(size)
    (r.top until r.bottom).foldLeft(this) { (s, row) =>
      (r.left until r.right).foldLeft(s) { (s2, col) =>
        val c = s2.at(row, col)
        s2.put(row, col, c.copy(style = f(c.style)))
      }
    }
  }

  /** `other` composited over this surface with its top-left at `origin`; clipped to the
    * grid.
    */
  /** Every cell inside `r` with `ground` layered under its own style ([[Style.over]]): a
    * colour the cell sets stays, one it leaves unset takes the ground's.
    */
  def under(r: Rect, ground: Style): Surface = restyle(r, _.over(ground))

  def blit(other: Surface, origin: Pos): Surface =
    (0 until other.size.rows).foldLeft(this) { (s, row) =>
      (0 until other.size.cols).foldLeft(s) { (s2, col) =>
        s2.put(origin.row + row, origin.col + col, other.at(row, col))
      }
    }

  /** This surface as plain text, one string per row -- what the tests assert on. */
  def lines: Vector[String] =
    (0 until size.rows).toVector.map { row =>
      (0 until size.cols).map(col => at(row, col).ch).mkString
    }
}

object Surface {

  /** A grid of blank cells. */
  def blank(size: Size): Surface = filled(size, Cell.blank)

  /** A grid where every cell is `cell`. */
  def filled(size: Size, cell: Cell): Surface =
    Surface(size, Vector.fill(math.max(0, size.rows * size.cols))(cell))
}

/** A complete statement of what the screen should look like: every cell, plus
  * where the hardware cursor belongs (`None` hides it).
  */
final case class Frame(surface: Surface, cursor: Option[Pos] = None)
