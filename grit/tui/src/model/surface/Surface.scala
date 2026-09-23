package grit.tui.model.surface

/** An immutable grid of cells -- the app's model of the screen.
  *
  * Writes off the edge are clipped, never wrapped: wrapping is a layout decision that
  * belongs to whoever produced the text, not to the grid.
  *
  * `cells` holds exactly `size.rows * size.cols` entries, row-major, and `panes` records
  * where named panes landed -- blit appends, innermost last. `blank` and `filled` are
  * the constructors.
  */
final case class Surface(size: Size, cells: Vector[Cell], panes: Vector[Placement] = Vector.empty) {

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

  /** `other` composited over this surface with its top-left at `origin`; clipped
    * to the grid.
    *
    * With a `PaneId`, the painted area is recorded as a placement -- innermost last --
    * and any placements `other` itself carries come over translated, after this pane's
    * own rect, so hit-testing finds the innermost pane first.
    */
  def blit(other: Surface, origin: Pos): Surface = blit(other, origin, None)

  /** See [[blit]]. */
  def blit(other: Surface, origin: Pos, pane: PaneId): Surface = blit(other, origin, Some(pane))

  private def blit(other: Surface, origin: Pos, pane: Option[PaneId]): Surface = {
    val painted = (0 until other.size.rows).foldLeft(this) { (s, row) =>
      (0 until other.size.cols).foldLeft(s) { (s2, col) =>
        s2.put(origin.row + row, origin.col + col, other.at(row, col))
      }
    }
    val visible = Rect(origin.row, origin.col, other.size.rows, other.size.cols).clip(size)
    val outer = pane match {
      case Some(id) if visible.rows > 0 && visible.cols > 0 =>
        Vector(Placement(id, visible))
      case _ => Vector.empty
    }
    val carried = other.panes
      .collect { case Placement(id, r) =>
        Placement(id, r.translate(origin).clip(size))
      }
      .filter(p => p.rect.rows > 0 && p.rect.cols > 0)
    painted.copy(panes = painted.panes ++ outer ++ carried)
  }

  /** This surface as plain text, one string per row -- what the tests assert on. */
  def lines: Vector[String] =
    (0 until size.rows).toVector.map { row =>
      (0 until size.cols).map(col => at(row, col).ch).mkString
    }

  /** Where this surface's named panes landed -- the inverse of rendering, read back off
    * what was painted. Lookups are innermost-first, matching [[Hit.paneAt]].
    */
  def placements: Placements = Placements(panes)
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
final case class Frame(surface: Surface, cursor: Option[Pos] = None) {

  /** Where the frame's named panes landed. */
  def placements: Placements = surface.placements
}
