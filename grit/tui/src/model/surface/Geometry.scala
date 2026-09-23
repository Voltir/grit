package grit.tui.model.surface

/** A position on a grid, 0-based. */
final case class Pos(row: Int, col: Int)

/** A grid dimension of `rows` x `cols` cells. */
final case class Size(rows: Int, cols: Int)

object Size {

  /** The paintable screen at the terminal's `size`: one column narrower, because the
    * last column is owed back (rule 2 -- deferred wrap parks the cursor there, and a
    * following `ESC[K` erases the character just written).
    *
    * This is where the rule lives: the runtime translates the terminal's size here,
    * before the app ever sees it, so no app can forget it and nothing else compensates
    * -- a frame is the paintable screen, and the painter writes its full width.
    */
  def screen(size: Size): Size = Size(size.rows, math.max(1, size.cols - 1))
}

/** A rectangle of cells: its top-left corner plus its extent. */
final case class Rect(top: Int, left: Int, rows: Int, cols: Int) {

  /** Exclusive bottom edge. */
  def bottom: Int = top + rows

  /** Exclusive right edge. */
  def right: Int = left + cols

  /** Whether `p` lands on one of this rect's cells. */
  def contains(p: Pos): Boolean =
    p.row >= top && p.row < bottom && p.col >= left && p.col < right

  /** This rect's extent, for handing to something that paints by size. */
  def size: Size = Size(rows, cols)

  /** This rect with `n` cells taken off every edge -- the inside of a frame. Never
    * inverts: an inset larger than the rect is an empty rect at its centre edge.
    */
  def inset(n: Int): Rect =
    Rect(top + n, left + n, math.max(0, rows - 2 * n), math.max(0, cols - 2 * n))

  /** The part of this rect inside `size`; empty if none is. */
  def clip(size: Size): Rect = {
    val t = math.max(top, 0)
    val l = math.max(left, 0)
    val b = math.min(bottom, size.rows)
    val r = math.min(right, size.cols)
    Rect(t, l, math.max(0, b - t), math.max(0, r - l))
  }

  /** This rect moved by (row, col) deltas of `p`. */
  def translate(p: Pos): Rect = Rect(top + p.row, left + p.col, rows, cols)
}
