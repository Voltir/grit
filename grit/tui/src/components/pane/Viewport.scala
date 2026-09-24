package grit.tui.components.pane

import grit.tui.model.select.{DocPos, Selection}
import grit.tui.model.surface.{Pos, Rect, Size, Style, Surface}
import grit.tui.model.text.{Span, Width}

/** Where a pane is reading from.
  *
  * `Bottom` follows the end of the document -- the transcript behaviour, where new
  * entries push the view along. `At` pins a *document* position, not a wrapped row: that
  * is what lets the window narrow underneath the reader without moving them, and what
  * makes a drag that spans several screens describable at all.
  */
enum Anchor {
  case Bottom
  case At(pos: DocPos)
}

/** One row of a laid-out viewport: which block it came from, its offset into that
  * block's logical text, and the text actually shown.
  *
  * The offset is the provenance the whole selection model runs on -- without it a screen
  * position can only be mapped back to what a particular width happened to break.
  *
  * `rule` marks a separator row: painted as a rule across the pane's width, but logically
  * empty, so a click on it lands at the row's start and a selection over it copies an
  * empty line. It is a rule, not content.
  *
  * `ground` is the style under the whole row, edge to edge, and `spans` are the block's
  * styles clipped and shifted into this row's own coordinates. Both are resolved when the
  * row is cut, because a span in document coordinates cannot be painted without knowing
  * which row it landed on.
  */
final case class ViewRow(
    entry: Int,
    start: Int,
    text: String,
    rule: Boolean = false,
    ground: Style = Style.plain,
    spans: Vector[Span] = Vector.empty
)

/** What a pane shows, at the size it was painted at.
  *
  * Cut from already-wrapped rows (the runtime's wrap memo) and consumed twice: by the
  * paint, which renders it, and by input routing, which reads it backwards. Those two
  * directions agreeing is the property [[docPosAt]] exists to hold.
  */
final case class Viewport(size: Size, rows: Vector[ViewRow]) {

  /** The document position of the first visible row, if anything is visible. */
  def top: Option[DocPos] = rows.headOption.map(r => DocPos(r.entry, r.start))

  /** The document position under a pane-local screen position.
    *
    * None outside the viewport's own rect: a position that was never painted has no
    * document position, and answering one anyway is how a selection escapes a pane.
    * Inside the rect but past the last row is the end of the document, so a drag into
    * the empty space below a short transcript selects to the end rather than snapping
    * back.
    */
  def docPosAt(pos: Pos): Option[DocPos] =
    if (pos.row < 0 || pos.row >= size.rows || pos.col < 0 || pos.col >= size.cols) { None }
    else if (pos.row < rows.length) {
      val r = rows(pos.row)
      Some(DocPos(r.entry, r.start + Width.offsetAtColumn(r.text, pos.col)))
    } else {
      rows.lastOption match {
        case Some(r) => Some(DocPos(r.entry, r.start + r.text.length))
        case None => Some(DocPos.zero)
      }
    }

  /** The rows painted onto a surface of this viewport's size, with `selection` shown as
    * a reverse-video mask.
    *
    * No wrapping and no measuring of anything not already measured: the rows arrived
    * wrapped, and this walks them once. The highlight is applied as a mask over painted
    * cells rather than as a style threaded through whoever produced the text -- which is
    * why a pane needs to know nothing about selection to be selectable.
    */
  def render(selection: Option[Selection]): Surface = {
    val painted = rows.zipWithIndex.foldLeft(Surface.blank(size)) { case (s, (row, r)) =>
      val ground =
        if (row.ground == Style.plain) s
        else s.restyle(Rect(r, 0, 1, size.cols), row.ground.over)
      val body =
        if (row.rule) { ground.write(r, 0, "─" * size.cols, row.ground) }
        else { ground.write(r, 0, row.text, row.ground) }
      paint(body, r, row)
    }
    selection match {
      case None => painted
      case Some(sel) =>
        rows.zipWithIndex.foldLeft(painted) { case (s, (row, r)) =>
          val (from, to) = sel.columnsOn(row.entry, row.start, row.text)
          if (to > from) { s.restyle(Rect(r, from, 1, to - from), _.copy(reverse = true)) }
          else { s }
        }
    }
  }

  /** Row `r`'s spans laid over what has already been painted there.
    *
    * A span is a mask, exactly as the selection below it is: `restyle` layers the span's
    * style over whatever the cell already carries, so a foreground named by a span keeps
    * the ground underneath it and the selection's reverse keeps both. Char offsets become
    * display columns through [[Width.columnAtOffset]] -- the same projection both
    * selection bounds go through, and the reason a span and the highlight over it cannot
    * disagree about where a wide glyph put them (rule 5).
    */
  private def paint(s: Surface, r: Int, row: ViewRow): Surface = {
    var acc = s
    var i = 0
    while (i < row.spans.length) {
      val sp = row.spans(i)
      val from = Width.columnAtOffset(row.text, sp.from)
      val to = Width.columnAtOffset(row.text, sp.to)
      if (to > from) { acc = acc.restyle(Rect(r, from, 1, to - from), sp.style.over) }
      i += 1
    }
    acc
  }
}

object Viewport {
  def empty: Viewport = Viewport(Size(0, 0), Vector.empty)
}
