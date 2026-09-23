package grit.tui.components.pane

import grit.tui.model.block.{Block, Overflow}
import grit.tui.model.select.{Doc, DocPos, Selection}
import grit.tui.model.surface.{PaneId, Pos, Rect, Size, Style, Surface}
import grit.tui.model.text.{ContentId, Row, Span, Width, WrapCache}

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
  * styles clipped and shifted into this row's own coordinates. Both are resolved here,
  * during layout, because `view` may not do work per cell (rule 7) and because a span in
  * document coordinates cannot be painted without knowing which row it landed on.
  */
final case class ViewRow(
    entry: Int,
    start: Int,
    text: String,
    rule: Boolean = false,
    ground: Style = Style.plain,
    spans: Vector[Span] = Vector.empty
)

/** What a pane decided to show, at the size it was asked about.
  *
  * Produced in `update` (where wrapping is allowed to happen) and consumed twice: by
  * `view`, which paints it, and by input handling, which reads it backwards. Those two
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

/** A pane of scrollable text: a document, where it is being read from, and the wrapped
  * rows it has already paid for.
  *
  * Scroll state lives here rather than in the app, so a two-column layout or a modal over
  * a transcript is two panes each reading independently rather than one app juggling two
  * sets of scroll variables.
  *
  * A pane does not keep its own viewport: what was last laid out lives on `Panes`,
  * because hit-testing must invert *what was painted* and painting is the frame's, not
  * the pane's. Produce one with [[viewport]] or [[scrolledBy]] -- both return the pane
  * with its cache stored back -- and store it with `Panes.layout`; `Panes.withDoc` and
  * `Panes.withAnchor` re-lay the pane out at its last painted size, so a viewport
  * describing an older document is not something a caller can forget to refresh.
  *
  * Beside the cache lives the pane's [[RowIndex]]: one `DocPos` per wrapped row of the
  * whole document, which the scrollbar's geometry speaks and the viewport deliberately
  * never computes. It is maintained through the same funnel, by revision reconcile:
  * [[reindexedAt]] re-wraps only blocks whose revision moved. Cache keys are
  * `paneId#entryIndex` at the entry's revision, so a streaming last entry re-wraps one
  * entry. A document that *reorders* entries must bump their revisions, and the index
  * alone will not notice.
  */
final case class TextPane(
    id: PaneId,
    doc: Doc,
    anchor: Anchor = Anchor.Bottom,
    cache: WrapCache = WrapCache.empty(80),
    index: RowIndex = RowIndex.empty
) {

  /** This pane showing `d`. */
  def withDoc(d: Doc): TextPane = copy(doc = d)

  /** This pane reading from `a`. */
  def withAnchor(a: Anchor): TextPane = copy(anchor = a)

  /** The rows visible at `size`, and this pane with them and the cache stored back.
    *
    * The walk stops as soon as the viewport is full, so its cost is the rows shown and
    * not the entries held -- the difference between a 400-entry transcript that scrolls
    * and one that re-wraps its whole history every frame.
    */
  def viewport(size: Size): (Viewport, TextPane) = {
    val need = math.max(0, size.rows)
    val c0 = cache.atWidth(math.max(1, size.cols))
    val (rows, c1) = anchor match {
      case Anchor.Bottom => fromBottom(need, c0)
      case Anchor.At(p) => fromTop(doc.clamp(p), need, c0)
    }
    val vp = Viewport(size, rows)
    (vp, copy(cache = c1))
  }

  /** This pane scrolled `delta` rows (negative walks back through the document), with the
    * viewport it was scrolled to: rows visible at `size`, and the pane with the anchor
    * and cache that produced them.
    *
    * Scrolling past the end returns to [[Anchor.Bottom]] rather than leaving a
    * half-empty screen anchored somewhere below the last entry, so "follow the tail" is
    * a state the user can scroll back into rather than a mode they have to re-arm.
    */
  def scrolledBy(delta: Int, size: Size): (Viewport, TextPane) = {
    val (vp, p) = viewport(size)
    vp.rows.headOption match {
      case None => (vp, p)
      case Some(head) =>
        val (rows, c1) = p.rowsOf(head.entry, p.cache)
        val (e2, r2, c2) = p.step(head.entry, rowIndexFor(rows, head.start), delta, c1)
        val (rows2, c3) = p.rowsOf(e2, c2)
        val at = DocPos(e2, rows2.lift(r2).map(_.startOffset).getOrElse(0))
        val (test, moved) = p.copy(anchor = Anchor.At(at), cache = c3).viewport(size)
        if (test.rows.length < size.rows) { moved.withAnchor(Anchor.Bottom).viewport(size) }
        else { (test, moved) }
    }
  }

  /** The row index for this pane's document at width `w`, reconciled against the
    * records the last index kept: unchanged revisions keep their wrapped rows, changed
    * ones re-wrap through this pane's own warm cache, and a width change rebuilds. The
    * pane comes back with the cache the reconcile used stored in it, so the viewport
    * that follows wraps at the same width and hits.
    */
  def reindexedAt(w: Int): (RowIndex, TextPane) = {
    val c0 = cache.atWidth(w)
    val (idx, c1) = RowIndex.reconciled(index, id, doc, w, c0, rowsOf)
    (idx, copy(cache = c1, index = idx))
  }

  /** The wrapped rows of one block, from the cache when its revision is unchanged.
    *
    * A `Truncate` diff bypasses the cache: cutting a line to the width is a slice per
    * line, not a wrap, and there is nothing worth remembering. Everything else -- empty
    * separators included -- wraps through the cache as before. Reachable from the pane
    * package because the row index reconciles through the same source the viewport
    * reads from: one row structure, painted and indexed alike.
    */
  private[pane] def rowsOf(e: Int, c: WrapCache): (Vector[Row], WrapCache) = {
    val b = doc.entry(e).getOrElse(Block.Text(""))
    b match {
      case d: Block.Diff if d.overflow == Overflow.Truncate =>
        (truncated(d.text, c.width), c)
      case _ => c.rowsFor(ContentId(s"${id.value}#$e"), b.rev, b.text)
    }
  }

  /** `text` as one row per logical line, each cut at `w` display columns. A cut that
    * lands inside a wide glyph resolves to that glyph's start, so a glyph is never split
    * -- the row may come up a column short, which is the honest price.
    */
  private def truncated(text: String, w: Int): Vector[Row] = {
    val out = Vector.newBuilder[Row]
    var base = 0
    var li = 0
    val logical = text.split("\n", -1)
    while (li < logical.length) {
      val line = logical(li)
      out += Row(base, line.substring(0, Width.offsetAtColumn(line, w)))
      base += line.length + 1
      li += 1
    }
    out.result()
  }

  /** Block `e`'s wrapped rows as viewport rows: the block's ground carried onto each,
    * and its spans clipped and shifted into each row's own coordinates.
    *
    * This is where document-coordinate styles become row-coordinate ones, and it happens
    * during layout rather than during painting because `view` may not do work per cell
    * (rule 7). The rebase is O(spans), not O(cells).
    */
  private def viewRows(e: Int, rows: Vector[Row]): Vector[ViewRow] = {
    val b = doc.entry(e)
    val rule = b match {
      case Some(_: Block.Separator) => true
      case _ => false
    }
    val spans = b.map(_.spans).getOrElse(Vector.empty)
    rows.map { r =>
      val ground = b.map(_.groundAt(r.startOffset)).getOrElse(Style.plain)
      ViewRow(
        e,
        r.startOffset,
        r.text,
        rule,
        ground,
        Span.rebase(spans, r.startOffset, r.text.length)
      )
    }
  }

  /** The last `need` rows of the document: entries walked backwards until full. */
  private def fromBottom(need: Int, c0: WrapCache): (Vector[ViewRow], WrapCache) = {
    var e = doc.length - 1
    var acc = Vector.empty[ViewRow]
    var c = c0
    while (e >= 0 && acc.length < need) {
      val (rows, c1) = rowsOf(e, c)
      c = c1
      acc = viewRows(e, rows) ++ acc
      e -= 1
    }
    (acc.takeRight(need), c)
  }

  /** `need` rows starting at the row that holds `from`: entries walked forwards. */
  private def fromTop(from: DocPos, need: Int, c0: WrapCache): (Vector[ViewRow], WrapCache) = {
    if (doc.isEmpty || need == 0) { (Vector.empty, c0) }
    else {
      val (first, c1) = rowsOf(from.entry, c0)
      var acc = viewRows(from.entry, first.drop(rowIndexFor(first, from.offset)))
      var e = from.entry + 1
      var c = c1
      while (e < doc.length && acc.length < need) {
        val (rows, c2) = rowsOf(e, c)
        c = c2
        acc = acc ++ viewRows(e, rows)
        e += 1
      }
      (acc.take(need), c)
    }
  }

  /** `delta` rows away from `(entry, row)` in document order, clamped at both ends. */
  private def step(entry: Int, row: Int, delta: Int, c0: WrapCache): (Int, Int, WrapCache) = {
    var e = entry
    var r = row
    var c = c0
    var n = delta
    var stop = false
    while (n > 0 && !stop) {
      val (rows, c1) = rowsOf(e, c)
      c = c1
      if (r + 1 < rows.length) { r += 1 }
      else if (e + 1 < doc.length) { e += 1; r = 0 }
      else { stop = true }
      n -= 1
    }
    while (n < 0 && !stop) {
      if (r > 0) { r -= 1 }
      else if (e > 0) {
        e -= 1
        val (rows, c1) = rowsOf(e, c)
        c = c1
        r = math.max(0, rows.length - 1)
      } else { stop = true }
      n += 1
    }
    (e, r, c)
  }

  /** The index of the wrapped row holding `offset` -- the last row starting at or before
    * it, and 0 when the entry wrapped to nothing.
    */
  private def rowIndexFor(rows: Vector[Row], offset: Int): Int = {
    var i = 0
    var found = 0
    while (i < rows.length) {
      if (rows(i).startOffset <= offset) { found = i }
      i += 1
    }
    found
  }
}
