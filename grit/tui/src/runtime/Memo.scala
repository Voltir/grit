package grit.tui.runtime

import grit.tui.components.PaneKey

import grit.tui.components.pane.{Anchor, ViewRow, Viewport}
import grit.tui.model.block.{Block, Overflow}
import grit.tui.model.select.{Doc, DocPos}
import grit.tui.model.surface.Size
import grit.tui.model.text.{Row, Span, Width, Wrap}

/** One document wrapped at one width, keyed by the **blocks themselves**.
  *
  * A block's rows are reused when the block at that position is `eq` to the one they were
  * wrapped from (the fast path: an untouched block in an untouched vector), else when it
  * is `==` to it (a block rebuilt with the same content); anything else re-wraps. There
  * is no revision to forget to bump, so a block that takes another's position cannot be
  * painted with the old one's rows.
  *
  * `starts(e)` is the global row index of block `e`'s first row, and `starts(length)` is
  * the document's total: the scrollbar's geometry and the scroll arithmetic both read it,
  * so there is no separate row index to keep in step.
  */
final case class DocMemo(
    width: Int,
    blocks: Vector[Block],
    rows: Vector[Vector[Row]],
    starts: Vector[Int],
    hits: Long,
    misses: Long
) {

  def total: Int = starts.lastOption.getOrElse(0)

  /** This memo brought up to date with `doc` at `w`. O(1) when the document's block vector
    * is the one last seen; otherwise one `eq` (or `==`) per block and a wrap per changed one.
    */
  def synced(doc: Doc, w0: Int): DocMemo = {
    val w = math.max(1, w0)
    if ((doc.blocks eq blocks) && w == width) { this }
    else {
      val sameWidth = w == width
      val n = doc.blocks.length
      val rb = Vector.newBuilder[Vector[Row]]
      val sb = Vector.newBuilder[Int]
      var h = hits
      var m = misses
      var acc = 0
      var e = 0
      while (e < n) {
        val b = doc.blocks(e)
        val reuse =
          sameWidth && e < blocks.length && {
            val old = blocks(e)
            (old eq b) || old == b
          }
        val rs =
          if (reuse) { h += 1; rows(e) }
          else { m += 1; DocMemo.wrap(b, w) }
        rb += rs
        sb += acc
        acc += rs.length
        e += 1
      }
      sb += acc
      DocMemo(w, doc.blocks, rb.result(), sb.result(), h, m)
    }
  }

  /** The block holding global row `r`, by binary search over [[starts]]. */
  def entryOf(r: Int): Int = {
    var lo = 0
    var hi = blocks.length - 1
    var found = 0
    while (lo <= hi) {
      val mid = (lo + hi) >>> 1
      if (starts(mid) <= r) { found = mid; lo = mid + 1 }
      else { hi = mid - 1 }
    }
    found
  }

  /** The document position that begins global row `r`, if there is one. */
  def posOf(r: Int): Option[DocPos] =
    if (r < 0 || r >= total) { None }
    else {
      val e = entryOf(r)
      rows(e).lift(r - starts(e)).map(row => DocPos(e, row.startOffset))
    }

  /** The global row holding `p`. */
  def rowOf(p: DocPos): Int =
    if (blocks.isEmpty) { 0 }
    else {
      val e = math.max(0, math.min(p.entry, blocks.length - 1))
      val rs = rows(e)
      var i = 0
      var found = 0
      while (i < rs.length) {
        if (rs(i).startOffset <= p.offset) { found = i }
        i += 1
      }
      starts(e) + found
    }

  /** The first row a viewport of `height` shows from `anchor`. */
  def topFor(anchor: Anchor, height: Int): Int = anchor match {
    case Anchor.Bottom => math.max(0, total - height)
    case Anchor.At(p) => math.max(0, math.min(rowOf(p), math.max(0, total - 1)))
  }

  /** The anchor after scrolling `delta` rows from `top` at `height`: past the end is the
    * tail again, so "follow the tail" is a state the user scrolls back into.
    */
  def scrolled(top: Int, delta: Int, height: Int): Anchor = {
    val t = math.max(0, top + delta)
    if (t + height >= total) { Anchor.Bottom }
    else { posOf(t).fold(Anchor.Bottom)(Anchor.At(_)) }
  }

  /** The rows visible at `size` from global row `top`: O(rows shown), no wrapping. */
  def viewport(top: Int, size: Size): Viewport = {
    val need = math.max(0, math.min(size.rows, total - top))
    val out = Vector.newBuilder[ViewRow]
    var r = top
    var e = if (need > 0) entryOf(top) else 0
    while (r < top + need) {
      while (e + 1 < blocks.length && starts(e + 1) <= r) { e += 1 }
      val b = blocks(e)
      val row = rows(e)(r - starts(e))
      val rule = b match {
        case _: Block.Separator => true
        case _ => false
      }
      out += ViewRow(
        e,
        row.startOffset,
        row.text,
        rule,
        b.groundAt(row.startOffset),
        Span.rebase(b.spans, row.startOffset, row.text.length)
      )
      r += 1
    }
    Viewport(size, out.result())
  }
}

object DocMemo {

  val empty: DocMemo = DocMemo(0, Vector.empty, Vector.empty, Vector(0), 0L, 0L)

  /** One block's rows at `w`: wrapped, or cut per line for a truncating diff. */
  def wrap(b: Block, w: Int): Vector[Row] = b match {
    case d: Block.Diff if d.overflow == Overflow.Truncate =>
      var base = 0
      d.lines.map { line =>
        val r = Row(base, line.substring(0, Width.offsetAtColumn(line, w)))
        base += line.length + 1
        r
      }
    case _ => Wrap.wrap(b.text, w)
  }
}

/** Every document the runtime has wrapped, by the key its pane was painted under. Owned
  * by the runtime and threaded through each paint; a key not painted in a frame is dropped
  * with it.
  */
final case class Memo(docs: Map[PaneKey, DocMemo]) {
  def hits: Long = docs.valuesIterator.map(_.hits).sum
  def misses: Long = docs.valuesIterator.map(_.misses).sum
}

object Memo {
  val empty: Memo = Memo(Map.empty)
}
