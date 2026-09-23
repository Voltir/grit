package grit.tui.components.pane

import grit.tui.model.block.Block
import grit.tui.model.select.{Doc, DocPos}
import grit.tui.model.surface.PaneId
import grit.tui.model.text.{Row, WrapCache}

/** One `DocPos` per wrapped row of a pane's document, in document order -- the row
  * space the scrollbar's geometry speaks.
  *
  * This is the index the scrollbar needs and the viewport deliberately never computes:
  * `content` is the *whole* wrapped document and a thumb drag names a row anywhere in
  * it, while the viewport walk stops as soon as the visible rows are full. The index is
  * a document-and-width fact, so it lives beside the wrap cache on the pane and is
  * maintained by the same funnel that maintains the cache ([[Panes.withDoc]] and
  * friends) -- a caller cannot forget to rebuild it, because there is no path to the
  * document that does not.
  *
  * It is maintained **by revision reconcile**, not by rebuilding: the records of which
  * block was wrapped at which revision are kept, an unchanged revision keeps its rows
  * (no wrap, no allocation), and only a block whose revision moved re-wraps -- through
  * the pane's own warm cache, so a stream tick pays for exactly one block. A width
  * change rebuilds; a wholesale replacement ([[Panes.resetDoc]]) drops the index, the
  * same rule that drops the cache: fresh blocks may carry revisions that collide with
  * old records.
  *
  * The discipline is the wrap cache's own, inherited unchanged: a document that
  * *reorders* or deletes entries must bump revisions, because the index alone will not
  * notice.
  */
final case class RowIndex(width: Int, rows: Vector[DocPos], entries: Vector[RowIndex.Entry]) {

  /** How many wrapped rows the document has. */
  def length: Int = rows.length

  /** The document position of wrapped row `row`, or None past the end. */
  def apply(row: Int): Option[DocPos] = rows.lift(row)

  /** Which wrapped row `pos` starts, by binary search over the document-ordered rows.
    * None when `pos` is not itself a row start -- a position mid-row is not the top of
    * any viewport, which is the only thing this is asked.
    */
  def indexOf(pos: DocPos): Option[Int] = {
    val ord = summon[Ordering[DocPos]]
    var lo = 0
    var hi = rows.length - 1
    var found = -1
    while (lo <= hi && found < 0) {
      val mid = (lo + hi) >>> 1
      ord.compare(rows(mid), pos) match {
        case 0 => found = mid
        case n if n < 0 => lo = mid + 1
        case _ => hi = mid - 1
      }
    }
    if (found >= 0) { Some(found) }
    else { None }
  }
}

object RowIndex {

  /** One block's reconciled record: the revision whose wrap these starts describe, and
    * the start offset of every wrapped row within the block's logical text.
    */
  final case class Entry(rev: Long, starts: Vector[Int])

  /** A pane never laid out: nothing has been wrapped, so nothing is indexed. */
  val empty: RowIndex = RowIndex(0, Vector.empty, Vector.empty)

  /** `prev` brought up to date with `doc` at width `w`, wrapping through `cache`.
    *
    * `rowsOf` is the pane's own row source -- the same one the viewport reads, so a
    * `Truncate` diff contributes one row per logical line here exactly as it paints.
    * Unchanged revisions are reused as-is; changed, new and shrunk-away blocks are
    * re-wrapped (a shrink cannot be spliced, so it rebuilds); and a width that differs
    * from the one the records were made at rebuilds everything, because every row count
    * is wrong.
    */
  def reconciled(
      prev: RowIndex,
      paneId: PaneId,
      doc: Doc,
      width: Int,
      cache: WrapCache,
      rowsOf: (Int, WrapCache) -> (Vector[Row], WrapCache)
  ): (RowIndex, WrapCache) = {
    val w = math.max(1, width)
    if (prev.width == w && sameRevs(prev, doc)) { (prev, cache) }
    else if (prev.width != w || doc.length < prev.entries.length) {
      build(paneId, doc, w, cache, rowsOf)
    } else {
      // Some blocks changed or were appended; splice their records, reuse the rest.
      var c = cache
      val entries = Vector.newBuilder[Entry]
      val rows = Vector.newBuilder[DocPos]
      var e = 0
      while (e < doc.length) {
        val b = doc.entry(e).getOrElse(Block.Text(""))
        val starts: Vector[Int] = prev.entries.lift(e) match {
          case Some(rec) if rec.rev == b.rev => rec.starts
          case _ =>
            val (rs, c1) = rowsOf(e, c)
            c = c1
            rs.map(_.startOffset)
        }
        entries += Entry(b.rev, starts)
        var i = 0
        while (i < starts.length) {
          rows += DocPos(e, starts(i))
          i += 1
        }
        e += 1
      }
      (RowIndex(w, rows.result(), entries.result()), c)
    }
  }

  /** True when every block's revision matches the record at its index -- the fast path
    * that makes a scroll or an anchor change (which re-lay out but change no content)
    * cost compares only.
    */
  private def sameRevs(prev: RowIndex, doc: Doc): Boolean = {
    var e = 0
    var same = prev.entries.length == doc.length
    while (same && e < doc.length) {
      same = prev.entries(e).rev == doc.entry(e).getOrElse(Block.Text("")).rev
      e += 1
    }
    same
  }

  /** Everything wrapped from scratch -- a width change or a shrunken document. */
  private def build(
      paneId: PaneId,
      doc: Doc,
      w: Int,
      cache: WrapCache,
      rowsOf: (Int, WrapCache) -> (Vector[Row], WrapCache)
  ): (RowIndex, WrapCache) = {
    var c = cache
    val entries = Vector.newBuilder[Entry]
    val rows = Vector.newBuilder[DocPos]
    var e = 0
    while (e < doc.length) {
      val b = doc.entry(e).getOrElse(Block.Text(""))
      val (rs, c1) = rowsOf(e, c)
      c = c1
      entries += Entry(b.rev, rs.map(_.startOffset))
      var i = 0
      while (i < rs.length) {
        rows += DocPos(e, rs(i).startOffset)
        i += 1
      }
      e += 1
    }
    (RowIndex(w, rows.result(), entries.result()), c)
  }
}
