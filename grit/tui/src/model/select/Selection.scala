package grit.tui.model.select

import grit.tui.model.block.Block
import grit.tui.model.surface.PaneId
import grit.tui.model.text.Width

/** A position in a pane's logical, *unwrapped* document: which block, and the char
  * offset into its logical text.
  *
  * Indexing unwrapped text is what makes a multi-screen drag possible: scrolling moves
  * the viewport's anchor and never the selection, and copying is a slice of what the
  * author wrote rather than of what an 80-column viewport broke it to. Nothing here
  * knows the width.
  */
final case class DocPos(entry: Int, offset: Int)

object DocPos {

  /** Document order: by entry, then by offset. */
  given Ordering[DocPos] = Ordering.by(p => (p.entry, p.offset))

  val zero: DocPos = DocPos(0, 0)
}

/** A pane's document: its blocks as logical, unwrapped text.
  *
  * A block is a group of rows that measures itself (see `grit.tui.model.block.Block`); a plain
  * string of text is the `Block.Text` case. The block -- not the line -- is the unit
  * `DocPos.entry` indexes, so a block that grows rows moves no sibling position.
  *
  * A pane owns one of these, and every position a drag produces is clamped into it --
  * which is how a selection is stopped from escaping a modal into what is behind it.
  */
final case class Doc(blocks: Vector[Block]) {

  /** The block at `i`, or None outside the document. */
  def entry(i: Int): Option[Block] = if (i >= 0 && i < blocks.length) Some(blocks(i)) else None

  /** The logical text of the block at `i`, empty outside the document. */
  def textAt(i: Int): String = entry(i).map(_.text).getOrElse("")

  /** How many blocks. */
  def length: Int = blocks.length

  def isEmpty: Boolean = blocks.isEmpty

  /** The last position in the document -- end of the last entry, or the origin if there
    * are no entries.
    */
  def end: DocPos =
    if (blocks.isEmpty) DocPos.zero else DocPos(blocks.length - 1, blocks.last.text.length)

  /** `p` moved to the nearest position that actually exists in this document. */
  def clamp(p: DocPos): DocPos =
    if (blocks.isEmpty) DocPos.zero
    else {
      val e = math.max(0, math.min(p.entry, blocks.length - 1))
      DocPos(e, math.max(0, math.min(p.offset, blocks(e).text.length)))
    }

  /** The text `sel` covers, entries joined by newlines, with the first and last trimmed
    * to the selection's bounds. Empty for an empty selection.
    *
    * No wrapping happens here at all: the result is a slice of the logical text, which
    * is the whole point of [[DocPos]].
    */
  def textOf(sel: Selection): String = {
    val from = clamp(sel.start)
    val to = clamp(sel.end)
    if (blocks.isEmpty || from == to) { "" }
    else if (from.entry == to.entry) {
      blocks(from.entry).text.slice(from.offset, to.offset)
    } else {
      val parts = Vector.newBuilder[String]
      parts += blocks(from.entry).text.drop(from.offset)
      var e = from.entry + 1
      while (e < to.entry) { parts += blocks(e).text; e += 1 }
      parts += blocks(to.entry).text.take(to.offset)
      parts.result().mkString("\n")
    }
  }

  /** This document with `b` added at the end. The streaming path's builder. */
  def append(b: Block): Doc = copy(blocks = blocks :+ b)

  /** This document with the block at `i` replaced. The streaming tail's operation: a
    * tick that grows the last block replaces it and bumps that block's revision alone.
    * Out of range it is this document -- there is nothing there to replace.
    */
  def updated(i: Int, b: Block): Doc =
    if (i >= 0 && i < blocks.length) { copy(blocks = blocks.updated(i, b)) }
    else { this }

  /** The empty document. The wholesale-replacement operation: a `/clear` -- not an
    * edit, so the pane's wrap cache and row index go with it (see `Panes.resetDoc`).
    */
  def clear: Doc = Doc.empty
}

object Doc {

  /** A document from plain strings, every block a `Block.Text` at revision zero. */
  def of(lines: String*): Doc = Doc(lines.toVector.map(t => Block.Text(t)))

  val empty: Doc = Doc(Vector.empty)
}

/** A half-open range of document positions, `start` <= `end`.
  *
  * Build one with [[Selection.between]] unless the bounds are already ordered.
  */
final case class Selection(start: DocPos, end: DocPos) {

  def isEmpty: Boolean = start == end

  /** True when `p` is inside the selection -- `start` included, `end` excluded. */
  def contains(p: DocPos): Boolean = {
    val ord = summon[Ordering[DocPos]]
    ord.lteq(start, p) && ord.lt(p, end)
  }

  /** The half-open column range of this selection on one wrapped row, given the entry
    * the row belongs to, the row's offset into that entry's logical text, and its text.
    *
    * **Both bounds go through the same projection.** Treating them asymmetrically -- a
    * bound before the row mapping to 0 for `start` but to the row's length for `end` --
    * paints every row *after* the selection while leaving the copied text correct
    * (FINDINGS bug 3). With one projection, `from <= to` follows from `start <= end`
    * for free, and a row outside the selection collapses to an empty range at whichever
    * edge it sits on.
    *
    * Columns, not char offsets: a wide glyph before the selection pushes it two cells.
    */
  def columnsOn(entry: Int, rowStart: Int, rowText: String): (Int, Int) =
    (project(start, entry, rowStart, rowText), project(end, entry, rowStart, rowText))

  private def project(b: DocPos, entry: Int, rowStart: Int, rowText: String): Int =
    if (b.entry < entry) { 0 }
    else if (b.entry > entry) { Width.of(rowText) }
    else {
      val off = math.max(0, math.min(b.offset - rowStart, rowText.length))
      Width.columnAtOffset(rowText, off)
    }
}

object Selection {

  /** The selection between two positions, in whichever order the drag ran. */
  def between(a: DocPos, b: DocPos): Selection = {
    val ord = summon[Ordering[DocPos]]
    if (ord.lteq(a, b)) { Selection(a, b) }
    else { Selection(b, a) }
  }

  /** Nothing selected. */
  val empty: Selection = Selection(DocPos.zero, DocPos.zero)
}

/** A drag in progress, bound for its whole life to the pane the button went down in.
  *
  * `anchor` is where the press landed and never moves; `head` follows the pointer. That
  * split is what lets autoscroll pull in screen after screen -- the tick moves the head
  * to the new edge row while the anchor stays wherever it was, however many screens ago.
  *
  * The pane clamps: [[extend]] takes the pane's own [[Doc]], so no motion outside the
  * pane can drag the selection past the end of that pane's document or into whatever is
  * painted behind it.
  */
final case class Drag(pane: PaneId, anchor: DocPos, head: DocPos) {

  /** This drag with its head moved to `to`, clamped into `doc`. The anchor is untouched. */
  def extend(to: DocPos, doc: Doc): Drag = copy(head = doc.clamp(to))

  /** What is selected right now, in document order. */
  def selection: Selection = Selection.between(anchor, head)
}

object Drag {

  /** A drag that has just begun: anchor and head both at the press. */
  def start(pane: PaneId, at: DocPos, doc: Doc): Drag = {
    val p = doc.clamp(at)
    Drag(pane, p, p)
  }
}
