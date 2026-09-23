package grit.tui.components.pane

import grit.tui.model.block.Block
import grit.tui.model.select.{Doc, DocPos}
import grit.tui.model.surface.{PaneId, Size}
import grit.tui.model.text.WrapCache
import utest.*

/** The document row index the scrollbar speaks, and the funnel that maintains it.
  *
  * The index is one `DocPos` per wrapped row of the *whole* document -- the thing the
  * viewport deliberately never computes -- so the property under test is that it stays
  * in step with the document through the same operations that keep the wrap cache in
  * step, re-wrapping only what changed.
  */
object RowIndexTests extends TestSuite {

  private val back = PaneId.of("transcript")

  private val size = Size(10, 20)

  private def transcript: TextPane =
    TextPane(back, Doc.of("alpha", "bravo", "charlie", "delta"), cache = WrapCache.empty(20))

  private def misses(p: Panes): Long = p.get(back).get.cache.misses

  val tests = Tests {

    test("an appended block re-wraps alone and the index grows by its rows") {
      val laid = Panes.of(transcript).layout(back, size)
      val before = laid.rowIndex(back)
      val beforeMisses = misses(laid)

      val next = laid.withDoc(back, Doc.of("alpha", "bravo", "charlie", "delta", "echo"))

      assert(misses(next) - beforeMisses == 1L) // one block wrapped: the new one
      assert(next.rowIndex(back).length == before.length + 1)
      assert(next.rowIndex(back)(before.length) == Some(DocPos(4, 0)))
      // And the unchanged prefix is bit-identical -- the recorded block-stability rule.
      assert(
        next.rowIndex(back).rows.take(before.length) == before.rows
      )
    }

    test("a stream tick on the tail re-wraps one entry; sibling records are untouched") {
      // The shape Transcript.stream has: the trailing block is *replaced* (text grows,
      // its revision bumps), never appended -- and grows far enough to wrap onto more
      // rows, so a reconcile that ignored the revision would keep the old row count.
      val laid = Panes
        .of(
          TextPane(
            back,
            Doc(Vector(Block.Text("alpha"), Block.Text("grit> tail", 1L))),
            cache = WrapCache.empty(20)
          )
        )
        .layout(back, size)
      val before = laid.rowIndex(back)
      val beforeMisses = misses(laid)

      val next = laid.withDoc(
        back,
        Doc(
          Vector(
            Block.Text("alpha"),
            Block.Text(
              "grit> tail and a much longer line that certainly wraps onto several rows now",
              2L
            )
          )
        )
      )

      assert(misses(next) - beforeMisses == 1L)
      assert(next.rowIndex(back).rows.head == before.rows.head) // entry 0 bit-identical
      assert(next.rowIndex(back).length > before.length) // the grown tail wraps to more rows
    }

    test("an interior expansion re-wraps one entry and moves no sibling position") {
      // The recorded DocPos rule, at the index level: expansion rewrites rows *inside*
      // one block, so every sibling position survives bit-identically.
      val doc = Doc(
        Vector(
          Block.Text("alpha"),
          Block.tool("Read", "x").finish(true, "ok", Vector("r1", "r2")),
          Block.Text("omega")
        )
      )
      val laid = Panes.of(TextPane(back, doc, cache = WrapCache.empty(20))).layout(back, size)
      val before = laid.rowIndex(back)
      val beforeMisses = misses(laid)

      val t = doc.entry(1).get.asInstanceOf[Block.Tool]
      val next = laid.withDoc(back, doc.updated(1, t.withExpanded(true)))

      assert(misses(next) - beforeMisses == 1L)
      val after = next.rowIndex(back)
      assert(after.length == before.length + 2) // two more wrapped rows, same block
      assert(after.rows.filter(_.entry != 1) == before.rows.filter(_.entry != 1))
      assert(after.indexOf(DocPos(2, 0)).isDefined) // the omega entry is findable
    }

    test("a wholesale replacement drops the index; a stale revision cannot land in it") {
      // The /clear shape, at the index level: entry indices restart at zero, so a fresh
      // block carrying a revision the old document already used would reconcile against
      // the wrong record. resetDoc drops the records with the document -- the replacement
      // below has the same entry count and the same revision as the original, and only
      // the dropped records stop the old wrapping from answering for the new text.
      val laid =
        Panes
          .of(
            TextPane(
              back,
              Doc(Vector(Block.Text("alpha alpha alpha", 1L))),
              cache = WrapCache.empty(8)
            )
          )
          .layout(back, Size(10, 8))
      assert(laid.rowIndex(back).length == 3) // wraps at 8 columns

      val cleared = laid.resetDoc(back, Doc(Vector(Block.Text("x", 1L))))
      assert(cleared.rowIndex(back).length == 1)
      assert(cleared.rowIndex(back)(0) == Some(DocPos(0, 0)))
      // The painted viewport and the index agree.
      assert(cleared.rendered(back).rows.map(_.start) == Vector(0))

      val emptied = laid.clear(back)
      assert(emptied.rowIndex(back).length == 0)
      assert(emptied.rendered(back).rows.isEmpty)
    }

    test("a width change rebuilds the index, because every row count is wrong") {
      val wrapped = Doc.of("one two three four five six seven")
      val laid = Panes.of(TextPane(back, wrapped, cache = WrapCache.empty(20))).layout(back, size)
      val wide = laid.rowIndex(back)

      val narrow = laid.layout(back, Size(10, 8))

      assert(narrow.rowIndex(back).length > wide.length)
      assert(narrow.rowIndex(back)(0) == Some(DocPos(0, 0)))
      // And the index is bound to the width it was rebuilt at.
      assert(narrow.rowIndex(back).width == 8)
    }

    test("scrolls and anchors leave the index alone") {
      val laid = Panes.of(transcript).layout(back, size)
      val before = laid.rowIndex(back)
      assert(laid.scrollBy(back, -1).rowIndex(back) == before)
      assert(laid.withAnchor(back, Anchor.At(DocPos(1, 0))).rowIndex(back) == before)
    }

    test("the index and the viewport answer each other") {
      // The scrollbar's offset read: the viewport's top is always a row of the index,
      // and its position is the row the pane is reading from.
      val laid = Panes.of(transcript).layout(back, size)
      val scrolled = laid.scrollBy(back, -2)
      val top = scrolled.rendered(back).top
      assert(top.isDefined)
      val row = scrolled.rowIndex(back).indexOf(top.get)
      assert(row.isDefined)
      assert(scrolled.rowIndex(back)(row.get) == top)
    }

    test("indexOf is exact: a mid-row position is not the top of any viewport") {
      val wrapped = Doc.of("one two three four five six seven")
      val laid = Panes.of(TextPane(back, wrapped, cache = WrapCache.empty(20))).layout(back, size)
      val index = laid.rowIndex(back)
      val first = index(0).get
      assert(index.indexOf(first) == Some(0))
      val secondEntryStart = index.rows.find(_.entry == 1)
      secondEntryStart.foreach(p => assert(index.indexOf(p).isDefined))
      assert(index.indexOf(DocPos(0, 1)).isEmpty) // offset 1 is inside row 0, not a row
    }

    test("an empty document indexes to nothing") {
      val laid = Panes.of(transcript).layout(back, size)
      val empty = laid.clear(back)
      val index = empty.rowIndex(back)
      assert(index.length == 0)
      assert(index(0).isEmpty)
      assert(index.indexOf(DocPos.zero).isEmpty)
    }

    test("a pane never laid out reads an empty index") {
      assert(Panes.of(transcript).rowIndex(back).length == 0)
    }
  }
}
