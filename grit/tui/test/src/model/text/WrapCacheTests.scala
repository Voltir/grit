package grit.tui.model.text

import utest.*

object WrapCacheTests extends TestSuite {

  private val id = ContentId("entry-1")
  private val other = ContentId("entry-2")

  val tests = Tests {

    test("a repeat lookup at the same (id, rev) is a hit; a bumped revision re-wraps") {
      val (rows1, c1) = WrapCache.empty(8).rowsFor(id, 0, "one two three")
      val (rows2, c2) = c1.rowsFor(id, 0, "one two three")
      assert(rows1 == rows2)
      assert(c2.hits == 1)
      assert(c2.misses == 1)
      // Streaming contract: a bump invalidates exactly the entry that changed.
      val (rows3, c3) = c2.rowsFor(id, 1, "one two three four")
      assert(rows3.map(_.text) == Vector("one two", "three", "four"))
      assert(c3.misses == 2)
    }

    test("a width change discards every entry but keeps the counters") {
      val (_, c1) = WrapCache.empty(20).rowsFor(id, 0, "hello world")
      val (_, c2) = WrapCache.empty(20).rowsFor(other, 0, "a b c")
      val c3 = c1.atWidth(10)
      assert(c3.hits == 0)
      assert(c3.misses == 1)
      val c4 = c2.atWidth(10)
      assert(c4.misses == 1)
      // The rows served after the change are wrapped at the new width, not the old.
      val (rows, _) = c3.atWidth(7).rowsFor(id, 0, "one two three")
      assert(rows.map(_.text) == Vector("one two", "three"))
    }
  }
}
