package grit.core.id

import utest.*

/** [[FileHandle]]: written `f<n>`, as the model sees it, and read back from that alone. */
object FileHandleTests extends TestSuite {

  val tests = Tests {
    test("a handle is written f and its number, and reads back") {
      FileHandle.of(41).map(h => (h.written, FileHandle.read(h.written))) ==>
        Some(("f41", Some(h41)))
    }
    test("text that is not f and a number from 1 is no handle") {
      Vector("41", "F41", "f", "f0", "f-1", "f4x", "f+4", " f41").map(FileHandle.read) ==>
        Vector.fill(8)(None)
    }
  }

  private lazy val h41 = FileHandle.of(41).getOrElse(throw new java.lang.AssertionError("41"))
}
