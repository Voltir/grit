package grit.core.files

import utest.*

/** [[FileRange.parse]]: a reader's arguments as the range they ask for. */
object FileRangeTests extends TestSuite {

  val tests = Tests {
    test("pages are one page or a run of them, by hyphen or en dash, counted from 1") {
      Vector(
        FileRange.parse(Some("212-214"), None, None, None),
        FileRange.parse(Some(" 212 – 214 "), None, None, None),
        FileRange.parse(Some("7"), None, None, None)
      ) ==> Vector(FileRange.pages(212, 214), FileRange.pages(212, 214), FileRange.pages(7, 7))
        .map(_.toRight(""))
    }

    test("a run ending before it starts, a page 0, or two ranges at once are refused in words") {
      Vector(
        FileRange.parse(Some("214-212"), None, None, None),
        FileRange.parse(Some("0"), None, None, None),
        FileRange.parse(Some("1"), Some("Intro"), None, None)
      ) ==> Vector(
        Left("Give pages as one page, \"212\", or a run of them, \"212-214\", not \"214-212\"."),
        Left("Give pages as one page, \"212\", or a run of them, \"212-214\", not \"0\"."),
        Left("Ask for pages, a section, a starting offset or near: one of them, not several.")
      )
    }

    test("no argument reads from the start") {
      FileRange.parse(None, None, None, None) ==> Right(FileRange.Start)
    }

    test("near reads back the anchor it writes; near not so written is refused in words") {
      val anchor = Anchor("All accounts are reviewed", "Policy | text. ", " by the owner.", 0.25)
      (
        anchor.written,
        FileRange.parse(None, None, None, Some(anchor.written)),
        FileRange.parse(None, None, None, Some("All accounts are reviewed"))
      ) ==> (
        "0.2500|Policy | text. ⟪All accounts are reviewed⟫ by the owner.",
        Right(FileRange.Near(anchor)),
        Left("Give near as a piece's pointer gives it, copied whole.")
      )
    }
  }
}
