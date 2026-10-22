package grit.formats.choice

import grit.core.files.FileRange

import utest.*

/** [[Defaults]]: which formats read which kinds, and what the rest are told. */
object DefaultsTests extends TestSuite {

  private val W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

  val tests = Tests {
    test("a Word file is read whole from its start") {
      val docx = Fixtures.zip(
        "a.docx",
        "word/document.xml" ->
          s"""<w:document xmlns:w="$W"><w:body><w:p><w:r><w:t>Hello.</w:t></w:r></w:p></w:body></w:document>"""
      )
      Fixtures.readAll(Defaults.formats, docx).map(r => (r.text, r.covered)) ==> Right(
        ("Hello.", "all of it")
      )
    }

    test("a PDF is read by its pages") {
      val pdf = grit.formats.pdf.Pdfs.file(
        "p.pdf",
        Vector("One.", "Two.", "Three.").map(t => grit.formats.pdf.Pdfs.lines(t))
      )
      Fixtures
        .readAll(Defaults.formats, pdf, FileRange.pages(2, 3).getOrElse(FileRange.Start))
        .map(r => (r.text, r.covered)) ==> Right(
        ("[page 2]\nTwo.\n\n[page 3]\nThree.", "pages 2–3 of 3")
      )
    }

    test("an .xls is refused in words saying what could be read instead") {
      val xls = Fixtures.file(
        "old.xls",
        Vector(0xd0, 0xcf, 0x11, 0xe0, 0xa1, 0xb1, 0x1a, 0xe1, 0, 0).map(_.toByte)
      )
      Fixtures.readAll(Defaults.formats, xls, FileRange.Start).left.map(_.message) ==> Left(
        "That file is an older Office file (.doc, .xls or .ppt), or one locked with a password, " +
          "so it can't be read here. A .docx or .xlsx copy, saved without a password, could be."
      )
    }
  }
}
