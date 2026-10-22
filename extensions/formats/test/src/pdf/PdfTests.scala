package grit.formats.pdf

import java.nio.file.Files

import grit.core.files.{CoverUnit, Covers, Kind, Limit, Locus, ReadLimits, Unread}
import grit.formats.choice.Fixtures
import grit.formats.pdf.Pdfs.{Line, Lock, Mark, lines}
import grit.prose.form.{Block, Level, Span, Text}

import utest.*

/** [[Pdf]] over PDFs written here: text in stream order, bookmarks as headings, each span
  * noted with its page, and the files it cannot read told apart.
  */
object PdfTests extends TestSuite {

  private def on(page: Int, s: String): Text[Locus] = Text(
    Vector(Span(s, Set.empty, Vector(Locus.Page(page))))
  )
  private def para(page: Int, s: String): Block[Locus] = Block.Paragraph(on(page, s))
  private def heading(level: Int, page: Int, s: String): Block[Locus] =
    Block.Heading(Level.of(level), on(page, s))

  val tests = Tests {
    test(
      "bookmarks are headings where their titles are printed, a section number its bookmark lacks kept"
    ) {
      val pdf = Pdfs.file(
        "b.pdf",
        Vector(
          lines("1 Introduction") ++ Vector(Line("This report covers the audit.", 680f)),
          lines("1.1 Scope") ++ Vector(Line("Every system is in scope.", 680f))
        ),
        Vector(Mark("Introduction", 1), Mark("1.1 Scope", 2, level = 2))
      )
      Fixtures.read(Pdf, pdf).map(u => (u.doc.blocks, u.covers)) ==> Right(
        (
          Vector(
            heading(1, 1, "1 Introduction"),
            para(1, "This report covers the audit."),
            heading(2, 2, "1.1 Scope"),
            para(2, "Every system is in scope.")
          ),
          Covers.Whole
        )
      )
    }

    test("a header and a footer printed on most pages are dropped") {
      // Each page's body in words of its own: a line repeated on most pages is what is dropped.
      val bodies = Vector(
        "Alpha covers access.",
        "Bravo covers audit.",
        "Charlie covers backups.",
        "Delta covers keys."
      )
      val pages = bodies.zipWithIndex.map((body, i) =>
        Vector(
          Line("Example Corp Quarterly Report", 760f),
          Line(body, 600f),
          Line(s"Page ${i + 1}", 40f)
        )
      )
      Fixtures.read(Pdf, Pdfs.file("h.pdf", pages)).map(_.doc.blocks) ==>
        Right(bodies.zipWithIndex.map((body, i) => para(i + 1, body)))
    }

    test("a paragraph running onto the next page keeps both pages") {
      val pdf = Pdfs.file(
        "c.pdf",
        Vector(
          lines("The policy applies to every account and it"),
          lines("continues on the next page.")
        )
      )
      Fixtures.read(Pdf, pdf).map(_.doc.blocks) ==> Right(
        Vector(
          Block.Paragraph(
            Text(
              Vector(
                Span(
                  "The policy applies to every account and it ",
                  Set.empty,
                  Vector(Locus.Page(1))
                ),
                Span("continues on the next page.", Set.empty, Vector(Locus.Page(2)))
              )
            )
          )
        )
      )
    }

    test("a run of short lines, as a table's rows, keeps its line breaks") {
      val pdf = Pdfs.file("t.pdf", Vector(lines("Name Role", "Ana Lead", "Bo Dev")))
      Fixtures.read(Pdf, pdf).map(_.doc.blocks) ==> Right(
        Vector(para(1, "Name Role\nAna Lead\nBo Dev"))
      )
    }

    test("a page drawing more glyphs than read stops, keeping the pages before it") {
      val bomb = (0 until 60).toVector.map(i => Line("W" * 200, 760f - 12f * i, 6f))
      val pdf = Pdfs.file("g.pdf", Vector(lines("First page text."), bomb))
      val started = System.nanoTime()
      val read = Fixtures.read(Pdf, pdf, ReadLimits.lowered(glyphs = 5000))
      val seconds = (System.nanoTime() - started) / 1e9
      (read.map(u => (u.doc.blocks, u.covers)), seconds < 2.5) ==> (
        Right(
          (Vector(para(1, "First page text.")), Covers.Part(1, 2, CoverUnit.Pages, Limit.Glyphs))
        ),
        true
      )
    }

    test("a PDF locked to open is Locked; one only marked not to be copied is read") {
      val locked = Pdfs.file("l.pdf", Vector(lines("Secret.")), lock = Some(Lock.Open))
      val marked = Pdfs.file("m.pdf", Vector(lines("Shared.")), lock = Some(Lock.NoCopy))
      (Fixtures.read(Pdf, locked), Fixtures.read(Pdf, marked).map(_.doc.blocks)) ==>
        (Left(Unread.Locked), Right(Vector(para(1, "Shared."))))
    }

    test("pages with no text are a scan; a cut-short file is damaged; another file is not a PDF") {
      val scan = Pdfs.file("s.pdf", Vector(Vector.empty, Vector.empty), drawn = true)
      val whole = Files.readAllBytes(Pdfs.file("w.pdf", Vector(lines("Text."))))
      val cut = Fixtures.file("cut.pdf", whole.take(60).toVector)
      (
        Fixtures.read(Pdf, scan),
        Fixtures.read(Pdf, cut),
        Fixtures.read(Pdf, Fixtures.zip("a.docx", "word/document.xml" -> "<w/>"))
      ) ==> (Left(Unread.Scanned(2)), Left(Unread.Damaged(Kind.Pdf)), Left(Unread.NotMine))
    }
  }
}
