package grit.core.files

import java.nio.charset.StandardCharsets.UTF_8

import utest.*

/** [[Kind.sniff]]: a file is what its bytes say. */
object KindTests extends TestSuite {

  private def bytes(s: String): Vector[Byte] = s.getBytes(UTF_8).toVector
  private def magic(b: Int*): Vector[Byte] = b.map(_.toByte).toVector

  private val pdf: Vector[Byte] = bytes("%PDF-1.7\n%âãÏÓ\n1 0 obj")
  private val ole2: Vector[Byte] = magic(0xd0, 0xcf, 0x11, 0xe0, 0xa1, 0xb1, 0x1a, 0xe1, 0, 0)
  private val zip: Vector[Byte] = magic(0x50, 0x4b, 0x03, 0x04, 0x14, 0)
  private val docx: Vector[String] =
    Vector("[Content_Types].xml", "word/document.xml", "word/styles.xml")
  private val xlsx: Vector[String] =
    Vector("[Content_Types].xml", "xl/workbook.xml", "xl/worksheets/sheet1.xml")
  private val Docx = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  private val Xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  private val SlidesDeck = "application/vnd.google-apps.presentation"

  val tests = Tests {
    test("a file is known by its bytes, never by the type its sender claims") {
      Vector(
        Kind.sniff(pdf, Vector.empty, Some(Docx)),
        Kind.sniff(zip, docx, Some("application/pdf")),
        Kind.sniff(zip, xlsx, Some(Docx)),
        Kind.sniff(ole2, Vector.empty, Some(Xlsx)),
        Kind.sniff(zip, Vector("xl/workbook.bin"), Some(Xlsx)),
        Kind.sniff(zip, Vector("mimetype", "content.xml"), Some(Docx)),
        Kind.sniff(magic(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a), Vector.empty, Some("text/plain"))
      ) ==> Vector(
        Kind.Pdf,
        Kind.Word,
        Kind.Excel,
        Kind.OldOffice,
        Kind.ExcelBinary,
        Kind.OtherZip,
        Kind.Image
      )
    }

    test("a zip's entries are read only when its bytes are a zip's") {
      var asked = 0
      def entries: Vector[String] = { asked += 1; docx }
      val kinds = Vector(Kind.sniff(pdf, entries, None), Kind.sniff(zip, entries, None))
      (kinds, asked) ==> (Vector(Kind.Pdf, Kind.Word), 1)
    }

    test("text is UTF-8 with no NUL, a character the head cut short allowed") {
      val accented = bytes("café crème")
      Vector(
        Kind.sniff(accented, Vector.empty, None),
        Kind.sniff(accented.dropRight(1), Vector.empty, None),
        Kind.sniff(bytes("a\u0000b"), Vector.empty, Some("text/plain")),
        Kind.sniff(magic('c', 'a', 'f', 0xe9, ' ', 'x'), Vector.empty, Some("text/plain"))
      ) ==> Vector(Kind.Text, Kind.Text, Kind.Unknown, Kind.Unknown)
    }

    test("the claim decides only an empty file and a zip holding both a document and a workbook") {
      Vector(
        Kind.sniff(Vector.empty, Vector.empty, Some("text/csv")),
        Kind.sniff(Vector.empty, Vector.empty, Some(Docx)),
        Kind.sniff(zip, docx ++ xlsx, Some(Xlsx)),
        Kind.sniff(zip, docx ++ xlsx, Some(Docx))
      ) ==> Vector(Kind.Text, Kind.Unknown, Kind.Excel, Kind.Word)
    }

    test("a JSON object is a Slides deck only when it is claimed as one") {
      val deck = bytes("""  {"presentationId": "p1", "slides": []}""")
      Vector(
        Kind.sniff(deck, Vector.empty, Some(SlidesDeck)),
        Kind.sniff(deck, Vector.empty, None),
        Kind.sniff(deck, Vector.empty, Some("application/json")),
        Kind.sniff(bytes("""["p1"]"""), Vector.empty, Some(SlidesDeck)),
        Kind.sniff(bytes("Roadmap {draft}"), Vector.empty, Some(SlidesDeck))
      ) ==> Vector(Kind.GoogleSlides, Kind.Text, Kind.Text, Kind.Text, Kind.Text)
    }
  }
}
