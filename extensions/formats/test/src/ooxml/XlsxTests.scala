package grit.formats.ooxml

import java.time.{LocalDate, LocalDateTime}

import grit.core.files.{CoverUnit, Covers, Limit, Missed, ReadLimits, Unread}
import grit.formats.choice.Fixtures
import grit.prose.form.{Block, Cell, Column, Level, Text}

import utest.*

/** [[Xlsx]] over workbooks written here, part by part. */
object XlsxTests extends TestSuite {

  private val S = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
  private val R = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"

  private def workbook(
      sheets: Vector[(String, String)],
      pr: String = ""
  ): Vector[(String, String)] = {
    val listed = sheets.zipWithIndex.map { case ((name, _), i) =>
      val hidden = if (name.startsWith("hidden:")) """ state="hidden"""" else ""
      s"""<sheet name="${name.stripPrefix(
          "hidden:"
        )}" sheetId="${i + 1}"$hidden r:id="rId${i + 1}"/>"""
    }
    val rels = sheets.indices.map(i =>
      s"""<Relationship Id="rId${i + 1}" Type="worksheet" Target="worksheets/sheet${i + 1}.xml"/>"""
    )
    Vector(
      "xl/workbook.xml" ->
        s"""<workbook xmlns="$S" xmlns:r="$R">$pr<sheets>${listed.mkString}</sheets></workbook>""",
      "xl/_rels/workbook.xml.rels" ->
        s"""<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">${rels.mkString}</Relationships>""",
      "xl/styles.xml" ->
        s"""<styleSheet xmlns="$S"><cellXfs count="4"><xf numFmtId="0"/><xf numFmtId="14"/><xf numFmtId="10"/><xf numFmtId="22"/></cellXfs></styleSheet>"""
    ) ++ sheets.zipWithIndex.map { case ((_, rows), i) =>
      s"xl/worksheets/sheet${i + 1}.xml" -> s"""<worksheet xmlns="$S"><sheetData>$rows</sheetData></worksheet>"""
    }
  }

  private def str(ref: String, s: String): String =
    s"""<c r="$ref" t="inlineStr"><is><t>$s</t></is></c>"""
  private def num(ref: String, v: String, style: Int = 0): String =
    s"""<c r="$ref" s="$style"><v>$v</v></c>"""

  private def w(s: String): Cell[Nothing] = Cell.Words(Text.plain(s))
  private def n(v: String, f: Option[String] = None): Cell[Nothing] =
    Cell.Number(BigDecimal(v), Text.plain(v), f)
  private def col(s: String): Column[Nothing] = Column(Text.plain(s))

  val tests = Tests {
    test(
      "a sheet is a heading and a table: shared strings, numbers, a 1904 date, a truth and formulas"
    ) {
      val shared =
        s"""<sst xmlns="$S"><si><t>Item</t></si><si><t>Count</t></si><si><t>Double</t></si><si><t>Due</t></si><si><t>Pens</t></si></sst>"""
      val rows =
        """<row r="1"><c r="A1" t="s"><v>0</v></c><c r="B1" t="s"><v>1</v></c><c r="C1" t="s"><v>2</v></c><c r="D1" t="s"><v>3</v></c></row>""" +
          """<row r="2"><c r="A2" t="s"><v>4</v></c>""" + num("B2", "1") +
          """<c r="C2"><f t="shared" ref="C2:C3" si="0">B2*2</f><v>2</v></c>""" + num(
            "D2",
            "0",
            1
          ) + "</row>" +
          "<row r=\"3\">" + str("A3", "Ink") + num("B3", "2") +
          """<c r="C3"><f t="shared" si="0"/><v>4</v></c><c r="D3" t="b"><v>1</v></c>""" +
          """<c r="E3"><f>B3+1</f></c></row>"""
      val book = Fixtures.zipOf(
        "b.xlsx",
        workbook(
          Vector("Q3" -> rows),
          """<workbookPr date1904="1"/>"""
        ) :+ ("xl/sharedStrings.xml" -> shared)
      )
      Fixtures.blocks(Fixtures.read(Xlsx, book)) ==> Right(
        Vector(
          Block.Heading(Level.of(2), Text.plain("Q3")),
          Block.table(
            Vector(col("Item"), col("Count"), col("Double"), col("Due"), Column(Text.empty)),
            Vector(
              Vector(
                w("Pens"),
                n("1"),
                n("2", Some("=B2*2, filled to C2:C3")),
                Cell.Date(LocalDate.of(1904, 1, 1), Text.plain("1904-01-01")),
                Cell.Empty()
              ),
              Vector(w("Ink"), n("2"), n("4"), Cell.Truth(true), Cell.Empty(Some("=B3+1")))
            )
          )
        )
      )
    }

    test("a date in the 1900 system, with its time when it has one, and a percentage as shown") {
      val rows =
        "<row r=\"1\">" + str("A1", "When") + str("B1", "At") + str("C1", "Share") + "</row>" +
          "<row r=\"2\">" + num("A2", "45000", 1) + num("B2", "45000.5", 3) + num(
            "C2",
            "0.125",
            2
          ) + "</row>"
      Fixtures.blocks(
        Fixtures.read(Xlsx, Fixtures.zipOf("d.xlsx", workbook(Vector("D" -> rows))))
      ) ==> Right(
        Vector(
          Block.Heading(Level.of(2), Text.plain("D")),
          Block.table(
            Vector(col("When"), col("At"), col("Share")),
            Vector(
              Vector(
                Cell.Date(LocalDate.of(2023, 3, 15), Text.plain("2023-03-15")),
                Cell.Date(LocalDateTime.of(2023, 3, 15, 12, 0), Text.plain("2023-03-15 12:00")),
                Cell.Number(BigDecimal("0.125"), Text.plain("12.5%"))
              )
            )
          )
        )
      )
    }

    test("a hidden sheet, drawings and cell comments are said to be missed, not read") {
      val book = Fixtures.zipOf(
        "h.xlsx",
        workbook(
          Vector("Shown" -> ("<row r=\"1\">" + str("A1", "a") + "</row>"), "hidden:Secret" -> "")
        ) ++
          Vector("xl/drawings/drawing1.xml" -> "<wsDr/>", "xl/comments1.xml" -> "<comments/>")
      )
      val read = Fixtures.read(Xlsx, book)
      (read.map(_.missed), read.map(_.doc.blocks.size)) ==> (
        Right(
          Vector(
            Missed("the hidden sheet 'Secret'"),
            Missed("cell comments"),
            Missed("drawings and text boxes")
          )
        ),
        Right(2)
      )
    }

    test("a cell past the characters read stops the sheet there, keeping the rows before it") {
      val rows = "<row r=\"1\">" + str("A1", "head") + "</row><row r=\"2\">" + str("A2", "first") +
        "</row><row r=\"3\">" + str("A3", "z" * 300000) + "</row>"
      val read = Fixtures.read(
        Xlsx,
        Fixtures.zipOf("big.xlsx", workbook(Vector("Big" -> rows, "Next" -> ""))),
        ReadLimits.lowered(chars = 50000)
      )
      (read.map(_.covers), Fixtures.blocks(read)) ==> (
        Right(Covers.Part(1, 2, CoverUnit.Sheets, Limit.Chars)),
        Right(
          Vector(
            Block.Heading(Level.of(2), Text.plain("Big")),
            Block.table(Vector(col("head")), Vector(Vector(w("first"))))
          )
        )
      )
    }

    test("a workbook damaged before its first row is damaged, not a read of no rows") {
      val sheets = workbook(Vector("Broken" -> "", "Next" -> ""))
        .map((name, xml) =>
          if (name == "xl/worksheets/sheet1.xml")
            (name, s"""<worksheet xmlns="$S"><sheetData><row""")
          else (name, xml)
        )
      Fixtures.read(Xlsx, Fixtures.zipOf("broken.xlsx", sheets)) ==>
        Left(Unread.Damaged(grit.core.files.Kind.Excel))
    }

    test("a zip holding no workbook is not an Excel file") {
      Fixtures.read(Xlsx, Fixtures.zip("w.docx", "word/document.xml" -> "<document/>")) ==>
        Left(Unread.NotMine)
    }
  }
}
