package grit.formats.ooxml

import grit.core.files.{Covers, Kind, Limit, Missed, ReadLimits, Unread}
import grit.formats.choice.Fixtures
import grit.prose.form.{Block, Cell, Column, Item, Level, Mark, Span, Text}

import utest.*

/** [[Docx]] over Word documents written here, part by part: what Word means by each part, and
  * where a hostile one stops it.
  */
object DocxTests extends TestSuite {

  private val W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
  private val Strict = "http://purl.oclc.org/ooxml/wordprocessingml/main"
  private val R = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"

  private def document(body: String, ns: String = W): String =
    s"""<?xml version="1.0" encoding="UTF-8"?><w:document xmlns:w="$ns" xmlns:r="$R"><w:body>$body</w:body></w:document>"""

  private def p(text: String, props: String = ""): String =
    s"""<w:p>${
        if (props.isEmpty) "" else s"<w:pPr>$props</w:pPr>"
      }<w:r><w:t xml:space="preserve">$text</w:t></w:r></w:p>"""

  private def docx(body: String, parts: (String, String)*): java.nio.file.Path =
    Fixtures.zipOf("a.docx", ("word/document.xml" -> document(body)) +: parts)

  private def para(s: String): Block[Nothing] = Block.Paragraph(Text.plain(s))
  private def heading(level: Int, s: String): Block[Nothing] =
    Block.Heading(Level.of(level), Text.plain(s))

  private val styles =
    s"""<w:styles xmlns:w="$W">
       |<w:style w:styleId="Heading1"><w:name w:val="heading 1"/></w:style>
       |<w:style w:styleId="Heading2"><w:name w:val="Heading 2"/></w:style>
       |<w:style w:styleId="ChapterTitle"><w:name w:val="Chapter Title"/><w:basedOn w:val="Heading1"/></w:style>
       |</w:styles>""".stripMargin

  val tests = Tests {
    test("headings are paragraphs in a heading style, or one based on one") {
      Fixtures.blocks(
        Fixtures.read(
          Docx,
          docx(
            p("Intro", """<w:pStyle w:val="ChapterTitle"/>""") + p("Body text.") +
              p("Scope", """<w:pStyle w:val="Heading2"/>"""),
            "word/styles.xml" -> styles
          )
        )
      ) ==> Right(Vector(heading(1, "Intro"), para("Body text."), heading(2, "Scope")))
    }

    test("a strict document is read by its parts' local names") {
      val strict =
        Fixtures.zip("s.docx", "word/document.xml" -> document(p("Strict text."), Strict))
      Fixtures.blocks(Fixtures.read(Docx, strict)) ==> Right(Vector(para("Strict text.")))
    }

    test("tracked deletions and the source of a move are not read; insertions are") {
      val body =
        """<w:p><w:r><w:t xml:space="preserve">kept </w:t></w:r>""" +
          """<w:del><w:r><w:delText>gone </w:delText></w:r></w:del>""" +
          """<w:ins><w:r><w:t>added</w:t></w:r></w:ins></w:p>""" +
          """<w:moveFrom><w:p><w:r><w:t>moved away</w:t></w:r></w:p></w:moveFrom>""" +
          p("moved here")
      Fixtures.blocks(Fixtures.read(Docx, docx(body))) ==>
        Right(Vector(para("kept added"), para("moved here")))
    }

    test("a list's items carry the numbers Word shows, nested by level") {
      val numbering =
        s"""<w:numbering xmlns:w="$W">
           |<w:abstractNum w:abstractNumId="0">
           |<w:lvl w:ilvl="0"><w:start w:val="1"/><w:numFmt w:val="decimal"/><w:lvlText w:val="%1."/></w:lvl>
           |<w:lvl w:ilvl="1"><w:start w:val="1"/><w:numFmt w:val="lowerLetter"/><w:lvlText w:val="%1.%2)"/></w:lvl>
           |</w:abstractNum>
           |<w:num w:numId="1"><w:abstractNumId w:val="0"/></w:num>
           |</w:numbering>""".stripMargin
      def item(level: Int, s: String) =
        p(s, s"""<w:numPr><w:ilvl w:val="$level"/><w:numId w:val="1"/></w:numPr>""")
      val body = item(0, "One") + item(1, "Sub a") + item(1, "Sub b") + item(0, "Two") + p("After.")
      Fixtures.blocks(Fixtures.read(Docx, docx(body, "word/numbering.xml" -> numbering))) ==> Right(
        Vector(
          Block.Bullets(
            Vector(
              Item(
                Vector(
                  para("1. One"),
                  Block.Bullets(
                    Vector(Item(Vector(para("1.a) Sub a"))), Item(Vector(para("1.b) Sub b"))))
                  )
                )
              ),
              Item(Vector(para("2. Two")))
            )
          ),
          para("After.")
        )
      )
    }

    test("a table has as many columns as its widest row, a merged cell blank") {
      def tc(s: String, props: String = "") =
        s"""<w:tc>${if (props.isEmpty) "" else s"<w:tcPr>$props</w:tcPr>"}${p(s)}</w:tc>"""
      val table =
        "<w:tbl>" +
          s"<w:tr>${tc("Name")}${tc("Role")}</w:tr>" +
          s"""<w:tr>${tc("Ana")}${tc("Lead", """<w:gridSpan w:val="2"/>""")}</w:tr>""" +
          s"<w:tr>${tc("Bo")}${tc("Dev")}${tc("remote")}</w:tr>" +
          "</w:tbl>"
      def w(s: String): Cell[Nothing] = Cell.Words(Text.plain(s))
      Fixtures.blocks(Fixtures.read(Docx, docx(table))) ==> Right(
        Vector(
          Block.table(
            Vector(Column(Text.plain("Name")), Column(Text.plain("Role")), Column(Text.empty)),
            Vector(
              Vector(w("Ana"), w("Lead"), Cell.Empty()),
              Vector(w("Bo"), w("Dev"), w("remote"))
            )
          )
        )
      )
    }

    test("a footnote is referenced as [n] and read in an end section; a link keeps its target") {
      val footnotes =
        s"""<w:footnotes xmlns:w="$W">
           |<w:footnote w:type="separator" w:id="-1"><w:p><w:r><w:t>---</w:t></w:r></w:p></w:footnote>
           |<w:footnote w:id="1"><w:p><w:r><w:t>Per the 2024 audit.</w:t></w:r></w:p></w:footnote>
           |</w:footnotes>""".stripMargin
      val rels =
        s"""<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
           |<Relationship Id="rId9" Type="hyperlink" Target="https://example.com/policy" TargetMode="External"/>
           |</Relationships>""".stripMargin
      val body =
        """<w:p><w:r><w:t xml:space="preserve">See </w:t></w:r>""" +
          """<w:hyperlink r:id="rId9"><w:r><w:t>the policy</w:t></w:r></w:hyperlink>""" +
          """<w:r><w:footnoteReference w:id="1"/></w:r></w:p>"""
      Fixtures.blocks(
        Fixtures.read(
          Docx,
          docx(body, "word/footnotes.xml" -> footnotes, "word/_rels/document.xml.rels" -> rels)
        )
      ) ==> Right(
        Vector(
          Block.Paragraph(
            Text(
              Vector(
                Span.plain("See "),
                Span("the policy", Set(Mark.Link("https://example.com/policy"))),
                Span.plain("[1]")
              )
            )
          ),
          heading(2, "Notes"),
          para("[1] Per the 2024 audit.")
        )
      )
    }

    test("a text box is read after its paragraph; comments are said to be missed") {
      val box =
        """<w:p><w:r><w:t>Host.</w:t></w:r><w:r><mc:AlternateContent xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006">""" +
          """<mc:Choice><w:drawing><w:txbxContent><w:p><w:r><w:t>Boxed.</w:t></w:r></w:p></w:txbxContent></w:drawing></mc:Choice>""" +
          """<mc:Fallback><w:pict><w:txbxContent><w:p><w:r><w:t>Boxed.</w:t></w:r></w:p></w:txbxContent></w:pict></mc:Fallback>""" +
          """</mc:AlternateContent></w:r></w:p>"""
      val comments =
        s"""<w:comments xmlns:w="$W"><w:comment w:id="0"><w:p><w:r><w:t>Hm</w:t></w:r></w:p></w:comment></w:comments>"""
      val read = Fixtures.read(Docx, docx(box, "word/comments.xml" -> comments))
      (Fixtures.blocks(read), read.map(_.missed)) ==>
        (Right(Vector(para("Host."), para("Boxed."))), Right(Vector(Missed("comments"))))
    }

    test("a billion laughs stops at its entity, keeping what came before") {
      val laughs =
        s"""<?xml version="1.0"?><!DOCTYPE w:document [<!ENTITY lol "lol"><!ENTITY lol2 "&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;">]>""" +
          s"""<w:document xmlns:w="$W"><w:body>${p("Before.")}${p(
              "&lol2;"
            )}</w:body></w:document>"""
      val read = Fixtures.read(Docx, Fixtures.zip("l.docx", "word/document.xml" -> laughs))
      (Fixtures.blocks(read), read.map(_.covers)) ==>
        (Right(Vector(para("Before."))), Right(Covers.Start(Limit.Damaged)))
    }

    test("a run past the characters read stops there, keeping the text before it") {
      val read = Fixtures.read(
        Docx,
        docx(p("First.") + p("x" * 200000)),
        ReadLimits.lowered(chars = 50000)
      )
      (
        read.map(_.covers),
        Fixtures.blocks(read).map(_.headOption),
        Fixtures.blocks(read).map(_.map(b => Fixtures.plainText(b).length).sum <= 50000)
      ) ==> (Right(Covers.Start(Limit.Chars)), Right(Some(para("First."))), Right(true))
    }

    test("a zip past the bytes unpacked stops there") {
      val read = Fixtures.read(Docx, docx(p("y" * 300000)), ReadLimits.lowered(inflated = 100000))
      read.map(_.covers) ==> Right(Covers.Start(Limit.Inflated))
    }

    test("a zip of more entries than read is damaged; a file that is no zip is not a Word file") {
      val many = Fixtures.zip(
        "m.docx",
        (("word/document.xml" -> document(p("x"))) +: (1 to 20).map(i => s"pad/$i" -> ""))*
      )
      (
        Fixtures.read(Docx, many, ReadLimits.lowered(entries = 10)),
        Fixtures.read(Docx, Fixtures.file("p.pdf", "%PDF-1.7".getBytes.toVector)),
        Fixtures.read(Docx, Fixtures.zip("x.xlsx", "xl/workbook.xml" -> "<workbook/>"))
      ) ==> (Left(Unread.Damaged(Kind.Word)), Left(Unread.NotMine), Left(Unread.NotMine))
    }
  }
}
