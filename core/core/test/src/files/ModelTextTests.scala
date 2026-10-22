package grit.core.files

import java.nio.charset.StandardCharsets.UTF_8

import grit.prose.form.{Block, Doc, Level, Span, Text}

import utest.*

/** [[ModelText.of]]: a range of a file's tree as the model reads it. */
object ModelTextTests extends TestSuite {

  private def on(page: Int, s: String): Text[Locus] =
    Text(Vector(Span(s, Set.empty, Vector(Locus.Page(page)))))
  private def para(page: Int, s: String): Block[Locus] = Block.Paragraph(on(page, s))
  private def heading(level: Int, page: Int, s: String): Block[Locus] =
    Block.Heading(Level.of(level), on(page, s))

  private def tree(blocks: Vector[Block[Locus]], covers: Covers = Covers.Whole): Understood =
    Understood(Doc(blocks), covers, Vector.empty, FormatName("test"), 1)

  /** 492 pages, one paragraph each; chapter 3 opens on page 210, its section 3.2 on 212 and 3.3
    * on 216.
    */
  private val book: Understood = tree((1 to 492).toVector.flatMap { n =>
    val opens = n match {
      case 1 => Vector(heading(1, 1, "1 Introduction"))
      case 210 => Vector(heading(1, 210, "3 Policy"))
      case 212 => Vector(heading(2, 212, "3.2 Access control"))
      case 216 => Vector(heading(2, 216, "3.3 Audit"))
      case _ => Vector.empty
    }
    opens :+ para(n, s"Text of page $n.")
  })

  private def size(s: String): Int = s.getBytes(UTF_8).length

  val tests = Tests {
    test("a run of pages says which pages of how many it covers, and reads on at the next run") {
      val read = ModelText.of(book, FileRange.pages(212, 214).getOrElse(FileRange.Start))
      (
        read.covered,
        read.text,
        read.next
      ) ==> (
        "pages 212–214 of 492",
        // Markdown's escape keeps "3.2" from reading as a list's number.
        "[page 212]\n## 3\\.2 Access control\n\nText of page 212.\n\n[page 213]\nText of page 213." +
          "\n\n[page 214]\nText of page 214.",
        FileRange.pages(215, 217)
      )
    }

    test("pages past the end read the last page") {
      val read = ModelText.of(book, FileRange.pages(600, 610).getOrElse(FileRange.Start))
      (read.covered, read.text, read.next) ==> (
        "page 492 of 492",
        "[page 492]\nText of page 492.",
        None
      )
    }

    test(
      "a section is the one whose heading path ends with the words asked, up to the next of its rank"
    ) {
      val read = ModelText.of(book, FileRange.Section("policy  >  3.2 ACCESS control"))
      (
        read.covered,
        read.text.linesIterator.toVector.filter(_.startsWith("Text")).size,
        read.next
      ) ==>
        ("pages 212–215 of 492", 4, None)
    }

    test("a section not found is the outline, saying there is none") {
      val read = ModelText.of(book, FileRange.Section("9.9 Nowhere"))
      read ==> FileRead(
        "There is no section '9.9 Nowhere'.",
        "its outline",
        None,
        Some(
          "- 1 Introduction (page 1)\n- 3 Policy (page 210)\n  - 3.2 Access control (page 212)\n" +
            "  - 3.3 Audit (page 216)"
        )
      )
    }

    test(
      "a read from the start keeps whole blocks within Room, and reading on starts at the next"
    ) {
      val long =
        tree((1 to 200).toVector.map(i => Block.Paragraph(Text.plain(f"$i%03d " + "x" * 996))))
      val first = ModelText.of(long, FileRange.Start)
      val shownBlocks = first.text.split("\n\n").toVector
      val second = first.next.map(ModelText.of(long, _))
      (
        size(first.shown) <= ModelText.Room,
        shownBlocks.forall(_.length == 1000),
        second.map(_.text.take(4)),
        shownBlocks.lastOption.map(_.take(3).toInt + 1)
      ) ==> (
        true,
        true,
        second.map(_ => f"${shownBlocks.size + 1}%03d "),
        Some(shownBlocks.size + 1)
      )
    }

    test("a block too long alone is cut at a line, and reading on starts at the next line") {
      val lines = (1 to 5000).map(i => f"line $i%05d").toVector
      val code = tree(Vector(Block.Code(None, lines)))
      val first = ModelText.of(code, FileRange.Start)
      val shown = first.text.linesIterator.toVector
      val second = first.next.map(ModelText.of(code, _))
      (
        size(first.shown) <= ModelText.Room,
        shown.lastOption.exists(_.startsWith("line ")),
        second.map(_.text.linesIterator.next())
      ) ==> (true, true, Some(f"line ${shown.size}%05d"))
    }

    test("what the reader stopped at and what it did not read are said beside the pages") {
      val cut = Understood(
        Doc(Vector(para(1, "One."), para(2, "Two."))),
        Covers.Part(2, 492, CoverUnit.Pages, Limit.Glyphs),
        Vector(Missed("comments"), Missed("a text box")),
        FormatName("test"),
        1
      )
      ModelText.of(cut, FileRange.Start).covered ==>
        "pages 1–2 of 2; only its first 2 of 492 pages were read: it is too long to read whole " +
        "here; not read here: comments, a text box"
    }
  }
}
