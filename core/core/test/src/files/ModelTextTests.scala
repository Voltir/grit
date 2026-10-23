package grit.core.files

import java.nio.charset.StandardCharsets.UTF_8

import grit.prose.form.{Block, Doc, Level, Mark, Span, Text}

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
        ),
        false
      )
    }

    test("a read is whole only when it shows all of a file its reader read whole") {
      val small = tree(Vector(para(1, "One."), para(2, "Two.")))
      val cut = small.copy(covers = Covers.Part(2, 9, CoverUnit.Pages, Limit.Time))
      Vector(
        ModelText.of(small, FileRange.Start).whole,
        ModelText.of(small, FileRange.pages(1, 2).getOrElse(FileRange.Start)).whole,
        ModelText.of(small, FileRange.pages(2, 2).getOrElse(FileRange.Start)).whole,
        ModelText.of(cut, FileRange.Start).whole
      ) ==> Vector(true, true, false, false)
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

    test("near a passage reads the page it lies on now, though pages came before it") {
      val passage = "Accounts are reviewed every ninety days by the system owner."
      val was = tree(Vector(heading(1, 1, "Policy"), para(1, "Scope."), para(2, passage)))
      val text = ModelText.text(was)
      val anchor = Anchor.of(text, text.indexOf(passage), text.indexOf(passage) + passage.length)
      val now = tree(
        Vector(heading(1, 1, "Policy"), para(1, "Scope."), para(2, "New."), para(3, "More.")) :+
          para(4, passage) :+ para(5, "After.")
      )
      val read = ModelText.of(now, FileRange.Near(anchor))
      (read.covered, read.text, read.next) ==>
        ("page 4 of 5", s"[page 4]\n$passage", FileRange.pages(5, 5))
    }

    test(
      "a file's text is its blocks' plain words, and an anchor in it is found in what is shown"
    ) {
      val rule = Text(
        Vector(
          Span("Set a_b_c_d to ", Set.empty, Vector(Locus.Page(2))),
          Span("daily", Set(Mark.Emphasis), Vector(Locus.Page(2))),
          Span(" [Assignment: frequency].", Set.empty, Vector(Locus.Page(2)))
        )
      )
      val file = tree(Vector(para(1, "Scope."), Block.Paragraph(rule)))
      val text = ModelText.text(file)
      val anchor = Anchor.of(text, text.indexOf("Set"), text.length)
      val shown = ModelText.of(file, FileRange.Start).text
      (text, Anchor.find(anchor, shown).map((s, _) => shown.substring(s).take(9))) ==>
        ("Scope.\n\nSet a_b_c_d to daily [Assignment: frequency].", Some("Set a_b_c"))
    }

    test("near a quote reads the page its words lie on, though its lines break elsewhere") {
      val now = tree(
        Vector(para(1, "Scope."), para(2, "SR-10 Developer screening.")) ++ Vector(
          heading(2, 3, "SR-11 COMPONENT AUTHENTICITY"),
          para(3, "a. Develop and implement\nanti-counterfeit policy and procedures that"),
          para(4, "After.")
        )
      )
      val near = "SR-11 COMPONENT AUTHENTICITY a. Develop and implement anti-counterfeit policy " +
        "and procedures"
      val read = FileRange.parse(None, None, None, Some(near)).map(ModelText.of(now, _))
      read.map(r => (r.covered, r.next)) ==> Right(("page 3 of 4", FileRange.pages(4, 4)))
    }

    test("near a quote no file words match says it is gone") {
      val near = "Suppliers are vetted by an independent panel each year."
      FileRange.parse(None, None, None, Some(near)).map(ModelText.of(book, _).text) ==>
        Right("That passage is gone from the file now.")
    }

    test("near a passage in a file not counted in pages reads on from its block") {
      val passage = "Accounts are reviewed every ninety days by the system owner."
      def plain(s: String): Block[Locus] = Block.Paragraph(Text.plain(s))
      val was = tree(Vector(plain("Scope."), plain(passage)))
      val text = ModelText.text(was)
      val anchor = Anchor.of(text, text.indexOf(passage), text.indexOf(passage) + passage.length)
      val now = tree(Vector(plain("Scope."), plain("A new rule."), plain(passage), plain("End.")))
      val read = ModelText.of(now, FileRange.Near(anchor))
      (read.covered, read.text) ==> ("characters 21–87 of 87", s"$passage\n\nEnd.")
    }

    test("near a passage no longer in the file says it is gone, with the outline") {
      val anchor = Anchor("Accounts are reviewed every ninety days by", "Scope.\n\n", " the", 0.5)
      ModelText.of(book, FileRange.Near(anchor)) ==> FileRead(
        "That passage is gone from the file now.",
        "its outline",
        None,
        Some(
          "- 1 Introduction (page 1)\n- 3 Policy (page 210)\n  - 3.2 Access control (page 212)\n" +
            "  - 3.3 Audit (page 216)"
        ),
        false
      )
    }
  }
}
