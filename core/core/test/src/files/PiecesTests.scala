package grit.core.files

import grit.core.message.{Message, Tokens}
import grit.core.provider.TokenEstimator
import grit.prose.form.{Block, Cell, Column, Doc, Level, Span, Text}

import utest.*

/** [[Pieces.of]]: a file's tree cut into pieces of about [[Pieces.Target]] tokens. */
object PiecesTests extends TestSuite {

  /** A token for each run of letters or digits, so the counts below are words. */
  private object PerWord extends TokenEstimator {
    def message(message: Message): Tokens = system(message.toString)
    def system(prompt: String): Tokens =
      Tokens("""[\p{L}\d]+""".r.findAllIn(prompt).size.toLong)
  }

  private def on(page: Int, s: String): Text[Locus] =
    Text(Vector(Span(s, Set.empty, Vector(Locus.Page(page)))))
  private def para(s: String, page: Int = 1): Block[Locus] = Block.Paragraph(on(page, s))
  private def heading(level: Int, s: String): Block[Locus] =
    Block.Heading(Level.of(level), on(1, s))

  /** `share` of [[Pieces.Target]], in words. */
  private def of(share: Double): Int = (Tokens.value(Pieces.Target) * share).toInt

  /** `tag` numbered from `from` to `to`: "tagfrom … tagto". */
  private def run(tag: String, from: Int, to: Int): String =
    (from to to).map(i => s"$tag$i").mkString(" ")

  /** `n` words, `tag` then numbered: "tag1 tag2 …". */
  private def words(tag: String, n: Int): String = (1 to n).map(i => s"$tag$i").mkString(" ")

  private def tree(blocks: Block[Locus]*): Understood =
    Understood(Doc(blocks.toVector), Covers.Whole, Vector.empty, FormatName("test"), 1)

  /** A table of a control's tag and text: rows r, s and t of 0.3 [[Pieces.Target]] and u of
    * 1.25.
    */
  private val controls: Understood = {
    val columns = Vector(Column(Text.plain("Control")), Column(Text.plain("Text")))
    def row(tag: String, n: Int): Vector[Cell[Locus]] =
      Vector(Cell.Words(on(1, tag)), Cell.Words(on(1, words(tag, n))))
    tree(
      Block.table(
        columns,
        Vector(row("r", of(0.3)), row("s", of(0.3)), row("t", of(0.3)), row("u", of(1.25)))
      )
    )
  }

  private def plain(p: Piece): String = IndexTextOf(p)
  private object IndexTextOf {
    def apply(p: Piece): String = grit.prose.index.IndexText.render(p.doc.unnoted)
  }

  val tests = Tests {

    test("paragraphs are packed whole, greedily, to at most Target tokens under one heading path") {
      Pieces.Target ==> Tokens(400)
      val (pieces, dropped) = Pieces.of(
        tree(
          heading(1, "Policy"),
          para(words("a", of(0.3))),
          para(words("b", of(0.3))),
          para(words("c", of(0.3))),
          para(words("d", of(0.3))),
          heading(2, "Scope"),
          para(words("e", 10)),
          heading(1, "Audit"),
          para(words("f", 10))
        ),
        PerWord
      )
      (pieces.map(p => (p.heading, p.doc.blocks.size)), dropped) ==> (
        Vector(
          (Vector("Policy"), 3),
          (Vector("Policy"), 1),
          (Vector("Policy", "Scope"), 1),
          (Vector("Audit"), 1)
        ),
        0
      )
    }

    test("each piece is anchored at its own first words in its file's text, cut ones too") {
      val s1 = words("a", of(0.45)) + "."
      val s2 = words("b", of(0.45)) + "."
      val s3 = words("c", of(1.25)) + "."
      val file = tree(heading(1, "Policy"), para("Scope."), para(s"$s1 $s2 $s3"))
      val text = ModelText.text(file)
      val (pieces, _) = Pieces.of(file, PerWord)
      val later = of(1) + 1
      pieces.map(p => (p.anchor.quote, Anchor.find(p.anchor, text).map(_._1))) ==> Vector(
        ("Scope.\n\n" + words("a", 7), Some(text.indexOf("Scope."))),
        (words("c", 8), Some(text.indexOf("c1 "))),
        (run("c", later, later + 7), Some(text.indexOf(s"c$later ")))
      )
    }

    test("a paragraph over Target is cut at its sentences, a sentence over it at words") {
      val s1 = words("a", of(0.45)) + "."
      val s2 = words("b", of(0.45)) + "."
      val s3 = words("c", of(1.25)) + "."
      val (pieces, _) = Pieces.of(tree(para(s"$s1 $s2 $s3")), PerWord)
      pieces.map(plain) ==> Vector(
        s"$s1 $s2",
        words("c", of(1)),
        run("c", of(1) + 1, of(1.25)) + "."
      )
    }

    test("a table's rows are packed with its columns, a row over Target cut at its columns") {
      val (pieces, _) = Pieces.of(controls, PerWord)
      pieces.map(_.doc.blocks.map {
        case Block.Table(cs, rows) => (cs.map(_.name.plain), rows.size)
        case other => (Vector(other.toString), 0)
      }) ==> Vector(
        Vector((Vector("Control", "Text"), 3), (Vector("Control"), 1)),
        Vector((Vector("Text"), 1)),
        Vector((Vector("Text"), 1))
      )
    }

    test("a table's pieces are anchored at their first row's words in its file's text") {
      val text = ModelText.text(controls)
      val (pieces, _) = Pieces.of(controls, PerWord)
      def firstWord(p: Piece): Option[String] = p.doc.blocks
        .collectFirst { case Block.Table(_, rows) =>
          rows.flatten.headOption.map(_.printed.plain)
        }
        .flatten
        .flatMap(_.split(' ').headOption)
      pieces.map(p =>
        Anchor.find(p.anchor, text).map((s, _) => text.substring(s).takeWhile(_ != ' '))
      ) ==>
        pieces.map(firstWord)
    }

    test("past PerFile pieces, the rest are dropped and counted") {
      val (pieces, dropped) =
        Pieces.of(
          tree((1 to Pieces.PerFile + 3).map(i => para(words(s"p${i}x", of(0.75))))*),
          PerWord
        )
      (pieces.size, dropped) ==> (Pieces.PerFile, 3)
    }

    test("a piece's pages are the first and last its text lies on") {
      val (pieces, _) = Pieces.of(tree(para("one", 3), para("two", 4)), PerWord)
      pieces.map(_.pages) ==> Vector(Some((3, 4)))
    }

    test(
      "a piece's fingerprint is the same for its words and path wherever they lie, and differs for others"
    ) {
      def piece(path: String, text: String, page: Int, at: Double) =
        Piece(Vector(path), Doc(Vector(para(text, page))), Anchor(text, "", "", at))
      val here = piece("Policy", "Accounts  are  reviewed.", 3, 0.1)
      val moved = piece("Policy", "accounts are reviewed.", 40, 0.9)
      val other = piece("Policy", "Accounts are removed.", 3, 0.1)
      val elsewhere = piece("Audit", "Accounts are reviewed.", 3, 0.1)
      (
        here.fingerprint == moved.fingerprint,
        here.fingerprint == other.fingerprint,
        here.fingerprint == elsewhere.fingerprint
      ) ==> (true, false, false)
    }
  }
}
