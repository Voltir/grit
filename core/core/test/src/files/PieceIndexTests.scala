package grit.core.files

import grit.prose.form.{Block, Doc, Span, Text}

import utest.*

/** [[PieceIndex]]: the text a piece is searched by, and a query aliased as it is. */
object PieceIndexTests extends TestSuite {

  private def para(s: String): Block[Locus] =
    Block.Paragraph(Text(Vector(Span(s, Set.empty, Vector(Locus.Page(1))))))

  val tests = Tests {

    test("a piece is searched by its file's name in words, its heading path, then its words") {
      val piece = Piece(Vector("Access Control", "Account Management"), Doc(Vector(para("Review accounts."))))
      PieceIndex.of("security_plan-final.pdf", piece) ==>
        "security plan final\nAccess Control > Account Management\nReview accounts."
    }

    test("an identifier is also written as its alias, in a piece's text and in a query") {
      val piece = Piece(Vector("AC-2 Account Management"), Doc(Vector(para("See AC-2(3) and SP 800-53."))))
      (
        PieceIndex.of("controls.pdf", piece),
        PieceIndex.query("what does ac-2(3) say, and A.5.1?")
      ) ==> (
        "controls\nAC-2 AC2 Account Management\nSee AC-2(3) AC2x3 and SP 800-53 80053.",
        "what does ac-2(3) ac2x3 say, and A.5.1 A51?"
      )
    }

    test("past Most characters, a piece's text is cut") {
      val piece = Piece(Vector.empty, Doc(Vector(para("word " * 2000))))
      PieceIndex.of("f.pdf", piece).length ==> PieceIndex.Most
    }
  }
}
