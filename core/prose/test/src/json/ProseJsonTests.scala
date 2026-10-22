package grit.prose.json

import java.time.{LocalDate, LocalDateTime}

import grit.prose.form.{Block, Cell, CellKind, Column, Doc, Item, Level, Mark, Span, Text}

import utest.*

/** Prose as stored: written flat, each note once, and read back to the doc it was written from;
  * what is refused, and why.
  */
object ProseJsonTests extends TestSuite {

  private final case class N(n: Int) extends caps.Pure

  private def noteJson(n: N): ujson.Value = ujson.Num(n.n)

  private def readNote(v: ujson.Value): Either[String, N] =
    v.numOpt.map(d => N(d.toInt)).toRight(s"no note: ${ujson.write(v)}")

  private def roundTrip(d: Doc[N]): Either[String, Doc[N]] =
    ProseJson.read(ProseJson.write(d, noteJson), readNote)

  private def plain(s: String): Text[Nothing] = Text.plain(s)

  private def para(s: String): Block[Nothing] = Block.Paragraph(plain(s))

  private def noted(s: String, ns: Int*): Text[N] = Text(
    Vector(Span(s, Set.empty, ns.map(N(_)).toVector))
  )

  private def item(text: String, inner: Block[N]*): Item[N] =
    Item(Block.Paragraph(plain(text)) +: inner.toVector)

  /** The blocks' records: each one's type and its level or depth, item-mores' and quotes' held
    * records after them.
    */
  private def shape(json: ujson.Value): Vector[String] = {
    def one(r: ujson.Value): Vector[String] = {
      val o = r.obj
      val at = o.get("level").orElse(o.get("depth")).fold("")(n => s"${n.num.toInt}")
      s"${o("type").str}$at" +: o.get("block").toVector.flatMap(b => one(b).map("  " + _))
    }
    json("blocks").arr.toVector.flatMap(one)
  }

  /** Every block kind and cell kind, a list and a quote inside each other, notes on several
    * spans and one note on two.
    */
  private val Every: Doc[N] = Doc(
    Vector(
      Block.Heading(Level.of(3), noted("title", 1)),
      Block.Paragraph(
        Text(
          Vector(
            Span("see ", Set(Mark.Strong, Mark.Link("https://e.io")), Vector(N(2), N(3))),
            Span("line\nbreak", Set(Mark.Code), Vector(N(2)))
          )
        )
      ),
      Block.Code(Some("scala"), Vector("val x = 1", "")),
      Block.Code(None, Vector.empty),
      Block.Numbered(
        3,
        Vector(
          item("one", Block.Quote(Vector(para("q"), Block.Bullets(Vector(item("in a quote")))))),
          Item(Vector(Block.Code(None, Vector("first block code")), para("after"))),
          Item(Vector.empty)
        )
      ),
      Block.Quote(
        Vector(
          Block.Bullets(Vector(item("a", Block.Quote(Vector(Block.Paragraph(noted("deep", 4))))))),
          Block.Quote(Vector.empty)
        )
      ),
      Block.Table(
        Vector(Column(noted("h", 5), CellKind.Words), Column(plain("n"), CellKind.Number)),
        Vector(
          Vector(
            Cell.Words(noted("w", 6), Some("=A1")),
            Cell.Number(BigDecimal("1234.50"), plain("1,234.5"))
          ),
          Vector(Cell.Date(LocalDate.of(2026, 10, 22), plain("22 Oct")), Cell.Truth(true)),
          Vector(
            Cell.Date(LocalDateTime.of(2026, 10, 22, 9, 30), plain("9:30")),
            Cell.Empty(Some("=NOW()"))
          )
        )
      ),
      Block.Rule
    )
  )

  val tests = Tests {

    test("a doc reads back as written, each distinct note written once") {
      val json = ProseJson.write(Every, noteJson)
      json("notes") ==> ujson.Arr(1, 2, 3, 4, 5, 6)
      roundTrip(Every) ==> Right(Every)
    }

    test("a list four deep is written flat, each item at its level") {
      val doc: Doc[N] = Doc(
        Vector(
          Block.Bullets(
            Vector(
              item(
                "1",
                Block.Numbered(
                  1,
                  Vector(
                    item(
                      "2",
                      Block.Bullets(Vector(item("3", Block.Numbered(5, Vector(item("4"))))))
                    )
                  )
                ),
                para("1 again")
              ),
              item("1 b")
            )
          )
        )
      )
      val json = ProseJson.write(doc, noteJson)
      shape(json) ==> Vector("item1", "item2", "item3", "item4", "item-more1", "  para", "item1")
      roundTrip(doc) ==> Right(doc)
    }

    test("adjacent lists of one kind, and adjacent quotes at any depth, read back apart") {
      val doc: Doc[N] = Doc(
        Vector(
          Block.Bullets(Vector(item("a"))),
          Block.Bullets(Vector(item("b"))),
          Block.Numbered(1, Vector(item("c"))),
          Block.Numbered(7, Vector(item("d"))),
          Block.Quote(Vector(para("e"))),
          Block.Quote(Vector(Block.Quote(Vector(para("f"))), Block.Quote(Vector(para("g"))))),
          Block.Quote(Vector(Block.Quote(Vector(para("h")))))
        )
      )
      roundTrip(doc) ==> Right(doc)
    }

    test("a list with no items is not written") {
      val doc: Doc[N] = Doc(Vector(para("a"), Block.Bullets(Vector.empty), para("b")))
      roundTrip(doc) ==> Right(Doc(Vector(para("a"), para("b"))))
    }

    // The stored form, pinned: rows written under it are read by every later reader.
    test("the stored form: version, notes once, flat records, spans by t, m and n") {
      val doc: Doc[N] = Doc(
        Vector(
          Block.Paragraph(Text(Vector(Span("x", Set(Mark.Emphasis), Vector(N(9), N(9)))))),
          Block.Numbered(2, Vector(item("y")))
        )
      )
      ujson.write(ProseJson.write(doc, noteJson)) ==>
        """{"v":1,"notes":[9],"blocks":[{"type":"para","text":[{"t":"x","m":["emphasis"],"n":[0,0]}]},""" +
        """{"type":"item","list":"number","level":1,"start":2,"text":[{"t":"y"}]}]}"""
    }

    test("an item-more with no item before it at its level is refused") {
      val json = ujson.Obj(
        "v" -> 1,
        "notes" -> ujson.Arr(),
        "blocks" -> ujson.Arr(
          ujson.Obj("type" -> "item-more", "level" -> 1, "block" -> ujson.Obj("type" -> "rule"))
        )
      )
      ProseJson.read(json, readNote) ==> Left("an item-more with no item before it at its level")
    }

    test("another version is refused, naming it") {
      val json = ujson.Obj("v" -> 2, "notes" -> ujson.Arr(), "blocks" -> ujson.Arr())
      ProseJson.read(json, readNote) ==> Left("prose of version 2, which this reader does not know")
    }

    test("a span noting an index past the notes is refused, naming it") {
      val json = ujson.Obj(
        "v" -> 1,
        "notes" -> ujson.Arr(1),
        "blocks" -> ujson.Arr(
          ujson.Obj(
            "type" -> "para",
            "text" -> ujson.Arr(ujson.Obj("t" -> "x", "n" -> ujson.Arr(1)))
          )
        )
      )
      ProseJson.read(json, readNote) ==> Left("a span's note is no note's index: 1")
    }
  }
}
