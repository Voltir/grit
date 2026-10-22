package grit.prose.form

import grit.prose.markdown.Markdown

import utest.*

/** A doc's notes: mapped and listed in reading order, folded in that order, and written in by
  * rewriting its spans.
  */
object DocTests extends TestSuite {

  private final case class N(n: Int) extends caps.Pure

  private final case class Topic(id: String) extends caps.Pure

  private def noted(text: String, ns: Int*): Span[N] = Span(text, Set.empty, ns.map(N(_)).toVector)

  private def line[A <: caps.Pure](spans: Span[A]*): Text[A] = Text(spans.toVector)

  /** Every block kind, a note in each, the quote holding a list: 1 to 9 in reading order, and
    * 5 on two spans.
    */
  private val Every: Doc[N] = Doc(
    Vector(
      Block.Heading(Level.of(2), line(noted("title", 1))),
      Block.Paragraph(line(Span.plain("see "), noted("this", 2, 3))),
      Block.Quote(
        Vector(
          Block.Paragraph(line(noted("quoted", 4))),
          Block.Bullets(Vector(Item(Vector(Block.Paragraph(line(noted("a", 5), noted("b", 5)))))))
        )
      ),
      Block.Code(Some("scala"), Vector("val x = 1")),
      Block.Numbered(3, Vector(Item(Vector(Block.Paragraph(line(noted("n", 6))))))),
      Block.Table(
        Vector(
          Column(line(noted("h", 7)), CellKind.Words),
          Column(Text.plain("n"), CellKind.Number)
        ),
        Vector(Vector(Cell.Words(line(noted("c", 8))), Cell.Number(BigDecimal(2), Text.plain("2"))))
          :+ Vector(Cell.Words(line(noted("d", 9))), Cell.Empty())
      ),
      Block.Rule
    )
  )

  val tests = Tests {

    test("map keeps structure and maps every note in reading order") {
      val seen = Vector.newBuilder[Int]
      val tens = Every.map { n =>
        seen += n.n
        N(n.n * 10)
      }
      seen.result() ==> Vector(1, 2, 3, 4, 5, 5, 6, 7, 8, 9)
      tens ==> Doc(
        Vector(
          Block.Heading(Level.of(2), line(noted("title", 10))),
          Block.Paragraph(line(Span.plain("see "), noted("this", 20, 30))),
          Block.Quote(
            Vector(
              Block.Paragraph(line(noted("quoted", 40))),
              Block.Bullets(
                Vector(Item(Vector(Block.Paragraph(line(noted("a", 50), noted("b", 50))))))
              )
            )
          ),
          Block.Code(Some("scala"), Vector("val x = 1")),
          Block.Numbered(3, Vector(Item(Vector(Block.Paragraph(line(noted("n", 60))))))),
          Block.Table(
            Vector(
              Column(line(noted("h", 70)), CellKind.Words),
              Column(Text.plain("n"), CellKind.Number)
            ),
            Vector(
              Vector(Cell.Words(line(noted("c", 80))), Cell.Number(BigDecimal(2), Text.plain("2"))),
              Vector(Cell.Words(line(noted("d", 90))), Cell.Empty())
            )
          ),
          Block.Rule
        )
      )
    }

    test("notes lists a nested quote's notes after its parent's earlier blocks") {
      Every.notes ==> Vector(1, 2, 3, 4, 5, 5, 6, 7, 8, 9).map(N(_))
    }

    test("foldMap folds the notes in notes' order") {
      Every.foldMap("", _ + _)(n => s"${n.n},") ==> "1,2,3,4,5,5,6,7,8,9,"
    }

    test("a cell prints as shown, its formula after it, a formula with no value alone") {
      val bold = Text(Vector(Span("1,234.50", Set(Mark.Strong))))
      Cell.Number(BigDecimal("1234.5"), bold).printed ==> bold
      Cell.Number(BigDecimal("1234.5"), bold, Some("=SUM(B2:B9)")).printed ==>
        Text(Vector(Span("1,234.50", Set(Mark.Strong)), Span.plain(" (=SUM(B2:B9))")))
      Cell.Truth(false, Some("=A1>2")).printed ==> Text.plain("FALSE (=A1>2)")
      Cell.Empty(Some("=NOW()")).printed ==> Text.plain("=NOW() (not calculated)")
      Cell.Empty().printed ==> Text.empty
    }

    test(
      "table folds a row past the columns into its last cell as printed, an empty end left out, and fills a short one"
    ) {
      val cols =
        Vector(Column(Text.plain("a"), CellKind.Words), Column(Text.plain("b"), CellKind.Truth))
      val x = Cell.Words(Text.plain("x"))
      Block.table(
        cols,
        Vector(
          Vector(x),
          Vector(x, Cell.Truth(true), Cell.Empty(Some("=A1")), Cell.Empty()),
          Vector(x, Cell.Truth(true), Cell.Empty())
        )
      ) ==> Block.Table(
        cols,
        Vector(
          Vector(x, Cell.Empty()),
          Vector(x, Cell.Words(Text.plain("TRUE | =A1 (not calculated)"))),
          Vector(x, Cell.Truth(true))
        )
      )
    }

    test("mapSpans rewrites a topic: link into a note and leaves others") {
      val doc =
        Markdown.parse("see [the plan](topic:plan) and [docs](https://e.io)\n\n> [*q*](topic:q)")
      val noted = doc.mapSpans { s =>
        s.marks.collectFirst { case l @ Mark.Link(h) if h.startsWith("topic:") => l } match {
          case Some(l) => Span(s.text, s.marks - l, Vector(Topic(l.href.stripPrefix("topic:"))))
          case None => s
        }
      }
      noted ==> Doc(
        Vector(
          Block.Paragraph(
            line(
              Span.plain("see "),
              Span("the plan", Set.empty, Vector(Topic("plan"))),
              Span.plain(" and "),
              Span("docs", Set(Mark.Link("https://e.io")))
            )
          ),
          Block.Quote(
            Vector(Block.Paragraph(line(Span("q", Set(Mark.Emphasis), Vector(Topic("q"))))))
          )
        )
      )
    }
  }
}
