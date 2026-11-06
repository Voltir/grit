package grit.prose.markdown

import grit.prose.form.{Block, Cell, Chart, Column, Doc, Mark, Span, Text}

import utest.*

/** A chart as the model writes it, a fenced block whose body is a pipe table: read, written
  * back, read while it streams, and what is shown of a fence that makes no chart.
  */
object ChartMarkdownTests extends TestSuite {

  private def words(s: String): Cell[Nothing] = Cell.Words(Text.plain(s))

  private val Table: Vector[String] =
    Vector("| Day | Won | Lost |", "|---|---|---|", "| Mon | 3 | 1 |", "| Tue | 5 | 0 |")

  private def chart(kind: Chart.Kind, title: String, values: Option[String]): Block[Nothing] =
    Chart.of(
      kind,
      title,
      values,
      Vector("Day", "Won", "Lost"),
      Vector(Vector("Mon", "3", "1"), Vector("Tue", "5", "0")).map(_.map(words))
    ) match {
      case Right(c) => Block.Chart(c)
      case Left(u) => throw new java.lang.AssertionError(s"refused: $u")
    }

  private def fenced(info: String, body: Vector[String] = Table): String =
    (s"```$info" +: body :+ "```").mkString("\n")

  private def table: Block[Nothing] =
    Block.table(
      Vector("Day", "Won", "Lost").map(c => Column(Text.plain(c))),
      Vector(Vector("Mon", "3", "1"), Vector("Tue", "5", "0")).map(_.map(words))
    )

  private def strong(s: String): Block[Nothing] =
    Block.Paragraph(Text(Vector(Span(s, Set(Mark.Strong)))))

  val tests = Tests {

    test("a chart fence reads as a chart, in pandoc's braced form and in the bare one") {
      val want = Doc(Vector(chart(Chart.Kind.Line, "Leads per day", Some("Leads"))))
      (
        Markdown.parse(fenced("""{.chart type=line title="Leads per day" values=Leads}""")),
        Markdown.parse(fenced("""chart type=line title="Leads per day" values="Leads"""")),
        Markdown.parse(fenced("""chart   values=Leads type=line   title="Leads per day" """))
      ) ==> (want, want, want)
    }

    test("a chart's type is bar unless given, and a quoted title holds an escaped quote") {
      Markdown.parse(fenced("""chart title="The \"best\" days \\ so far"""")) ==>
        Doc(Vector(chart(Chart.Kind.Bar, """The "best" days \ so far""", None)))
    }

    test("a chart fence that makes no chart shows its title and table, or its code when no table") {
      val prose = Vector("Mon was 3.", "Tue was 5.")
      (
        Markdown.parse(fenced("""chart type=scatter title="Leads per day"""")),
        Markdown.parse(fenced("chart")),
        Markdown.parse(fenced("""chart title="Leads"""", prose))
      ) ==> (
        Doc(Vector(strong("Leads per day"), table)),
        Doc(Vector(table)),
        Doc(Vector(Block.Code(Some("chart"), prose)))
      )
    }

    test("a chart is written as pandoc's braced fence, its table as a table is written") {
      Markdown.write(Doc(Vector(chart(Chart.Kind.Bar, "Leads per day", Some("Leads"))))) ==>
        fenced(
          """{.chart type=bar title="Leads per day" values="Leads"}""",
          Vector("| Day | Won | Lost |", "|---|---|---|", "| Mon | 3 | 1 |", "| Tue | 5 | 0 |")
        )
    }

    test("a chart written exactly reads back as itself, a title with a quote or a backtick too") {
      val docs = Vector(
        Doc(Vector(chart(Chart.Kind.Area, "Leads per day", Some("Leads")))),
        Doc(Vector(chart(Chart.Kind.Bar, "The \"best\" `days`", None)))
      )
      docs.map(d => Markdown.parse(Markdown.writeExact(d))) ==> docs
    }

    test("a streaming chart fence is held back until it closes, then is the chart") {
      val open = "Here:\n\n" + fenced("""chart title="Leads per day"""").stripSuffix("```")
      (Markdown.parsePrefix(open), Markdown.parsePrefix(open + "```\n")) ==> (
        Doc(Vector(Block.Paragraph(Text.plain("Here:")))),
        Doc(
          Vector(Block.Paragraph(Text.plain("Here:")), chart(Chart.Kind.Bar, "Leads per day", None))
        )
      )
    }

    test("the guide's example reads as a chart") {
      Markdown.parse(Markdown.chartExample).blocks.map {
        case Block.Chart(c) => s"${c.title}: ${c.categories.mkString(", ")}"
        case other => s"no chart: $other"
      } ==> Vector("Leads per day: Mon, Tue, Wed")
    }
  }
}
