package grit.prose.form

import utest.*

/** A chart made from a table's columns and rows, what it refuses, and what is drawn of it where
  * it cannot be charted.
  */
object ChartTests extends TestSuite {

  private def words(s: String): Cell[Nothing] = Cell.Words(Text.plain(s))

  private def num(s: String): Cell.Number = Cell.Number(BigDecimal(s), Text.plain(s))

  private def rows(rs: Vector[String]*): Vector[Vector[Cell[Nothing]]] =
    rs.toVector.map(_.map(words))

  private val Leads: Vector[Vector[Cell[Nothing]]] =
    rows(Vector("Mon", "3", "1"), Vector("Tue", "5", "0"))

  private def of(
      columns: Vector[String],
      rs: Vector[Vector[Cell[Nothing]]],
      kind: Chart.Kind = Chart.Kind.Bar,
      title: String = "Leads per day"
  ): Either[Chart.Unfit, Chart] = Chart.of(kind, title, Some("Leads"), columns, rs)

  private def made(e: Either[Chart.Unfit, Chart]): Chart = e match {
    case Right(c) => c
    case Left(u) => throw new java.lang.AssertionError(s"refused: $u")
  }

  private def strong(s: String): Block[Nothing] =
    Block.Paragraph(Text(Vector(Span(s, Set(Mark.Strong)))))

  private def table(columns: Vector[String], rs: Vector[Vector[Cell[Nothing]]]): Block[Nothing] =
    Block.table(columns.map(c => Column(Text.plain(c))), rs)

  val tests = Tests {

    test("a table's first column is the categories and each other column a series") {
      val c = made(of(Vector("Day", "Won", "Lost"), Leads))
      (c.kind, c.title, c.axis, c.values, c.categories, c.series.map(s => (s.name, s.values))) ==> (
        Chart.Kind.Bar,
        "Leads per day",
        "Day",
        Some("Leads"),
        Vector("Mon", "Tue"),
        Vector("Won" -> Vector(num("3"), num("5")), "Lost" -> Vector(num("1"), num("0")))
      )
    }

    test("its table is the columns and rows it was made of, the numbers as cells") {
      val c = made(of(Vector("Day", "Won", "Lost"), Leads))
      (c.table: Block[Nothing]) ==> table(
        Vector("Day", "Won", "Lost"),
        Vector(Vector(words("Mon"), num("3"), num("1")), Vector(words("Tue"), num("5"), num("0")))
      )
    }

    test("each table that is no chart is refused, naming the first reason") {
      import Chart.Unfit.*
      val pie = Chart.Kind.Pie
      Vector(
        of(Vector("Day", "Won", "Lost"), Leads, title = "  "),
        of(Vector("Day"), rows(Vector("Mon"))),
        of(Vector("Day", "Won"), Vector.empty),
        of(Vector("Day", "Won", "Lost"), rows(Vector("Mon", "3", "1"), Vector("Tue", "5"))),
        of(Vector("Day", "Won"), rows(Vector("Mon", "3"), Vector("Mon", "4"))),
        of(Vector("Day", "Won", "Won"), rows(Vector("Mon", "3", "1"))),
        of(Vector("Day", "Won"), rows(Vector("Mon", "3"), Vector("Tue", "five"))),
        of(Vector("Day", "Won", "Lost"), Leads, kind = pie),
        of(Vector("Day", "Won"), rows(Vector("Mon", "3"), Vector("Tue", "0")), kind = pie)
      ) ==> Vector(
        NoTitle,
        NoSeries,
        NoCategories,
        Ragged("Tue"),
        SameCategory("Mon"),
        SameName("Won"),
        NotNumber("Won", "Tue", "five"),
        PieOfMany(2),
        PieNotPositive("Tue")
      ).map(Left(_))
    }

    test("one series of no negative value falls back to its title and table with bars") {
      val c = of(
        Vector("Day", "Leads"),
        rows(Vector("Mon", "10"), Vector("Tue", "5"), Vector("Wed", "0"), Vector("Thu", "0.1"))
      )
      c.map(_.fallback) ==> Right(
        Vector(
          strong("Leads per day"),
          table(
            Vector("Day", "Leads", ""),
            Vector(
              Vector(words("Mon"), num("10"), words("█" * Chart.BarWidth)),
              Vector(words("Tue"), num("5"), words("█" * (Chart.BarWidth / 2))),
              Vector(words("Wed"), num("0"), Cell.Empty()),
              Vector(words("Thu"), num("0.1"), words("█"))
            )
          )
        )
      )
    }

    test("two series, or one with a negative value, fall back to the title and table alone") {
      val two = made(of(Vector("Day", "Won", "Lost"), Leads))
      val negative = made(of(Vector("Day", "Net"), rows(Vector("Mon", "-2"), Vector("Tue", "4"))))
      (two.fallback, negative.fallback) ==> (
        Vector(strong("Leads per day"), two.table),
        Vector(strong("Leads per day"), negative.table)
      )
    }
  }
}
