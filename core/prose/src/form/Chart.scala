package grit.prose.form

/** Numbers compared across categories, drawn as `kind`. Its data is a table ([[table]]): the
  * categories down the first column under `axis`, a column of numbers per series. `values`
  * says what the numbers are, when its writer said.
  */
final case class Chart private (
    kind: Chart.Kind,
    title: String,
    axis: String,
    values: Option[String],
    categories: Vector[String],
    series: Vector[Chart.Series]
) {

  /** `axis` then each series' name as the columns, a row per category, each number as its
    * source showed it.
    */
  def table: Block.Table[Nothing] =
    Block.Table(
      (axis +: series.map(_.name)).map(n => Column(Text.plain(n))),
      categories.indices.toVector.map(k =>
        Cell.Words(Text.plain(categories(k))) +: series.map(_.values(k))
      )
    )

  /** What a consumer that cannot chart draws: the title as a strong paragraph, then
    * [[table]]; for one series with no negative value, the table with a last, unnamed column
    * of bars, [[Chart.BarWidth]] `█` at the largest value, in proportion below it, and at least
    * one for any value above 0.
    */
  def fallback: Vector[Block[Nothing]] = {
    val heading = Block.Paragraph(Text(Vector(Span(title, Set(Mark.Strong)))))
    series match {
      case Vector(one) if one.values.forall(_.value >= 0) =>
        val top = one.values.map(_.value).maxOption.getOrElse(BigDecimal(0))
        def bar(v: BigDecimal): String =
          if (top <= 0 || v <= 0) ""
          else
            "█" * math.max(
              1,
              (v / top * Chart.BarWidth).setScale(0, BigDecimal.RoundingMode.HALF_UP).toInt
            )
        val t = table
        Vector(
          heading,
          Block.table(
            t.columns :+ Column(Text.empty),
            t.rows.indices.toVector.map(k =>
              t.rows(k) :+ Cell.Words(Text.plain(bar(one.values(k).value)))
            )
          )
        )
      case _ => Vector(heading, table)
    }
  }
}

object Chart {

  /** How a chart draws its numbers. */
  enum Kind {
    case Bar, Line, Area, Pie
  }

  /** A series' name, and one number per category, in the categories' order. */
  final case class Series private[form] (name: String, values: Vector[Cell.Number])

  /** Why [[of]] made no chart. */
  enum Unfit {
    case NoTitle
    case NoSeries
    case NoCategories
    case Ragged(category: String)
    case SameCategory(category: String)
    case SameName(series: String)
    case NotNumber(series: String, category: String, shown: String)
    case PieOfMany(series: Int)
    case PieNotPositive(category: String)
  }

  /** 20: the longest bar in [[Chart.fallback]], in characters. */
  val BarWidth: Int = 20

  /** The chart of `columns` and `rows`, laid out as [[Chart.table]] is: the first column's
    * cells, as printed, the categories, under the first column's name; every other column a
    * series named by its column, whose cells are each a [[Cell.Number]] or words that read as
    * one ([[Cell.Number.read]]); a pie of one series, each value above 0. The first reason it
    * is none, in [[Unfit]]'s order, otherwise; a blank title is [[Unfit.NoTitle]].
    */
  def of(
      kind: Kind,
      title: String,
      values: Option[String],
      columns: Vector[String],
      rows: Vector[Vector[Cell[Nothing]]]
  ): Either[Unfit, Chart] = {
    def category(row: Vector[Cell[Nothing]]): String =
      row.headOption.map(_.printed.plain.trim).getOrElse("")
    def first[X](xs: Vector[X]): Option[X] = xs.headOption
    def repeated(xs: Vector[String]): Option[String] =
      xs.diff(xs.distinct).headOption
    val names = columns.drop(1)
    val categories = rows.map(category)
    def number(name: String, at: Int, row: Vector[Cell[Nothing]]): Either[Unfit, Cell.Number] =
      row.lift(at) match {
        case Some(n: Cell.Number) => Right(n)
        case Some(c) =>
          val shown = c.printed.plain
          Cell.Number.read(shown).toRight(Unfit.NotNumber(name, category(row), shown))
        case None => Left(Unfit.Ragged(category(row)))
      }
    for {
      _ <- Either.cond(title.trim.nonEmpty, (), Unfit.NoTitle)
      _ <- Either.cond(names.nonEmpty, (), Unfit.NoSeries)
      _ <- Either.cond(rows.nonEmpty, (), Unfit.NoCategories)
      _ <- first(rows.filter(_.length != columns.length))
        .map(r => Unfit.Ragged(category(r)))
        .toLeft(())
      _ <- repeated(categories).map(Unfit.SameCategory(_)).toLeft(())
      _ <- repeated(names).map(Unfit.SameName(_)).toLeft(())
      series <- traverse(names.zipWithIndex)((name, k) =>
        traverse(rows)(number(name, k + 1, _)).map(Series(name, _))
      )
      _ <- Either.cond(kind != Kind.Pie || series.length == 1, (), Unfit.PieOfMany(series.length))
      _ <- (if (kind == Kind.Pie) series.flatMap(s => categories.zip(s.values)) else Vector.empty)
        .collectFirst { case (c, v) if v.value <= 0 => Unfit.PieNotPositive(c) }
        .toLeft(())
    } yield Chart(kind, title, columns.headOption.getOrElse(""), values, categories, series)
  }

  private def traverse[X, B](xs: Vector[X])(f: X => Either[Unfit, B]): Either[Unfit, Vector[B]] =
    xs.foldLeft[Either[Unfit, Vector[B]]](Right(Vector.empty))((acc, x) =>
      acc.flatMap(bs => f(x).map(bs :+ _))
    )
}
