package grit.slack.text

import scala.compiletime.constValue
import scala.deriving.Mirror

import grit.prose.form.{Block, Cell, Chart, Column, Doc, Item, Level, Mark, Span, Text}
import grit.prose.markdown.Markdown
import grit.slack.event.UserId

import utest.*

/** [[RichText]]: a reply's prose as Slack messages. */
object RichTextTests extends TestSuite {

  private def rich(elements: ujson.Value*): ujson.Obj =
    ujson.Obj("type" -> "rich_text", "elements" -> ujson.Arr(elements*))

  private def section(elements: ujson.Value*): ujson.Obj =
    ujson.Obj("type" -> "rich_text_section", "elements" -> ujson.Arr(elements*))

  private def text(t: String, style: (String, Boolean)*): ujson.Obj = {
    val o = ujson.Obj("type" -> "text", "text" -> t)
    if (style.nonEmpty) o("style") = ujson.Obj.from(style.map((k, v) => k -> ujson.Bool(v)))
    o
  }

  private def raw(t: String): ujson.Obj = ujson.Obj("type" -> "raw_text", "text" -> t)

  private def para(t: String): Block[Nothing] = Block.Paragraph(Text.plain(t))

  private def only(doc: Doc[Nothing]): ujson.Arr =
    RichText.render(doc) match {
      case Vector(post) => post.blocks
      case other => throw new java.lang.AssertionError(s"${other.size} posts")
    }

  /** How many cases the sum type `T` has, counted by the compiler. */
  /** A chart of `categories` (each a row) and `series` (each a column of numbers), or the
    * test's failure.
    */
  private def chart(
      kind: Chart.Kind,
      title: String,
      categories: Vector[String],
      series: Vector[(String, Vector[String])],
      axis: String = "Day",
      values: Option[String] = Some("Leads")
  ): Block[Nothing] =
    Chart.of(
      kind,
      title,
      values,
      axis +: series.map(_._1),
      categories.indices.toVector.map(k =>
        Cell.Words(Text.plain(categories(k))) +: series.map(s =>
          Cell.Number(BigDecimal(s._2(k)), Text.plain(s._2(k)))
        )
      )
    ) match {
      case Right(c) => Block.Chart(c)
      case Left(u) => throw new java.lang.AssertionError(s"refused: $u")
    }

  private def charted(b: Block[Nothing]): Boolean =
    ujson.write(only(Doc(Vector(b)))).contains("\"data_visualization\"")

  private inline def cases[T](using m: Mirror.SumOf[T]): Int =
    constValue[Tuple.Size[m.MirroredElemLabels]]

  /** What a claim about `blocks` (one example's) must say, and what its drawing must show. */
  private def drawn(blocks: Vector[Block[Nothing]]): (String, String => Boolean) =
    blocks match {
      case Vector(Block.Paragraph(t)) =>
        t.spans.flatMap(_.marks).headOption match {
          case Some(Mark.Strong) => ("bold", _.contains("\"bold\":true"))
          case Some(Mark.Emphasis) => ("italic", _.contains("\"italic\":true"))
          case Some(Mark.Code) => ("code style", _.contains("\"code\":true"))
          case Some(Mark.Link(href)) => ("link", _.contains(s"\"type\":\"link\",\"url\":\"$href\""))
          case None => ("as written", j => j.contains("rich_text_section") && !j.contains("style"))
        }
      case Vector(Block.Heading(_, _)) => ("bold line", _.contains("\"bold\":true"))
      case Vector(Block.Bullets(_)) => ("indented", _.contains("\"style\":\"bullet\",\"indent\":1"))
      case Vector(Block.Numbered(_, _)) => ("numbered", _.contains("\"style\":\"ordered\""))
      case Vector(Block.Quote(_)) =>
        ("plain text", j => j.contains("rich_text_quote") && !j.contains("rich_text_list"))
      case Vector(Block.Code(language, _)) =>
        ("language", j => j.contains("rich_text_preformatted") && language.forall(!j.contains(_)))
      case Vector(Block.Table(_, _)) => ("Slack table", _.contains("\"type\":\"table\""))
      case Vector(Block.Rule) => ("divider", _.contains("\"divider\""))
      case Vector(Block.Chart(_)) => ("Slack chart", _.contains("\"data_visualization\""))
      case other => throw new java.lang.AssertionError(s"one block, not ${other.size}")
    }

  val tests = Tests {
    test(
      "a post's fallback escapes &, < and >, so nothing in it becomes a mention, a broadcast or a link"
    ) {
      RichText
        .render(Doc(Vector(para("<!channel> <@U0BEN0001> & <https://x.io|y>"))))
        .map(
          _.fallback
        ) ==> Vector("&lt;!channel&gt; &lt;@U0BEN0001&gt; &amp; &lt;https://x.io|y&gt;")
    }

    test("a paragraph is a section, each mark a style, a link a link element") {
      val doc = Doc(
        Vector(
          Block.Paragraph(
            Text(
              Vector(
                Span("a ", Set.empty),
                Span("b", Set(Mark.Strong)),
                Span("c", Set(Mark.Emphasis, Mark.Code)),
                Span("d", Set(Mark.Link("https://x.io")))
              )
            )
          )
        )
      )
      RichText.render(doc).map(p => (p.blocks, p.fallback)) ==> Vector(
        (
          ujson.Arr(
            rich(
              section(
                text("a "),
                text("b", "bold" -> true),
                text("c", "italic" -> true, "code" -> true),
                ujson.Obj("type" -> "link", "url" -> "https://x.io", "text" -> "d")
              )
            )
          ),
          "a bcd"
        )
      )
    }

    test(
      "a heading is bold text, a rule a divider, code preformatted without its language, a quote a quote"
    ) {
      only(
        Doc(
          Vector(
            Block.Heading(Level.of(1), Text.plain("Title")),
            Block.Rule,
            Block.Code(Some("scala"), Vector("val a = 1", "val b = 2")),
            Block.Quote(Vector(para("one"), para("two")))
          )
        )
      ) ==> ujson.Arr(
        rich(section(text("Title", "bold" -> true))),
        ujson.Obj("type" -> "divider"),
        rich(
          ujson.Obj(
            "type" -> "rich_text_preformatted",
            "elements" -> ujson.Arr(text("val a = 1\nval b = 2"))
          )
        ),
        rich(ujson.Obj("type" -> "rich_text_quote", "elements" -> ujson.Arr(text("one\ntwo"))))
      )
    }

    test("a nested list is a list at the next indent, and the outer list resumes after it") {
      only(
        Doc(
          Vector(
            Block.Numbered(
              3,
              Vector(
                Item(Vector(para("a"), Block.Bullets(Vector(Item(Vector(para("a1"))))))),
                Item(Vector(para("b")))
              )
            )
          )
        )
      ) ==> ujson.Arr(
        rich(
          ujson.Obj(
            "type" -> "rich_text_list",
            "style" -> "ordered",
            "indent" -> 0,
            "offset" -> 2,
            "elements" -> ujson.Arr(section(text("a")))
          ),
          ujson.Obj(
            "type" -> "rich_text_list",
            "style" -> "bullet",
            "indent" -> 1,
            "offset" -> 0,
            "elements" -> ujson.Arr(section(text("a1")))
          ),
          ujson.Obj(
            "type" -> "rich_text_list",
            "style" -> "ordered",
            "indent" -> 0,
            "offset" -> 3,
            "elements" -> ujson.Arr(section(text("b")))
          )
        )
      )
    }

    test(
      "a table is Slack's table block, its names a bold first row and each cell as it prints"
    ) {
      only(
        Doc(
          Vector(
            Block.table(
              Vector(
                Column(Text.plain("name")),
                Column(Text.plain("n"))
              ),
              Vector(
                Vector(
                  Cell.Words(Text.plain("a")),
                  Cell.Number(BigDecimal(10), Text.plain("10.0"))
                ),
                Vector(Cell.Empty(), Cell.Empty(Some("=B1")))
              )
            )
          )
        )
      ) ==> ujson.Arr(
        ujson.Obj(
          "type" -> "table",
          "rows" -> ujson.Arr(
            ujson.Arr(
              rich(section(text("name", "bold" -> true))),
              rich(section(text("n", "bold" -> true)))
            ),
            ujson.Arr(raw("a"), raw("10.0")),
            ujson.Arr(raw(" "), raw("=B1 (not calculated)"))
          ),
          "column_settings" -> ujson.Arr(
            ujson.Obj("align" -> "left", "is_wrapped" -> true),
            ujson.Obj("align" -> "right")
          )
        )
      )
    }

    test(
      "a table's cells keep their marks as rich text, and words that read as numbers align right"
    ) {
      val md =
        "| Owner | Leads |\n|---|---|\n| **Ana** | 1,200 |\n| [Bo](https://example.com/bo) | $3 |"
      only(Markdown.parse(md)) ==> ujson.Arr(
        ujson.Obj(
          "type" -> "table",
          "rows" -> ujson.Arr(
            ujson.Arr(
              rich(section(text("Owner", "bold" -> true))),
              rich(section(text("Leads", "bold" -> true)))
            ),
            ujson.Arr(rich(section(text("Ana", "bold" -> true))), raw("1,200")),
            ujson.Arr(
              rich(
                section(
                  ujson.Obj("type" -> "link", "url" -> "https://example.com/bo", "text" -> "Bo")
                )
              ),
              raw("$3")
            )
          ),
          "column_settings" -> ujson.Arr(
            ujson.Obj("align" -> "left", "is_wrapped" -> true),
            ujson.Obj("align" -> "right")
          )
        )
      )
    }

    test(
      "a table at each of Slack's limits is a table block, and one past it is preformatted " +
        "text holding every row"
    ) {
      def grid(columns: Int, rows: Int, cell: String = "c"): Block[Nothing] =
        Block.table(
          Vector.tabulate(columns)(k => Column(Text.plain(s"h$k"))),
          Vector.tabulate(rows)(r =>
            Vector.tabulate(columns)(k => Cell.Words(Text.plain(if (k == 0) s"$cell$r" else "c")))
          )
        )
      def wide(n: Int) = "x" * n
      def at(limit: Int): Vector[Block[Nothing]] = Vector(
        grid(1, RichText.MaxTableRows - 1 + limit),
        grid(RichText.MaxTableColumns + limit, 1),
        // "h0" and the cell's text and its row's number, 0
        grid(1, 1, wide(RichText.MaxTableChars - 3 + limit))
      )
      def drawn(b: Block[Nothing]): String =
        RichText.render(Doc(Vector(b))).map(p => ujson.write(p.blocks)).mkString
      def tabled(b: Block[Nothing]): Boolean = drawn(b).contains("\"type\":\"table\"")
      val past = at(1)
      (at(0).map(tabled), past.map(tabled)) ==> (Vector.fill(3)(true), Vector.fill(3)(false))
      assert(past.forall(b => drawn(b).contains("rich_text_preformatted")))
      val rows = RichText.render(Doc(Vector(past(0)))).flatMap(_.fallback.split("\n")).map(_.trim)
      (0 until RichText.MaxTableRows).map(r => s"c$r").filterNot(rows.contains) ==> Vector()
    }

    test("the tables of one post hold at most MaxTableChars characters; the next starts a post") {
      // "h", then one cell of `n` characters
      def one(n: Int): Block[Nothing] =
        Block.table(
          Vector(Column(Text.plain("h"))),
          Vector(Vector(Cell.Words(Text.plain("x" * n))))
        )
      val half = RichText.MaxTableChars / 2 - 1
      def tables(ps: Vector[Post]): Vector[Int] =
        ps.map(_.blocks.value.count(_.obj.get("type").exists(_.str == "table")))
      tables(RichText.render(Doc(Vector(one(half), one(half))))) ==> Vector(2)
      tables(RichText.render(Doc(Vector(one(half), one(half + 1))))) ==> Vector(1, 1)
    }

    test("a chart past Slack's chart limits is its title and a table block") {
      val days = (1 to RichText.MaxPoints + 1).toVector.map(d => s"d$d")
      val c = chart(Chart.Kind.Bar, "t", days, Vector("n" -> Vector.fill(days.length)("1")))
      only(Doc(Vector(c))).value.map(_("type").str) ==> Vector("rich_text", "table")
    }

    test(
      "a table past Slack's limits is preformatted text, its columns aligned, each cell as it prints"
    ) {
      val more = RichText.MaxTableColumns - 1
      only(
        Doc(
          Vector(
            Block.table(
              Vector(
                Column(Text.plain("name")),
                Column(Text.plain("n"))
              ) ++ Vector.fill(more)(Column(Text.plain("-"))),
              Vector(
                Vector(
                  Cell.Words(Text.plain("a")),
                  Cell.Number(BigDecimal(10), Text.plain("10.0"))
                ),
                Vector(Cell.Words(Text.plain("long")), Cell.Empty(Some("=B1")))
              )
            )
          )
        )
      ) ==> ujson.Arr(
        rich(
          ujson.Obj(
            "type" -> "rich_text_preformatted",
            "elements" -> ujson.Arr(
              text(
                Vector(
                  "name | n                   " + " | -" * more,
                  "-----+---------------------" + "-+--" * more,
                  "a    | 10.0                " + " |  " * more,
                  "long | =B1 (not calculated)" + " |  " * more
                ).mkString("\n")
              )
            )
          )
        )
      )
    }

    test("text stays literal: a broadcast or mention written by the model is text, not markup") {
      only(Doc(Vector(para("hey <!channel> and <@U1>")))) ==>
        ujson.Arr(rich(section(text("hey <!channel> and <@U1>"))))
    }

    test("a reply splits at MaxBlocks blocks, and a code block past MaxChars at its lines") {
      RichText
        .render(Doc(Vector.fill(RichText.MaxBlocks + 1)(para("x"))))
        .map(_.blocks.value.size) ==>
        Vector(RichText.MaxBlocks, 1)
      val line = "y" * 100
      val code = Block.Code(None, Vector.fill(45)(line))
      val posts = RichText.render(Doc(Vector(code)))
      posts.map(_.fallback.length) ==> Vector(29 * 101 - 1, 16 * 101 - 1)
      assert(posts.forall(_.fallback.length <= RichText.MaxChars))
    }

    test(
      "a noted reply ends with the line as a plain-text context block in its last post; in a post of its own when that one is full; no posts, none added"
    ) {
      val note = ujson.Obj(
        "type" -> "context",
        "elements" -> ujson.Arr(ujson.Obj("type" -> "plain_text", "text" -> "strong · <m>"))
      )
      val two = RichText.render(Doc(Vector.fill(RichText.MaxBlocks + 1)(para("x"))))
      val noted = RichText.noted(two, "strong · <m>")
      (noted.map(_.blocks.value.size), noted.last.blocks.value.last, noted.map(_.fallback)) ==>
        (Vector(RichText.MaxBlocks, 2), note, two.map(_.fallback))
      val full = RichText.render(Doc(Vector.fill(RichText.MaxBlocks)(para("x"))))
      RichText.noted(full, "strong · <m>").map(_.blocks.value.toVector) ==>
        Vector(full.head.blocks.value.toVector, Vector(note))
      RichText.noted(Vector.empty, "strong · <m>") ==> Vector.empty
    }

    test("a bar, line or area chart is a data_visualization block: a series per column") {
      only(
        Doc(
          Vector(
            chart(
              Chart.Kind.Line,
              "Leads",
              Vector("Mon", "Tue"),
              Vector("Won" -> Vector("3", "-1.5"))
            )
          )
        )
      ) ==> ujson.Arr(
        ujson.Obj(
          "type" -> "data_visualization",
          "title" -> "Leads",
          "chart" -> ujson.Obj(
            "type" -> "line",
            "series" -> ujson.Arr(
              ujson.Obj(
                "name" -> "Won",
                "data" -> ujson.Arr(
                  ujson.Obj("label" -> "Mon", "value" -> 3),
                  ujson.Obj("label" -> "Tue", "value" -> -1.5)
                )
              )
            ),
            "axis_config" -> ujson.Obj(
              "categories" -> ujson.Arr("Mon", "Tue"),
              "x_label" -> "Day",
              "y_label" -> "Leads"
            )
          )
        )
      )
    }

    test("a pie chart is a data_visualization block of segments") {
      only(
        Doc(
          Vector(
            chart(Chart.Kind.Pie, "Leads", Vector("Won", "Lost"), Vector("n" -> Vector("3", "1")))
          )
        )
      ) ==>
        ujson.Arr(
          ujson.Obj(
            "type" -> "data_visualization",
            "title" -> "Leads",
            "chart" -> ujson.Obj(
              "type" -> "pie",
              "segments" -> ujson.Arr(
                ujson.Obj("label" -> "Won", "value" -> 3),
                ujson.Obj("label" -> "Lost", "value" -> 1)
              )
            )
          )
        )
    }

    test("a chart at each of Slack's limits is charted, and one past it is its title and table") {
      def x(n: Int) = "x" * n
      def days(n: Int) = (1 to n).toVector.map(d => s"d$d")
      def one(n: Int) = Vector("n" -> Vector.fill(n)("1"))
      def many(n: Int) = (1 to n).toVector.map(k => s"s$k" -> Vector("1"))
      val bar = Chart.Kind.Bar
      def at(limit: Int): Vector[Block[Nothing]] = Vector(
        chart(bar, x(RichText.MaxChartTitle + limit), Vector("a"), one(1)),
        chart(bar, "t", Vector(x(RichText.MaxChartLabel + limit)), one(1)),
        chart(bar, "t", Vector("a"), Vector(x(RichText.MaxChartLabel + limit) -> Vector("1"))),
        chart(bar, "t", Vector("a"), one(1), axis = x(RichText.MaxAxisLabel + limit)),
        chart(bar, "t", Vector("a"), one(1), values = Some(x(RichText.MaxAxisLabel + limit))),
        chart(bar, "t", Vector("a"), many(RichText.MaxSeries + limit)),
        chart(bar, "t", days(RichText.MaxPoints + limit), one(RichText.MaxPoints + limit)),
        chart(
          Chart.Kind.Pie,
          "t",
          days(RichText.MaxSeries + limit),
          one(RichText.MaxSeries + limit)
        )
      )
      val past = at(1)
      (at(0).map(charted), past.map(charted)) ==> (Vector.fill(8)(true), Vector.fill(8)(false))
      past.map(b => only(Doc(Vector(b)))) ==> past.map {
        case Block.Chart(c) => only(Doc(c.fallback))
        case other => throw new java.lang.AssertionError(s"no chart: $other")
      }
    }

    test("a post holds at most MaxCharts charts; the next starts a post of its own") {
      val c = chart(Chart.Kind.Bar, "t", Vector("a"), Vector("n" -> Vector("1")))
      RichText.render(Doc(Vector.fill(RichText.MaxCharts + 1)(c))).map(_.blocks.value.size) ==>
        Vector(RichText.MaxCharts, 1)
    }

    test("a chart whose table is longer than MaxChars stays one chart") {
      val names = (1 to RichText.MaxSeries).toVector.map(k => f"series $k%013d")
      val days = (1 to RichText.MaxPoints).toVector.map(d => f"day $d%016d")
      val c =
        chart(Chart.Kind.Bar, "t", days, names.map(_ -> Vector.fill(days.length)("1000000000.25")))
      val posts = RichText.render(Doc(Vector(c)))
      assert(posts.headOption.exists(_.fallback.length > RichText.MaxChars))
      posts.map(p => ujson.write(p.blocks).contains("\"data_visualization\"")) ==> Vector(true)
    }

    test("a chart's plain text, where blocks are not shown, is its title and table") {
      val c = chart(
        Chart.Kind.Bar,
        "Leads",
        Vector("Mon"),
        Vector("Won" -> Vector("3"), "Lost" -> Vector("1"))
      )
      RichText.render(Doc(Vector(c))).map(_.fallback) ==>
        Vector("Leads\nDay | Won | Lost\nMon | 3 | 1")
    }

    test("an empty reply is no post") {
      RichText.render(Doc.empty) ==> Vector.empty
    }

    test("what Slack is told names every block and mark render draws, each as it draws it") {
      val parsed = RichText.told.map((md, claim) => (md, claim, Markdown.parse(md).blocks))
      val blocks = parsed.flatMap(_._3)
      val marks = blocks.collect { case Block.Paragraph(t) => t.spans.flatMap(_.marks) }.flatten
      blocks.map(_.ordinal).distinct.sorted ==> (0 until cases[Block[Nothing]]).toVector
      marks.map(_.ordinal).distinct.sorted ==> (0 until cases[Mark]).toVector
      parsed.foreach { (md, claim, bs) =>
        val (says, holds) = drawn(bs)
        val json = ujson.write(only(Doc(bs)))
        assert(RichText.Shown.contains(s"$claim:\n$md"), claim.contains(says), holds(json))
      }
    }

    test("what Slack is told ends with when to write a table, then when to write a chart") {
      assert(
        RichText.Shown.endsWith("\n\n" + Markdown.tableGuide + "\n\n" + Markdown.chartGuide)
      )
    }

    test("a mention is a user element between literal text, and a mention in the plain text") {
      val p = RichText.mentioning("<!channel> ask ", UserId("U1"), " & go")
      (p.blocks, p.fallback) ==> (
        ujson.Arr(
          rich(
            section(
              text("<!channel> ask "),
              ujson.Obj("type" -> "user", "user_id" -> "U1"),
              text(" & go")
            )
          )
        ),
        "&lt;!channel&gt; ask <@U1> &amp; go"
      )
    }
  }
}
