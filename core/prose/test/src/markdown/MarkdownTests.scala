package grit.prose.markdown

import grit.prose.form.{Block, Doc, Item, Level, Mark, Span, Text}

import utest.*

/** Markdown read into prose: each construct of the subset, what is literal, and a reply
  * read at every length it passes through while it streams.
  */
object MarkdownTests extends TestSuite {

  private def plain(s: String): Text = Text.plain(s)

  private def marked(s: String, marks: Mark*): Span = Span(s, marks.toSet)

  private def para(spans: Span*): Block = Block.Paragraph(Text(spans.toVector))

  /** Every block, depth first: the shape a reader sees, whatever the words. */
  private def kinds(blocks: Vector[Block]): Vector[String] =
    blocks.flatMap {
      case Block.Paragraph(_) => Vector("p")
      case Block.Heading(l, _) => Vector(s"h${l.value}")
      case Block.Bullets(items) => "ul" +: items.flatMap(i => "li" +: kinds(i.blocks))
      case Block.Numbered(_, items) => "ol" +: items.flatMap(i => "li" +: kinds(i.blocks))
      case Block.Quote(bs) => "quote" +: kinds(bs)
      case Block.Code(_, _) => Vector("code")
      case Block.Table(_, _) => Vector("table")
      case Block.Rule => Vector("hr")
    }

  /** Every character a reader would be shown, marks dropped. */
  private def shown(blocks: Vector[Block]): String =
    blocks
      .map {
        case Block.Paragraph(t) => t.plain
        case Block.Heading(_, t) => t.plain
        case Block.Bullets(items) => items.map(i => shown(i.blocks)).mkString("\n")
        case Block.Numbered(_, items) => items.map(i => shown(i.blocks)).mkString("\n")
        case Block.Quote(bs) => shown(bs)
        case Block.Code(_, ls) => ls.mkString("\n")
        case Block.Table(h, rs) => (h +: rs).map(_.map(_.plain).mkString("|")).mkString("\n")
        case Block.Rule => ""
      }
      .mkString("\n")

  /** A reply in every construct the subset has, as a model writes one. */
  val Sample: String =
    """Here is the **plan**, with *care* and `code`.
      |
      |# Steps
      |
      |1. Read the [docs](https://example.com/docs) first
      |2. Then **write
      |   it** out
      |   - nested *one*
      |   - nested two
      |
      |> A quote, with `inline` code.
      |
      |```scala
      |def twice(x: Int): Int = x + x
      |  // indented
      |```
      |
      || name | role |
      ||------|------|
      || ᚨ | grit |
      || ᛗ | you |
      |
      |---
      |Done.""".stripMargin

  val tests = Tests {

    test("paragraphs, with the line breaks inside them kept") {
      Markdown.parse("one\ntwo\n\nthree") ==> Doc(
        Vector(Block.Paragraph(plain("one\ntwo")), Block.Paragraph(plain("three")))
      )
    }

    test("headings by rank, closing hashes dropped; #word is not one") {
      Markdown.parse("# Title\n### Minor ##\n#hashtag") ==> Doc(
        Vector(
          Block.Heading(Level.of(1), plain("Title")),
          Block.Heading(Level.of(3), plain("Minor")),
          Block.Paragraph(plain("#hashtag"))
        )
      )
    }

    test("strong, emphasis, both, and code, which nothing inside is marked in") {
      Markdown.parse("a **b** *c* ***d*** `*e*` __f__ _g_") ==> Doc(
        Vector(
          para(
            marked("a "),
            marked("b", Mark.Strong),
            marked(" "),
            marked("c", Mark.Emphasis),
            marked(" "),
            marked("d", Mark.Strong, Mark.Emphasis),
            marked(" "),
            marked("*e*", Mark.Code),
            marked(" "),
            marked("f", Mark.Strong),
            marked(" "),
            marked("g", Mark.Emphasis)
          )
        )
      )
    }

    test("emphasis nests inside strong") {
      Markdown.parse("**a *b* c**") ==> Doc(
        Vector(
          para(
            marked("a ", Mark.Strong),
            marked("b", Mark.Strong, Mark.Emphasis),
            marked(" c", Mark.Strong)
          )
        )
      )
    }

    test("what cannot open is literal: arithmetic, snake_case, an unclosed marker, escapes") {
      Markdown.parse("2 * 3 * 4") ==> Doc(Vector(Block.Paragraph(plain("2 * 3 * 4"))))
      Markdown.parse("snake_case_name") ==> Doc(Vector(Block.Paragraph(plain("snake_case_name"))))
      Markdown.parse("a **b") ==> Doc(Vector(Block.Paragraph(plain("a **b"))))
      Markdown.parse("""\*not\* \`code\`""") ==> Doc(Vector(Block.Paragraph(plain("*not* `code`"))))
    }

    test("links, images and autolinks, marked with where they go") {
      Markdown.parse(
        "see [the *docs*](https://x.io \"t\"), ![logo](l.png), <https://y.io>"
      ) ==> Doc(
        Vector(
          para(
            marked("see "),
            marked("the ", Mark.Link("https://x.io")),
            marked("docs", Mark.Link("https://x.io"), Mark.Emphasis),
            marked(", "),
            marked("logo", Mark.Link("l.png")),
            marked(", "),
            marked("https://y.io", Mark.Link("https://y.io"))
          )
        )
      )
      Markdown.parse("[not a link] and [x]") ==> Doc(
        Vector(Block.Paragraph(plain("[not a link] and [x]")))
      )
    }

    test("fenced code keeps its lines exactly, and its language; ~~~ fences too") {
      Markdown.parse("```py\n  x = 1\n\n*y*\n```\n~~~\nz\n~~~") ==> Doc(
        Vector(
          Block.Code(Some("py"), Vector("  x = 1", "", "*y*")),
          Block.Code(None, Vector("z"))
        )
      )
    }

    test("an unclosed fence runs to the end") {
      Markdown.parse("```\na\nb") ==> Doc(Vector(Block.Code(None, Vector("a", "b"))))
    }

    test("bullets and numbers, nested by indentation, and a lazy continuation") {
      Markdown.parse("- a\n  - b\n    - c\n- d\ncontinued\n\n3. x\n4. y") ==> Doc(
        Vector(
          Block.Bullets(
            Vector(
              Item(
                Vector(
                  Block.Paragraph(plain("a")),
                  Block.Bullets(
                    Vector(
                      Item(
                        Vector(
                          Block.Paragraph(plain("b")),
                          Block.Bullets(Vector(Item(Vector(Block.Paragraph(plain("c"))))))
                        )
                      )
                    )
                  )
                )
              ),
              Item(Vector(Block.Paragraph(plain("d\ncontinued"))))
            )
          ),
          Block.Numbered(
            3,
            Vector(
              Item(Vector(Block.Paragraph(plain("x")))),
              Item(Vector(Block.Paragraph(plain("y"))))
            )
          )
        )
      )
    }

    test("a nested list under a number may be indented less than the number's text") {
      kinds(Markdown.parse("1. a\n  - b").blocks) ==> Vector("ol", "li", "p", "ul", "li", "p")
    }

    test("a year does not start a list in the middle of a paragraph") {
      Markdown.parse("It was\n2024. A year.") ==> Doc(
        Vector(Block.Paragraph(plain("It was\n2024. A year.")))
      )
    }

    test("quotes hold blocks; rules are three of - * or _; a rule is not a bullet") {
      Markdown.parse("> # q\n> text\n\n---\n* * *\n___") ==> Doc(
        Vector(
          Block.Quote(
            Vector(Block.Heading(Level.of(1), plain("q")), Block.Paragraph(plain("text")))
          ),
          Block.Rule,
          Block.Rule,
          Block.Rule
        )
      )
    }

    test("a pipe table: header, rows, escaped pipes, marks in cells") {
      Markdown.parse("| a | b |\n|:--|--:|\n| `x` | y \\| z |\n| only |") ==> Doc(
        Vector(
          Block.Table(
            Vector(plain("a"), plain("b")),
            Vector(
              Vector(Text(Vector(marked("x", Mark.Code))), plain("y | z")),
              Vector(plain("only"))
            )
          )
        )
      )
    }

    test("the sample reads as every construct, in order") {
      kinds(Markdown.parse(Sample).blocks) ==> Vector(
        "p",
        "h1",
        "ol",
        "li",
        "p",
        "li",
        "p",
        "ul",
        "li",
        "p",
        "li",
        "p",
        "quote",
        "p",
        "code",
        "table",
        "hr",
        "p"
      )
    }

    test("any text at all is some prose: no input throws") {
      val nasty = Vector(
        "",
        "\n",
        "*",
        "**",
        "***",
        "`",
        "``",
        "```",
        "[",
        "](",
        "[a](",
        "![",
        "<",
        "<http://",
        "\\",
        "# ",
        "#",
        "-",
        "- ",
        "1.",
        "1. ",
        ">",
        "> >",
        "|",
        "|-|",
        "| a |\n|-",
        "\t- x",
        "_ _ _",
        "*a **b* c**",
        "[a](b(c)",
        "````\n```",
        "\r\n\r\n",
        "  \n  ",
        "1)"
      )
      for (s <- nasty) {
        Markdown.parse(s)
        Markdown.parsePrefix(s)
      }
    }

    test("a reply streaming in: each prefix's shape is a prefix of the whole reply's") {
      val whole = kinds(Markdown.parse(Sample).blocks)
      val bad = (0 to Sample.length).flatMap { k =>
        val shape = kinds(Markdown.parsePrefix(Sample.take(k)).blocks)
        Option.when(whole.take(shape.length) != shape)(s"at $k: $shape")
      }
      bad ==> Vector.empty
    }

    test("a reply streaming in never shows a markdown marker the whole reply hides") {
      // The sample's shown text has no * _ # or backtick of its own, so any in a prefix
      // is a marker that leaked before its meaning was known.
      assert(!shown(Markdown.parse(Sample).blocks).exists("*_#`".contains(_)))
      val leaks = (0 to Sample.length).flatMap { k =>
        val text = shown(Markdown.parsePrefix(Sample.take(k)).blocks)
        Option.when(text.exists("*_#`".contains(_)))(s"at $k: $text")
      }
      leaks ==> Vector.empty
    }

    test("a streaming unclosed span runs to the end, and a trailing marker is held") {
      Markdown.parsePrefix("a **bo") ==> Doc(Vector(para(marked("a "), marked("bo", Mark.Strong))))
      Markdown.parsePrefix("a **") ==> Doc(Vector(para(marked("a "))))
      Markdown.parsePrefix("a `co") ==> Doc(Vector(para(marked("a "), marked("co", Mark.Code))))
      Markdown.parsePrefix("[docs](https://ex") ==> Doc(Vector(para(marked("docs"))))
    }

    test("a streaming line that may yet become a marker is held until it has") {
      Markdown.parsePrefix("a\n#") ==> Markdown.parsePrefix("a\n")
      Markdown.parsePrefix("a\n1") ==> Markdown.parsePrefix("a\n")
      Markdown.parsePrefix("a\n``") ==> Markdown.parsePrefix("a\n")
      Markdown.parsePrefix("a\n```sca") ==> Markdown.parsePrefix("a\n")
      kinds(Markdown.parsePrefix("a\n```scala\n").blocks) ==> Vector("p", "code")
    }

    test("a streaming table is held until its delimiter row arrives, then a row at a time") {
      Markdown.parsePrefix("| a | b |\n") ==> Doc.empty
      Markdown.parsePrefix("| a | b |\n|--|-") ==> Doc.empty
      Markdown.parsePrefix("| a | b |\n|--|--|\n| x") ==> Doc(
        Vector(Block.Table(Vector(plain("a"), plain("b")), Vector.empty))
      )
    }

    test("a whole reply and its last prefix agree") {
      Markdown.parsePrefix(Sample) ==> Markdown.parse(Sample)
    }
  }
}
