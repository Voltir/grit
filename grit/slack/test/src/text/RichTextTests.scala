package grit.slack.text

import grit.prose.form.{Block, Doc, Item, Level, Mark, Span, Text}

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

  private def para(t: String): Block = Block.Paragraph(Text.plain(t))

  private def only(doc: Doc): ujson.Arr =
    RichText.render(doc) match {
      case Vector(post) => post.blocks
      case other => throw new java.lang.AssertionError(s"${other.size} posts")
    }

  val tests = Tests {
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

    test("a table is preformatted text, its columns aligned") {
      only(
        Doc(
          Vector(
            Block.Table(
              Vector(Text.plain("name"), Text.plain("n")),
              Vector(
                Vector(Text.plain("a"), Text.plain("10")),
                Vector(Text.plain("long"), Text.plain("2"))
              )
            )
          )
        )
      ) ==> ujson.Arr(
        rich(
          ujson.Obj(
            "type" -> "rich_text_preformatted",
            "elements" -> ujson.Arr(text("name | n \n-----+---\na    | 10\nlong | 2 "))
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

    test("an empty reply is no post") {
      RichText.render(Doc.empty) ==> Vector.empty
    }
  }
}
