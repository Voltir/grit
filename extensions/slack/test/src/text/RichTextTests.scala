package grit.slack.text

import scala.compiletime.constValue
import scala.deriving.Mirror

import grit.prose.form.{Block, Cell, Column, Doc, Item, Level, Mark, Span, Text}
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

  private def para(t: String): Block[Nothing] = Block.Paragraph(Text.plain(t))

  private def only(doc: Doc[Nothing]): ujson.Arr =
    RichText.render(doc) match {
      case Vector(post) => post.blocks
      case other => throw new java.lang.AssertionError(s"${other.size} posts")
    }

  /** How many cases the sum type `T` has, counted by the compiler. */
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
      case Vector(Block.Table(_, _)) => ("columns aligned", _.contains("rich_text_preformatted"))
      case Vector(Block.Rule) => ("divider", _.contains("\"divider\""))
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

    test("a table is preformatted text, its columns aligned, each cell as it prints") {
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
                "name | n                   \n-----+---------------------\na    | 10.0                \nlong | =B1 (not calculated)"
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
