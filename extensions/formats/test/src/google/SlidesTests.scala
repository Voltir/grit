package grit.formats.google

import java.nio.charset.StandardCharsets.UTF_8

import grit.core.files.{CoverUnit, Covers, Kind, Limit, Locus, Missed, ReadLimits, Unread}
import grit.formats.choice.Fixtures
import grit.prose.form.{Block, Cell, Column, Doc, Item, Level, Mark, Span, Text}

import utest.*

/** [[Slides]] over decks written here in the shape the Slides API's `presentations.get` answers
  * in: what each part of a slide is read as, and what is said to be missed.
  */
object SlidesTests extends TestSuite {

  // The API's answer, part by part.
  private def run(s: String, style: ujson.Obj = ujson.Obj()): ujson.Value =
    ujson.Obj("textRun" -> ujson.Obj("content" -> s, "style" -> style))
  private def marker(bullet: Option[ujson.Obj] = None): ujson.Value =
    ujson.Obj.from(Seq("style" -> ujson.Obj()) ++ bullet.map("bullet" -> _))
  private def para(runs: ujson.Value*): Vector[ujson.Value] =
    Vector(ujson.Obj("paragraphMarker" -> marker())) ++ runs
  private def line(s: String): Vector[ujson.Value] = para(run(s + "\n"))
  private def bullet(level: Int, glyph: String, s: String): Vector[ujson.Value] = {
    val b = ujson.Obj("listId" -> "list1", "glyph" -> glyph)
    if (level > 0) b("nestingLevel") = level
    Vector(ujson.Obj("paragraphMarker" -> marker(Some(b))), run(s + "\n"))
  }
  private def text(elements: Vector[ujson.Value]): ujson.Value =
    ujson.Obj("textElements" -> ujson.Arr.from(elements))
  private def shape(id: String, placeholder: Option[String], elements: Vector[ujson.Value]) =
    ujson.Obj(
      "objectId" -> id,
      "shape" -> ujson.Obj.from(
        Seq("text" -> text(elements)) ++
          placeholder.map(t => "placeholder" -> ujson.Obj("type" -> t))
      )
    )
  private def title(s: String): ujson.Value = shape("title", Some("TITLE"), line(s))
  private def body(elements: Vector[ujson.Value]): ujson.Value = shape("body", None, elements)
  private def slide(
      elements: Vector[ujson.Value],
      notes: Vector[ujson.Value] = Vector.empty,
      skipped: Boolean = false,
      layout: String = "layout-body"
  ): ujson.Value = {
    val props = ujson.Obj(
      "layoutObjectId" -> layout,
      "notesPage" -> ujson.Obj(
        "notesProperties" -> ujson.Obj("speakerNotesObjectId" -> "notes"),
        "pageElements" -> ujson.Arr(
          shape("notes-slide-image", Some("SLIDE_IMAGE"), Vector.empty),
          shape("notes", Some("BODY"), notes)
        )
      )
    )
    if (skipped) props("isSkipped") = true
    ujson.Obj(
      "objectId" -> "slide",
      "slideProperties" -> props,
      "pageElements" -> ujson.Arr.from(elements)
    )
  }
  private def deck(slides: ujson.Value*): ujson.Value =
    ujson.Obj(
      "presentationId" -> "deck1",
      "title" -> "Plans",
      "layouts" -> ujson.Arr(
        ujson.Obj(
          "objectId" -> "layout-body",
          "layoutProperties" -> ujson.Obj("name" -> "TITLE_AND_BODY")
        ),
        ujson.Obj(
          "objectId" -> "layout-section",
          "layoutProperties" -> ujson.Obj("name" -> "SECTION_HEADER")
        ),
        ujson.Obj(
          "objectId" -> "layout-section-described",
          "layoutProperties" -> ujson.Obj("name" -> "SECTION_TITLE_AND_DESCRIPTION")
        )
      ),
      "slides" -> ujson.Arr.from(slides)
    )

  private def file(json: ujson.Value): java.nio.file.Path =
    Fixtures.file("deck.json", ujson.write(json).getBytes(UTF_8).toVector)
  private def blocks(slides: ujson.Value*): Either[Unread, Vector[Block[Nothing]]] =
    Fixtures.blocks(Fixtures.read(Slides, file(deck(slides*))))

  private def plain(s: String): Text[Nothing] = Text.plain(s)
  private def p(s: String): Block[Nothing] = Block.Paragraph(plain(s))
  private def heading(s: String): Block[Nothing] = Block.Heading(Level.of(1), plain(s))

  val tests = Tests {
    test(
      "each slide is a section headed by its title, else 'Slide n', every span on its slide's page"
    ) {
      val read = Fixtures.read(
        Slides,
        file(
          deck(
            slide(Vector(title("Roadmap"), body(line("Ship it.")))),
            slide(Vector(body(line("Loose words."))))
          )
        )
      )
      val pages = read.map(_.doc.blocks.map(b => Doc(Vector(b)).notes.distinct))
      (Fixtures.blocks(read), pages) ==> (
        Right(Vector(heading("Roadmap"), p("Ship it."), heading("Slide 2"), p("Loose words."))),
        Right(
          Vector(
            Vector(Locus.Page(1)),
            Vector(Locus.Page(1)),
            Vector(Locus.Page(2)),
            Vector(Locus.Page(2))
          )
        )
      )
    }

    test(
      "the title leads, then the subtitle, then the rest in the answer's order, a group's where it stands"
    ) {
      val group = ujson.Obj(
        "objectId" -> "group",
        "elementGroup" -> ujson.Obj(
          "children" -> ujson.Arr(body(line("Inside.")), body(line("Also inside.")))
        )
      )
      blocks(
        slide(
          Vector(
            body(line("Before.")),
            shape("sub", Some("SUBTITLE"), line("The plan")),
            group,
            title("Roadmap"),
            shape("number", Some("SLIDE_NUMBER"), line("1")),
            body(line("After."))
          )
        )
      ) ==> Right(
        Vector(
          heading("Roadmap"),
          p("The plan"),
          p("Before."),
          p("Inside."),
          p("Also inside."),
          p("After.")
        )
      )
    }

    test(
      "bold, italic and a link are marks; a soft break a line break; a link to a slide its text"
    ) {
      val styled = para(
        run("Bold", ujson.Obj("bold" -> true)),
        run(" and "),
        run("leaning", ujson.Obj("italic" -> true)),
        run("\u000bsee "),
        run("the site", ujson.Obj("link" -> ujson.Obj("url" -> "https://example.com/plan"))),
        run(" or "),
        run("slide 3", ujson.Obj("link" -> ujson.Obj("slideIndex" -> 2))),
        run(".\n")
      )
      blocks(slide(Vector(body(styled)))) ==> Right(
        Vector(
          heading("Slide 1"),
          Block.Paragraph(
            Text(
              Vector(
                Span("Bold", Set(Mark.Strong)),
                Span.plain(" and "),
                Span("leaning", Set(Mark.Emphasis)),
                Span.plain("\nsee "),
                Span("the site", Set(Mark.Link("https://example.com/plan"))),
                Span.plain(" or slide 3.")
              )
            )
          )
        )
      )
    }

    test("a list's items nest by level, numbered from the first glyph's number, else bulleted") {
      val items =
        bullet(0, "3.", "Three") ++ bullet(1, "●", "Sub") ++ bullet(0, "4.", "Four") ++ line(
          "After."
        )
      blocks(slide(Vector(body(items)))) ==> Right(
        Vector(
          heading("Slide 1"),
          Block.Numbered(
            3,
            Vector(
              Item(Vector(p("Three"), Block.Bullets(Vector(Item(Vector(p("Sub"))))))),
              Item(Vector(p("Four")))
            )
          ),
          p("After.")
        )
      )
    }

    test("a table's first row names its columns, a position a merged cell covers empty") {
      def cell(s: String, columns: Int = 1) =
        ujson.Obj("rowSpan" -> 1, "columnSpan" -> columns, "text" -> text(line(s)))
      val table = ujson.Obj(
        "objectId" -> "table",
        "table" -> ujson.Obj(
          "rows" -> 2,
          "columns" -> 3,
          "tableRows" -> ujson.Arr(
            ujson.Obj("tableCells" -> ujson.Arr(cell("Quarter"), cell("Plan", 2))),
            ujson.Obj("tableCells" -> ujson.Arr(cell("Q3"), cell("Hire"), cell("Ship")))
          )
        )
      )
      blocks(slide(Vector(table))) ==> Right(
        Vector(
          heading("Slide 1"),
          Block.table(
            Vector(Column(plain("Quarter")), Column(plain("Plan")), Column(Text.empty)),
            Vector(
              Vector(Cell.Words(plain("Q3")), Cell.Words(plain("Hire")), Cell.Words(plain("Ship")))
            )
          )
        )
      )
    }

    test(
      "speaker notes are a quote led by 'Speaker notes' at the slide's end; empty notes are nothing"
    ) {
      blocks(
        slide(Vector(title("Roadmap"), body(line("Ship it."))), notes = line("Stress the date.")),
        slide(Vector(title("Next")), notes = Vector.empty)
      ) ==> Right(
        Vector(
          heading("Roadmap"),
          p("Ship it."),
          Block.Quote(
            Vector(
              Block.Paragraph(Text(Vector(Span("Speaker notes", Set(Mark.Strong))))),
              p("Stress the date.")
            )
          ),
          heading("Next")
        )
      )
    }

    test(
      "slides after a section header slide are nested under it, up to the next; those before it, and a deck with none, stay flat"
    ) {
      def at(level: Int, s: String) = Block.Heading(Level.of(level), plain(s))
      (
        blocks(
          slide(Vector(title("Welcome"))),
          slide(Vector(title("Plans")), layout = "layout-section"),
          slide(Vector(title("Hiring"))),
          slide(Vector(title("Results")), layout = "layout-section-described"),
          slide(Vector(title("Revenue")))
        ),
        blocks(slide(Vector(title("One"))), slide(Vector(title("Two"))))
      ) ==> (
        Right(
          Vector(
            at(1, "Welcome"),
            at(1, "Plans"),
            at(2, "Hiring"),
            at(1, "Results"),
            at(2, "Revenue")
          )
        ),
        Right(Vector(at(1, "One"), at(1, "Two")))
      )
    }

    test("a skipped slide says it is skipped in the slideshow") {
      blocks(slide(Vector(title("Old plan"), body(line("Dropped."))), skipped = true)) ==> Right(
        Vector(
          heading("Old plan"),
          Block.Paragraph(Text(Vector(Span("Skipped in the slideshow.", Set(Mark.Emphasis))))),
          p("Dropped.")
        )
      )
    }

    test(
      "alt text is read; images and charts without it, and videos, are missed; lines are dropped"
    ) {
      def image(id: String, alt: Option[(String, String)]) =
        ujson.Obj.from(
          Seq(
            "objectId" -> ujson.Str(id),
            "image" -> ujson.Obj("sourceUrl" -> "https://example.com/i.png")
          ) ++
            alt.toSeq.flatMap((t, d) => Seq("title" -> ujson.Str(t), "description" -> ujson.Str(d)))
        )
      val read = Fixtures.read(
        Slides,
        file(
          deck(
            slide(
              Vector(
                image("i1", Some(("Revenue", "bars rising each quarter"))),
                image("i2", None),
                image("i3", None),
                ujson.Obj("objectId" -> "c1", "sheetsChart" -> ujson.Obj("chartId" -> 7)),
                ujson.Obj("objectId" -> "v1", "video" -> ujson.Obj("id" -> "abc")),
                ujson.Obj("objectId" -> "w1", "wordArt" -> ujson.Obj("renderedText" -> "LAUNCH")),
                ujson.Obj("objectId" -> "l1", "line" -> ujson.Obj("lineCategory" -> "STRAIGHT"))
              )
            )
          )
        )
      )
      (Fixtures.blocks(read), read.map(_.missed)) ==> (
        Right(
          Vector(
            heading("Slide 1"),
            Block.Paragraph(
              Text(Vector(Span("Image: Revenue — bars rising each quarter", Set(Mark.Emphasis))))
            ),
            p("LAUNCH")
          )
        ),
        Right(
          Vector(
            Missed("2 images without alt text"),
            Missed("a chart from a Sheet"),
            Missed("a video")
          )
        )
      )
    }

    test("JSON that is not a deck is not this format's; JSON that does not parse is damaged") {
      (
        Fixtures.read(Slides, file(ujson.Obj("kind" -> "drive#file", "id" -> "f1"))),
        Fixtures.read(
          Slides,
          Fixtures.file(
            "deck.json",
            """{"presentationId": "p1", "slides": [""".getBytes(UTF_8).toVector
          )
        )
      ) ==> (Left(Unread.NotMine), Left(Unread.Damaged(Kind.GoogleSlides)))
    }

    test("a read past the characters stops there, covering the slides it finished") {
      val read = Fixtures.read(
        Slides,
        file(
          deck(
            slide(Vector(title("One"), body(line("Short.")))),
            slide(Vector(body(line("x" * 50))))
          )
        ),
        ReadLimits.lowered(chars = 20)
      )
      (Fixtures.blocks(read), read.map(_.covers)) ==> (
        Right(Vector(heading("One"), p("Short."))),
        Right(Covers.Part(1, 2, CoverUnit.Pages, Limit.Chars))
      )
    }
  }
}
