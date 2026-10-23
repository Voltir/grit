package grit.formats.google

import java.io.IOException
import java.nio.file.Files

import scala.collection.mutable

import grit.core.files.{
  CoverUnit,
  Covers,
  Format,
  FormatName,
  Kind,
  Limit,
  Missed,
  OpenFile,
  ReadLimits,
  Understood,
  Unread
}
import grit.formats.limits.Guard
import grit.formats.tree.{Blocks, Deck, Levelled}
import grit.prose.form.{Block, Cell, Mark, Span, Text}

/** Google Slides decks as the Slides API's `presentations.get` answers them: each slide a section
  * headed by its title (else "Slide n"), every span on page n, n its place among all slides;
  * body text, lists, tables and alt text in the answer's element order, never by position;
  * speaker notes as a quote led by **Speaker notes**; a skipped slide said so. A slide number,
  * date or footer placeholder, a line and a shape with no text are dropped. Images and charts
  * without alt text, and videos, are [[Missed]]. [[Unread.NotMine]] for JSON that is not a deck;
  * [[Unread.Damaged]] for JSON that does not parse. A stopped read [[Covers.Part]] in slides
  * counted as pages: the slides it finished.
  */
object Slides extends Format {

  val name: FormatName = FormatName("google-slides")
  val version: Int = 1

  /** What a slide holds that is not read, counted for [[Missed]]. */
  private enum Unseen derives CanEqual { case Image, Chart, Video }

  private val Titles = Set("TITLE", "CENTERED_TITLE")
  private val Dropped = Set("SLIDE_NUMBER", "DATE_AND_TIME", "FOOTER")

  def read(file: OpenFile^, limits: ReadLimits): Either[Unread, Understood] =
    parsed(file).flatMap { json =>
      val slides = all(json, "slides")
      if (str(json, "presentationId").isEmpty || field(json, "slides").flatMap(_.arrOpt).isEmpty)
        Left(Unread.NotMine)
      else {
        val done = mutable.ArrayBuffer.empty[Deck.Slide]
        val unseen = mutable.ArrayBuffer.empty[Unseen]
        val stopped = Guard.until(file, limits) { count =>
          slides.zipWithIndex.foreach { (s, i) =>
            val (slide, missed) = slideOf(s, i + 1, count)
            done += slide
            unseen ++= missed
          }
        }
        if (done.isEmpty && stopped.contains(Limit.Time)) Left(Unread.TooSlow)
        else if (done.isEmpty && stopped.contains(Limit.Damaged))
          Left(Unread.Damaged(Kind.GoogleSlides))
        else
          Right(
            Understood(
              Deck.doc(done.toVector),
              stopped.fold(Covers.Whole)(Covers.Part(done.size, slides.size, CoverUnit.Pages, _)),
              missed(unseen.toVector),
              name,
              version
            )
          )
      }
    }

  private def parsed(file: OpenFile^): Either[Unread, ujson.Value] =
    try Right(ujson.read(Files.readAllBytes(file.path)))
    catch {
      // ujson wraps a failure inside an array or object in a trace naming where it lay.
      case _: ujson.ParsingFailedException | _: upickle.core.TraceVisitor.TraceException |
          _: IOException =>
        Left(Unread.Damaged(Kind.GoogleSlides))
    }

  /** Slide `n`, and what it holds that is not read. */
  private def slideOf(s: ujson.Value, n: Int, count: Guard.Count^): (Deck.Slide, Vector[Unseen]) = {
    val elements = flat(all(s, "pageElements"))
    def placed(types: Set[String]): Option[ujson.Value] =
      elements.find(e => placeholder(e).exists(types.contains))
    val title = placed(Titles)
    val subtitle = placed(Set("SUBTITLE"))
    val rest = elements.filterNot(e =>
      title.contains(e) || subtitle.contains(e) || placeholder(e).exists(Dropped.contains)
    )
    def line(e: Option[ujson.Value]): Option[Text[Nothing]] =
      e.flatMap(field(_, "shape"))
        .flatMap(field(_, "text"))
        .map(t => joined(paragraphs(t, count).map(_.text)))
        .filterNot(_.isEmpty)
    val heading = line(title)
    val sub = line(subtitle)
    val read = rest.map(element(_, count))
    val props = field(s, "slideProperties")
    val notesPage = props.flatMap(field(_, "notesPage"))
    val notesId =
      notesPage.flatMap(field(_, "notesProperties")).flatMap(str(_, "speakerNotesObjectId"))
    val notes = notesPage.toVector
      .flatMap(p => flat(all(p, "pageElements")))
      .find(e => notesId.isDefined && str(e, "objectId") == notesId)
      .flatMap(field(_, "shape"))
      .flatMap(field(_, "text"))
      .fold(Vector.empty[Block[Nothing]])(t => blocks(paragraphs(t, count)))
    val slide = Deck.Slide(
      n,
      heading,
      sub,
      read.flatMap(_._1),
      notes,
      props.exists(flag(_, "isSkipped"))
    )
    (slide, read.flatMap(_._2))
  }

  /** `elements` with each group's children where the group stands. */
  private def flat(elements: Vector[ujson.Value]): Vector[ujson.Value] =
    elements.flatMap(e => field(e, "elementGroup").fold(Vector(e))(g => flat(all(g, "children"))))

  private def placeholder(e: ujson.Value): Option[String] =
    field(e, "shape").flatMap(field(_, "placeholder")).flatMap(str(_, "type"))

  /** One element's blocks, and what of it is not read. */
  private def element(
      e: ujson.Value,
      count: Guard.Count^
  ): (Vector[Block[Nothing]], Vector[Unseen]) = {
    count.step()
    val alt = Vector(str(e, "title"), str(e, "description")).flatten.map(_.trim).filter(_.nonEmpty)
    def described(what: String): Vector[Block[Nothing]] =
      if (alt.isEmpty) Vector.empty
      else
        Vector(Block.Paragraph(kept(s"$what: ${alt.mkString(" — ")}", Set(Mark.Emphasis), count)))
    def unlessDescribed(what: String, unseen: Unseen) =
      if (alt.isEmpty) (Vector.empty, Vector(unseen)) else (described(what), Vector.empty)
    if (field(e, "shape").isDefined)
      (
        field(e, "shape")
          .flatMap(field(_, "text"))
          .fold(Vector.empty)(t => blocks(paragraphs(t, count))),
        Vector.empty
      )
    else if (field(e, "table").isDefined)
      (field(e, "table").flatMap(table(_, count)).toVector, Vector.empty)
    else if (field(e, "image").isDefined) unlessDescribed("Image", Unseen.Image)
    else if (field(e, "sheetsChart").isDefined) unlessDescribed("Chart", Unseen.Chart)
    else if (field(e, "video").isDefined) (described("Video"), Vector(Unseen.Video))
    else
      field(e, "wordArt").flatMap(str(_, "renderedText")).filter(_.trim.nonEmpty) match {
        case Some(w) => (Vector(Block.Paragraph(kept(w, Set.empty, count))), Vector.empty)
        case None => (Vector.empty, Vector.empty)
      }
  }

  /** `s` under `marks`, counted against the characters read. */
  private def kept(s: String, marks: Set[Mark], count: Guard.Count^): Text[Nothing] = {
    var out = ""
    count.keep(s, k => out = k)
    Text.of(Vector(Span(out, marks)))
  }

  /** A table's rows, a position a cell spanning rows or columns covers empty. */
  private def table(t: ujson.Value, count: Guard.Count^): Option[Block[Nothing]] = {
    val rows = all(t, "tableRows").map(r => all(r, "tableCells"))
    val width = num(t, "columns").getOrElse(rows.map(_.size).maxOption.getOrElse(0))
    val covered = mutable.Set.empty[(Int, Int)]
    val grid = rows.zipWithIndex.map { (cells, r) =>
      val left = cells.iterator
      (0 until width).toVector.map { c =>
        if (covered.contains((r, c)) || !left.hasNext) Cell.Empty(): Cell[Nothing]
        else {
          val cell = left.next()
          val down = num(cell, "rowSpan").getOrElse(1).max(1)
          val across = num(cell, "columnSpan").getOrElse(1).max(1)
          for (i <- r until r + down; j <- c until c + across if (i, j) != ((r, c)))
            covered += ((i, j))
          val text =
            field(cell, "text").fold(Text.empty)(x => joined(paragraphs(x, count).map(_.text)))
          if (text.isEmpty) Cell.Empty() else Cell.Words(text)
        }
      }
    }
    Blocks.table(grid)
  }

  /** A paragraph of a shape's text: its list depth and number when it is a list's item. */
  private final case class Para(listed: Option[(Int, Option[Int])], text: Text[Nothing])

  /** A shape's or a cell's text as paragraphs, by the API's paragraph markers and line ends. */
  private def paragraphs(text: ujson.Value, count: Guard.Count^): Vector[Para] = {
    val out = mutable.ArrayBuffer.empty[Para]
    val spans = mutable.ArrayBuffer.empty[Span[Nothing]]
    var listed: Option[(Int, Option[Int])] = None
    def flush(): Unit = {
      val t = Text.of(spans.toVector)
      if (!t.isEmpty) out += Para(listed, t)
      spans.clear()
      listed = None
    }
    def add(content: String, marks: Set[Mark]): Unit =
      content.split("\n", -1).zipWithIndex.foreach { (part, i) =>
        if (i > 0) flush()
        if (part.nonEmpty) count.keep(part.replace('\u000b', '\n'), k => spans += Span(k, marks))
      }
    all(text, "textElements").foreach { e =>
      count.step()
      field(e, "paragraphMarker").foreach { m =>
        flush()
        listed = field(m, "bullet").map(b =>
          (num(b, "nestingLevel").getOrElse(0), str(b, "glyph").flatMap(numbered))
        )
      }
      field(e, "textRun").foreach(r => add(str(r, "content").getOrElse(""), marks(r)))
      field(e, "autoText").foreach(a => add(str(a, "content").getOrElse(""), Set.empty))
    }
    flush()
    out.toVector
  }

  /** A run's marks: bold, italic, and a link to a page outside the deck. */
  private def marks(run: ujson.Value): Set[Mark] = {
    val style = field(run, "style")
    def on(key: String) = style.exists(flag(_, key))
    Set(Mark.Strong).filter(_ => on("bold")) ++ Set(Mark.Emphasis).filter(_ => on("italic")) ++
      style.flatMap(field(_, "link")).flatMap(str(_, "url")).map(Mark.Link(_))
  }

  /** Paragraphs as blocks, each run of list items one list. */
  private def blocks(paras: Vector[Para]): Vector[Block[Nothing]] =
    if (paras.isEmpty) Vector.empty
    else
      paras.head.listed match {
        case None => Block.Paragraph(paras.head.text) +: blocks(paras.tail)
        case Some(_) =>
          val (items, after) = paras.span(_.listed.isDefined)
          val levelled = items.toList.flatMap(p => p.listed.map((d, n) => Levelled(d, n, p.text)))
          Blocks.list(levelled) +: blocks(after)
      }

  /** The number a list's marker shows: `3.` is 3, `c)` is 3, `iv.` is 4; `None` for a bullet. */
  private def numbered(glyph: String): Option[Int] =
    glyph.split("[^\\p{L}\\p{N}]+").filter(_.nonEmpty).lastOption.flatMap { g =>
      val lower = g.toLowerCase
      if (g.forall(_.isDigit)) g.take(9).toIntOption
      else if (lower.forall("ivxlcdm".contains(_)) && (lower.length > 1 || lower == "i"))
        Some(roman(lower))
      else if (g.length == 1 && lower.head >= 'a' && lower.head <= 'z') Some(lower.head - 'a' + 1)
      else None
    }

  private def roman(s: String): Int = {
    val value = Map('i' -> 1, 'v' -> 5, 'x' -> 10, 'l' -> 50, 'c' -> 100, 'd' -> 500, 'm' -> 1000)
    val vs = s.toVector.map(c => value.getOrElse(c, 0))
    vs.indices.map(i => if (vs.lift(i + 1).exists(_ > vs(i))) -vs(i) else vs(i)).sum
  }

  /** Paragraphs as one line, joined by a space. */
  private def joined(texts: Vector[Text[Nothing]]): Text[Nothing] =
    Text.of(
      texts.zipWithIndex.flatMap((t, i) =>
        (if (i > 0) Vector(Span.plain(" ")) else Vector.empty) ++ t.spans
      )
    )

  /** What was seen and not read, counted in words. */
  private def missed(unseen: Vector[Unseen]): Vector[Missed] = {
    def n(u: Unseen) = unseen.count(_ == u)
    def said(k: Int, one: String, many: String) =
      Option.when(k > 0)(Missed(if (k == 1) one else s"$k $many"))
    Vector(
      said(n(Unseen.Image), "an image without alt text", "images without alt text"),
      said(n(Unseen.Chart), "a chart from a Sheet", "charts from Sheets"),
      said(n(Unseen.Video), "a video", "videos")
    ).flatten
  }

  private def field(v: ujson.Value, key: String): Option[ujson.Value] =
    v.objOpt.flatMap(_.get(key))
  private def all(v: ujson.Value, key: String): Vector[ujson.Value] =
    field(v, key).flatMap(_.arrOpt).fold(Vector.empty)(_.toVector)
  private def str(v: ujson.Value, key: String): Option[String] = field(v, key).flatMap(_.strOpt)
  private def num(v: ujson.Value, key: String): Option[Int] =
    field(v, key).flatMap(_.numOpt).filter(d => d >= 0 && d <= Int.MaxValue).map(_.toInt)
  private def flag(v: ujson.Value, key: String): Boolean =
    field(v, key).flatMap(_.boolOpt).contains(true)
}
