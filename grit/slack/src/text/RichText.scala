package grit.slack.text

import grit.prose.form.{Block, Doc, Item, Mark, Renderer, Span, Text}

/** One Slack message of a reply: its rich_text and divider blocks, and the plain text Slack
  * shows where blocks are not shown (a notification), escaped so it reads as written. Only
  * [[RichText]] makes one, so none is past Slack's limits.
  */
final case class Post private[text] (blocks: ujson.Arr, fallback: String)

/** A reply's prose as Slack messages. Rich text rather than Slack's `markdown_text`: a rich
  * text element is literal, so nothing the model writes becomes a mention or a broadcast
  * (whether `markdown_text` parses `<!channel>` is undocumented).
  */
object RichText extends Renderer[Vector[Post]] {

  /** 50: Slack refuses a message with more blocks (`msg_blocks_too_many`). */
  val MaxBlocks = 50

  /** 3,000: the character limit of a section's text, taken for rich text, whose own limit
    * Slack's reference does not state.
    */
  val MaxChars = 3000

  /** `doc` as Slack messages, split at block boundaries (a code block longer than one post at
    * its line boundaries, any other block at spaces) into posts of at most [[MaxBlocks]]
    * blocks and [[MaxChars]] characters of text; a heading as bold text, a rule as a divider,
    * a table as preformatted aligned text, a code block's language dropped, a list or quote
    * inside a list item or a quote flattened to its text. None for an empty doc.
    */
  def render(doc: Doc): Vector[Post] =
    posts(doc.blocks.flatMap(block).flatMap(fit))

  /** `text` with `&`, `<` and `>` written as Slack's mrkdwn escapes them, so a message's
    * plain text reads as written: nothing in it becomes a mention, a broadcast or a link.
    */
  private def escaped(text: String): String =
    text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

  /** A Slack block and its plain text, which is what counts against [[MaxChars]]. */
  private final case class Piece(json: ujson.Value, plain: String)

  private def posts(pieces: Vector[Piece]): Vector[Post] = {
    // Built in scoped locals: nested vectors folded in a lambda trip capture checking.
    val done = Vector.newBuilder[Post]
    var run = List.empty[Piece] // reversed
    var size = 0
    def flush(): Unit = if (run.nonEmpty) {
      val ps = run.reverse
      done += Post(
        ujson.Arr.from(ps.map(_.json)),
        escaped(ps.map(_.plain).filter(_.nonEmpty).mkString("\n"))
      )
      run = Nil
      size = 0
    }
    pieces.foreach { piece =>
      if (run.size >= MaxBlocks || size + piece.plain.length > MaxChars) flush()
      run = piece :: run
      size += piece.plain.length
    }
    flush()
    done.result()
  }

  /** `piece` split into pieces of at most [[MaxChars]]: a code block at its lines (a line
    * longer than that at the limit), anything else as plain text at spaces.
    */
  private def fit(piece: Piece): Vector[Piece] =
    if (piece.plain.length <= MaxChars) Vector(piece)
    else if (isPreformatted(piece.json))
      chunks(piece.plain.split("\n", -1).toVector.flatMap(_.grouped(MaxChars)), "\n")
        .map(t => Piece(rich(preformatted(t)), t))
    else
      chunks(piece.plain.split(" ", -1).toVector.flatMap(_.grouped(MaxChars)), " ")
        .map(t => Piece(rich(section(Vector(plain(t)))), t))

  /** `parts` joined by `sep` into as few runs of at most [[MaxChars]] as keep them whole. */
  private def chunks(parts: Vector[String], sep: String): Vector[String] = {
    val done = Vector.newBuilder[String]
    var run: Option[String] = None
    parts.foreach { part =>
      run match {
        case Some(last) if last.length + sep.length + part.length <= MaxChars =>
          run = Some(s"$last$sep$part")
        case Some(last) =>
          done += last
          run = Some(part)
        case None => run = Some(part)
      }
    }
    run.foreach(done += _)
    done.result()
  }

  private def isPreformatted(json: ujson.Value): Boolean =
    json.objOpt
      .flatMap(_.get("elements"))
      .flatMap(_.arrOpt)
      .exists(
        _.exists(
          _.objOpt.flatMap(_.get("type")).flatMap(_.strOpt).contains("rich_text_preformatted")
        )
      )

  private def block(b: Block): Vector[Piece] = b match {
    case Block.Paragraph(t) => Vector(Piece(rich(section(spans(t))), t.plain))
    case Block.Heading(_, t) => Vector(Piece(rich(section(spans(t, bold = true))), t.plain))
    case Block.Rule => Vector(Piece(ujson.Obj("type" -> "divider"), ""))
    case Block.Code(_, lines) =>
      val t = lines.mkString("\n")
      Vector(Piece(rich(preformatted(t)), t))
    case Block.Quote(blocks) =>
      val els = flat(blocks)
      Vector(
        Piece(
          rich(ujson.Obj("type" -> "rich_text_quote", "elements" -> ujson.Arr.from(els))),
          plainOf(blocks)
        )
      )
    case Block.Bullets(items) =>
      Vector(
        Piece(rich(lists("bullet", 0, items, 0)*), items.map(i => plainOf(i.blocks)).mkString("\n"))
      )
    case Block.Numbered(start, items) =>
      Vector(
        Piece(
          rich(lists("ordered", start - 1, items, 0)*),
          items.map(i => plainOf(i.blocks)).mkString("\n")
        )
      )
    case Block.Table(header, rows) =>
      val t = table(header, rows)
      Vector(Piece(rich(preformatted(t)), t))
  }

  /** A list's items as Slack list elements: one list per run of items, each nested list a list
    * at `indent` + 1 between them, an ordered list's `offset` counting on across the runs.
    */
  private def lists(
      style: String,
      offset: Int,
      items: Vector[Item],
      indent: Int
  ): Vector[ujson.Obj] = {
    def list(at: Int, sections: Vector[ujson.Value]): ujson.Obj =
      ujson.Obj(
        "type" -> "rich_text_list",
        "style" -> style,
        "indent" -> indent,
        "offset" -> at,
        "elements" -> ujson.Arr.from(sections)
      )
    val done = Vector.newBuilder[ujson.Obj]
    var run = Vector.empty[ujson.Value]
    var at = offset
    items.foreach { item =>
      val nested: Vector[ujson.Obj] = item.blocks.flatMap {
        case Block.Bullets(is) => lists("bullet", 0, is, indent + 1)
        case Block.Numbered(s, is) => lists("ordered", s - 1, is, indent + 1)
        case _ => Vector.empty
      }
      run = run :+ section(flat(item.blocks.filter {
        case Block.Bullets(_) | Block.Numbered(_, _) => false
        case _ => true
      }))
      if (nested.nonEmpty) {
        done += list(at, run)
        done ++= nested
        at += run.size
        run = Vector.empty
      }
    }
    if (run.nonEmpty) done += list(at, run)
    done.result()
  }

  /** `blocks` as one run of inline elements, a line break between blocks: what a list item or
    * a quote may hold.
    */
  private def flat(blocks: Vector[Block]): Vector[ujson.Value] =
    merge(blocks.zipWithIndex.flatMap { (b, i) =>
      val sep = if (i == 0) Vector.empty else Vector(plain("\n"))
      sep ++ (b match {
        case Block.Paragraph(t) => spans(t)
        case Block.Heading(_, t) => spans(t, bold = true)
        case Block.Code(_, lines) => Vector(styled(lines.mkString("\n"), Set(Mark.Code)))
        case other => Vector(plain(plainOf(Vector(other))))
      })
    })

  private def plainOf(blocks: Vector[Block]): String =
    blocks
      .map {
        case Block.Paragraph(t) => t.plain
        case Block.Heading(_, t) => t.plain
        case Block.Code(_, lines) => lines.mkString("\n")
        case Block.Quote(bs) => plainOf(bs)
        case Block.Bullets(items) => items.map(i => plainOf(i.blocks)).mkString("\n")
        case Block.Numbered(_, items) => items.map(i => plainOf(i.blocks)).mkString("\n")
        case Block.Table(h, rows) => table(h, rows)
        case Block.Rule => ""
      }
      .mkString("\n")

  private def spans(t: Text, bold: Boolean = false): Vector[ujson.Value] =
    merge(t.spans.map { case Span(text, marks) =>
      val all = if (bold) marks + Mark.Strong else marks
      all.collectFirst { case Mark.Link(href) => href } match {
        case Some(href) =>
          val o = ujson.Obj("type" -> "link", "url" -> href, "text" -> text)
          style(all).foreach(s => o("style") = s)
          o
        case None => styled(text, all)
      }
    })

  private def styled(text: String, marks: Set[Mark]): ujson.Obj = {
    val o = ujson.Obj("type" -> "text", "text" -> text)
    style(marks).foreach(s => o("style") = s)
    o
  }

  private def plain(text: String): ujson.Obj = styled(text, Set.empty)

  private def style(marks: Set[Mark]): Option[ujson.Obj] = {
    val on = marks.toVector.collect {
      case Mark.Strong => "bold"
      case Mark.Emphasis => "italic"
      case Mark.Code => "code"
    }
    Option.when(on.nonEmpty)(ujson.Obj.from(on.distinct.map(_ -> ujson.Bool(true))))
  }

  /** Adjacent text elements of one style as one. */
  private def merge(els: Vector[ujson.Value]): Vector[ujson.Value] =
    els.foldLeft(Vector.empty[ujson.Value]) { (done, el) =>
      (done.lastOption, el) match {
        case (Some(last), next)
            if isText(last) && isText(next) && last.obj.get("style") == next.obj.get("style") =>
          val joined = ujson.Obj("type" -> "text", "text" -> (last("text").str + next("text").str))
          last.obj.get("style").foreach(s => joined("style") = s)
          done.init :+ joined
        case _ => done :+ el
      }
    }

  private def isText(v: ujson.Value): Boolean =
    v.objOpt.flatMap(_.get("type")).flatMap(_.strOpt).contains("text")

  private def rich(elements: ujson.Value*): ujson.Obj =
    ujson.Obj("type" -> "rich_text", "elements" -> ujson.Arr(elements*))

  private def section(elements: Vector[ujson.Value]): ujson.Obj =
    ujson.Obj("type" -> "rich_text_section", "elements" -> ujson.Arr.from(elements))

  private def preformatted(t: String): ujson.Obj =
    ujson.Obj("type" -> "rich_text_preformatted", "elements" -> ujson.Arr(plain(t)))

  /** `header` and `rows` as aligned text: cells padded to their column's width, split by
    * ` | `, the header ruled off with `-` and `+`.
    */
  private def table(header: Vector[Text], rows: Vector[Vector[Text]]): String = {
    val all = (header +: rows).map(_.map(_.plain))
    val columns = all.map(_.size).maxOption.getOrElse(0)
    val cells = all.map(r => r.padTo(columns, ""))
    val widths = (0 until columns).toVector.map(c => cells.map(_(c).length).max)
    def line(r: Vector[String]): String =
      r.zip(widths).map((c, w) => c.padTo(w, ' ')).mkString(" | ")
    (line(cells.head) +: widths.map("-" * _).mkString("-+-") +: cells.tail.map(line)).mkString("\n")
  }
}
