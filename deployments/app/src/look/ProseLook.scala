package grit.app.look

import grit.prose.form.{Block as Prose, Doc, Item, Mark, Renderer, Text as Inline}
import grit.prose.markdown.Markdown
import grit.tui.model.block.{Block, Overflow}
import grit.tui.model.surface.Style
import grit.tui.model.text.{StyledText, Width}

/** A reply in `look`'s colours, as transcript blocks: the terminal's output mode for
  * prose. grit's rune leads the first line, and the whole reply hangs in one column after
  * it. Nothing here knows the pane's width: prose wraps where the pane is, and code and
  * tables, which must not reflow, are cut there instead (the copy is whole). Colour and
  * weight only, as everywhere in [[Look]]: strong is bold, emphasis is grit's colour, and
  * code sits on the well.
  *
  * What stands beside the words -- the rune, the reply's indent, a quote's rail, the
  * indent a list item's rows hang at -- is each block's lead and hang, never its text, so
  * a copy is what was written. A list's bullets and numbers are its text: they are the
  * list.
  */
final case class ProseLook(look: Look) extends Renderer[Vector[Block]] {
  import ProseLook.*

  private val theme = look.theme

  /** What goes before a reply's first line: the rail and grit's rune. */
  val lead: StyledText =
    StyledText.styled("▌", Style.fg(theme.grit)) ++
      StyledText.styled(s"${Look.Runes.Grit} ", Style.fg(theme.grit) + Style.Bold)

  /** What goes before every other line of a reply: blank, as wide as [[lead]], so the
    * reply reads as one column after the rune.
    */
  val indent: StyledText = StyledText(" " * Width.of(lead.text))

  /** A whole reply, from its markdown. */
  def reply(markdown: String): Vector[Block] = render(Markdown.parse(markdown))

  /** A reply still arriving, from as much of its markdown as has come, with `caret` after
    * its last character. Rendered as the whole reply is, so the reply does not change
    * layout when it lands; the rest can only extend what is shown ([[Markdown.parsePrefix]]).
    */
  def streaming(markdown: String, caret: StyledText): Vector[Block] = {
    val blocks = render(Markdown.parsePrefix(markdown))
    blocks.lastOption match {
      case Some(t: Block.Text) => blocks.dropRight(1) :+ t.append(caret)
      case _ => blocks :+ Block.styled(caret).beside(indent, indent)
    }
  }

  def render(doc: Doc): Vector[Block] =
    if (doc.isEmpty) Vector(Block.Text("").beside(lead, indent))
    else {
      val out = Vector.newBuilder[Block]
      var i = 0
      while (i < doc.blocks.length) {
        if (i > 0) out += Block.Text("")
        out ++= block(doc.blocks(i), if (i == 0) lead else indent, indent, 0)
        i += 1
      }
      out.result()
    }

  private def body: Style = Style.fg(theme.ink)

  private def faint: Style = Style.fg(theme.faint)

  /** `b` as blocks: its first row after `first`, every other row after `rest` -- the
    * reply's indent and the rails and indents of the lists and quotes it sits in. `depth`
    * is how many lists deep.
    */
  private def block(b: Prose, first: StyledText, rest: StyledText, depth: Int): Vector[Block] =
    b match {
      case Prose.Paragraph(_) | Prose.Heading(_, _) =>
        Vector(Block.styled(words(b)).beside(first, rest))
      case Prose.Bullets(items) =>
        list(items, _ => Bullets(depth % Bullets.length), first, rest, depth)
      case Prose.Numbered(start, items) =>
        val wide = (start + items.length - 1).toString.length + 1
        list(items, k => s"${start + k}.".padTo(wide, ' '), first, rest, depth)
      case Prose.Quote(blocks) =>
        val rail = StyledText.styled("▎ ", faint)
        if (blocks.isEmpty) Vector(Block.Text("").beside(first ++ rail, rest ++ rail))
        else {
          val out = Vector.newBuilder[Block]
          var j = 0
          while (j < blocks.length) {
            if (j > 0) out += Block.Text("").beside(rest ++ rail, rest ++ rail)
            out ++= block(blocks(j), (if (j == 0) first else rest) ++ rail, rest ++ rail, depth)
            j += 1
          }
          out.result()
        }
      case Prose.Code(language, lines) =>
        // A cell of the well before the code, so it does not sit hard against the rail.
        val margin = rest ++ " "
        val label = language.toVector.map(l =>
          Block
            .styled(StyledText.styled(l, faint))
            .beside(margin, margin)
            .copy(ground = Style.bg(theme.well), overflow = Overflow.Truncate)
        )
        val shown = (if (lines.isEmpty) Vector("") else lines).map(_.replace("\t", "    "))
        own(first, rest) ++ label :+ Block
          .styled(StyledText.styled(shown.mkString("\n"), body))
          .beside(margin, margin)
          .copy(ground = Style.bg(theme.well), overflow = Overflow.Truncate)
      case Prose.Table(header, rows) => own(first, rest) :+ table(header, rows, rest ++ " ")
      case Prose.Rule =>
        own(first, rest) :+ Block.Separator(Style.fg(theme.rail), '─', lead = rest)
    }

  /** A list's items, each marked by `marker(k)`, its rows hanging after the marker. The
    * marker leads the item's first paragraph; an item that opens with anything else has
    * its marker on a row of its own.
    */
  private def list(
      items: Vector[Item],
      marker: Int -> String,
      first: StyledText,
      rest: StyledText,
      depth: Int
  ): Vector[Block] = {
    val out = Vector.newBuilder[Block]
    var k = 0
    while (k < items.length) {
      val m = s"${marker(k)} "
      val mark = StyledText.styled(m, Style.fg(theme.grit))
      val under = rest ++ (" " * Width.of(m))
      val lead = if (k == 0) first else rest
      val blocks = items(k).blocks
      val opened = blocks.headOption match {
        case Some(p @ (Prose.Paragraph(_) | Prose.Heading(_, _))) =>
          out += Block.styled(mark ++ words(p)).beside(lead, under)
          1
        case _ =>
          out += Block.styled(mark).beside(lead, under)
          0
      }
      var j = opened
      while (j < blocks.length) {
        out ++= block(blocks(j), under, under, depth + 1)
        j += 1
      }
      k += 1
    }
    out.result()
  }

  /** A block that cannot carry a row's lead -- code, a table, a rule -- gets the lead on
    * a row of its own, when there is anything but blank in it to show.
    */
  private def own(first: StyledText, rest: StyledText): Vector[Block] =
    if (first.text.trim.isEmpty) Vector.empty else Vector(Block.Text("").beside(first, rest))

  /** A table's columns padded to their widest cell, the header in bold, cut rather than
    * wrapped at the pane's edge, after `margin` on every row.
    */
  private def table(
      header: Vector[Inline],
      rows: Vector[Vector[Inline]],
      margin: StyledText
  ): Block = {
    val cols = (header.length +: rows.map(_.length)).maxOption.getOrElse(0)
    def cells(row: Vector[Inline], style: Style): Vector[StyledText] =
      (0 until cols).toVector.map(c => row.lift(c).fold(StyledText.empty)(t => phrase(t, style)))
    val head = cells(header, body + Style.Bold)
    val body1 = rows.map(cells(_, body))
    val widths = (0 until cols).toVector.map(c =>
      (head +: body1).map(r => r.lift(c).fold(0)(t => Width.of(t.text))).maxOption.getOrElse(0)
    )
    def line(row: Vector[StyledText]): StyledText = {
      var acc = StyledText.empty
      var c = 0
      while (c < cols) {
        val cell = row.lift(c).getOrElse(StyledText.empty)
        if (c > 0) acc = acc ++ StyledText.styled(" │ ", faint)
        acc = acc ++ cell ++ (" " * math.max(0, widths.lift(c).getOrElse(0) - Width.of(cell.text)))
        c += 1
      }
      acc
    }
    val rule = StyledText.styled(widths.map(w => "─" * w).mkString("─┼─"), faint)
    Block
      .styled(StyledText.join("\n", (line(head) +: rule +: body1.map(line))))
      .beside(margin, margin)
      .copy(overflow = Overflow.Truncate)
  }

  /** A paragraph's or a heading's words, in their styles; nothing for any other block. */
  private def words(b: Prose): StyledText = b match {
    case Prose.Paragraph(t) => phrase(t, body)
    case Prose.Heading(level, t) =>
      phrase(t, if (level.value <= 2) Style.fg(theme.grit) + Style.Bold else body + Style.Bold)
    case _ => StyledText.empty
  }

  /** `t` in `base`. A link's text is followed by where it goes, faint, unless the text is
    * the address.
    */
  private def phrase(t: Inline, base: Style): StyledText = {
    var acc = StyledText.empty
    var linked = ""
    var i = 0
    while (i < t.spans.length) {
      val span = t.spans(i)
      acc = acc ++ StyledText.styled(span.text, marked(base, span.marks))
      val href = hrefOf(span.marks)
      val before = if (i > 0) t.spans.lift(i - 1).flatMap(s => hrefOf(s.marks)) else None
      linked =
        if (href.isEmpty) "" else if (href == before) linked + span.text else span.text
      val next = t.spans.lift(i + 1).flatMap(s => hrefOf(s.marks))
      href match {
        case Some(h) if next != href && linked != h =>
          acc = acc ++ StyledText.styled(s" ($h)", faint)
        case _ => ()
      }
      i += 1
    }
    acc
  }

  /** `base` with `marks` on it: weight for strong, grit's colour for emphasis (never the
    * terminal's italic), the well under code.
    */
  private def marked(base: Style, marks: Set[Mark]): Style = {
    val strong = if (marks.contains(Mark.Strong)) base + Style.Bold else base
    val emphasis = if (marks.contains(Mark.Emphasis)) strong + Style.fg(theme.grit) else strong
    if (marks.contains(Mark.Code)) emphasis + Style.bg(theme.well) else emphasis
  }
}

object ProseLook {

  /** A bullet for each depth of nesting, in turn. */
  val Bullets: Vector[String] = Vector("•", "◦", "▪")

  private def hrefOf(marks: Set[Mark]): Option[String] =
    marks.collectFirst { case Mark.Link(h) => h }
}
