package grit.app.look

import grit.prose.form.{Block as Prose, Doc, Item, Mark, Renderer, Text as Inline}
import grit.prose.markdown.Markdown
import grit.tui.model.block.{Block, Overflow}
import grit.tui.model.surface.Style
import grit.tui.model.text.{StyledText, Width}

/** A reply in `look`'s colours, as transcript blocks: the terminal's output mode for
  * prose. grit's rune leads the first line. Nothing here knows the pane's width: prose
  * wraps where the pane is, and code and tables, which must not reflow, are cut there
  * instead (the copy is whole). Colour and weight only, as everywhere in [[Look]]:
  * strong is bold, emphasis is grit's colour, and code sits on the well.
  */
final case class ProseLook(look: Look) extends Renderer[Vector[Block]] {
  import ProseLook.*

  private val theme = look.theme

  /** What goes before a reply's first line: the rail and grit's rune. */
  val lead: StyledText =
    StyledText.styled("▌", Style.fg(theme.grit)) ++
      StyledText.styled(s"${Look.Runes.Grit} ", Style.fg(theme.grit) + Style.Bold)

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
      case _ => blocks :+ Block.styled(caret)
    }
  }

  def render(doc: Doc): Vector[Block] =
    if (doc.isEmpty) Vector(Block.styled(lead))
    else {
      val out = Vector.newBuilder[Block]
      var i = 0
      while (i < doc.blocks.length) {
        if (i > 0) out += Block.Text("")
        out ++= block(doc.blocks(i), if (i == 0) lead else StyledText.empty, StyledText.empty, 0)
        i += 1
      }
      out.result()
    }

  private def body: Style = Style.fg(theme.ink)

  private def faint: Style = Style.fg(theme.faint)

  /** `b` as blocks: its first line after `first`, every other line after `rest` -- the
    * indent and rails of the lists and quotes it sits in. `depth` is how many lists deep.
    */
  private def block(b: Prose, first: StyledText, rest: StyledText, depth: Int): Vector[Block] =
    b match {
      case Prose.Paragraph(t) => Vector(Block.styled(words(t, body, first, rest)))
      case Prose.Heading(level, t) =>
        val style =
          if (level.value <= 2) Style.fg(theme.grit) + Style.Bold else body + Style.Bold
        Vector(Block.styled(words(t, style, first, rest)))
      case Prose.Bullets(items) =>
        list(items, _ => Bullets(depth % Bullets.length), first, rest, depth)
      case Prose.Numbered(start, items) =>
        val wide = (start + items.length - 1).toString.length + 1
        list(items, k => s"${start + k}.".padTo(wide, ' '), first, rest, depth)
      case Prose.Quote(blocks) =>
        val rail = StyledText.styled("▎ ", faint)
        if (blocks.isEmpty) Vector(Block.styled(first ++ rail))
        else {
          val out = Vector.newBuilder[Block]
          var j = 0
          while (j < blocks.length) {
            if (j > 0) out += Block.styled(rest ++ rail)
            out ++= block(blocks(j), (if (j == 0) first else rest) ++ rail, rest ++ rail, depth)
            j += 1
          }
          out.result()
        }
      case Prose.Code(language, lines) =>
        val label = language.map(l => rest ++ StyledText.styled(s" $l", faint)).toVector
        val shown = (if (lines.isEmpty) Vector("") else lines).map(l =>
          rest ++ StyledText.styled(s" ${l.replace("\t", "    ")}", body)
        )
        own(first) :+ Block
          .styled(StyledText.join("\n", label ++ shown))
          .copy(ground = Style.bg(theme.well), overflow = Overflow.Truncate)
      case Prose.Table(header, rows) => own(first) :+ table(header, rows, rest)
      case Prose.Rule => own(first) :+ Block.Separator(Style.fg(theme.rail), '─')
    }

  /** A list's items, each marked by `marker(k)`, its lines after the marker's width. */
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
      val pad = StyledText(" " * Width.of(m))
      val lead = (if (k == 0) first else rest) ++ mark
      val blocks = items(k).blocks
      if (blocks.isEmpty) out += Block.styled(lead)
      var j = 0
      while (j < blocks.length) {
        out ++= block(blocks(j), if (j == 0) lead else rest ++ pad, rest ++ pad, depth + 1)
        j += 1
      }
      k += 1
    }
    out.result()
  }

  /** A block that cannot carry a line's lead -- code, a table, a rule -- gets the lead on
    * a row of its own, when there is one to show.
    */
  private def own(first: StyledText): Vector[Block] =
    if (first.text.trim.isEmpty) Vector.empty else Vector(Block.styled(first))

  /** A table's columns padded to their widest cell, the header in bold, cut rather than
    * wrapped at the pane's edge.
    */
  private def table(
      header: Vector[Inline],
      rows: Vector[Vector[Inline]],
      rest: StyledText
  ): Block = {
    val cols = (header.length +: rows.map(_.length)).maxOption.getOrElse(0)
    def cells(row: Vector[Inline], style: Style): Vector[StyledText] =
      (0 until cols).toVector.map(c =>
        row.lift(c).fold(StyledText.empty)(t => words(t, style, StyledText.empty, StyledText.empty))
      )
    val head = cells(header, body + Style.Bold)
    val body1 = rows.map(cells(_, body))
    val widths = (0 until cols).toVector.map(c =>
      (head +: body1).map(r => r.lift(c).fold(0)(t => Width.of(t.text))).maxOption.getOrElse(0)
    )
    def line(row: Vector[StyledText]): StyledText = {
      var acc = rest ++ " "
      var c = 0
      while (c < cols) {
        val cell = row.lift(c).getOrElse(StyledText.empty)
        if (c > 0) acc = acc ++ StyledText.styled(" │ ", faint)
        acc = acc ++ cell ++ (" " * math.max(0, widths.lift(c).getOrElse(0) - Width.of(cell.text)))
        c += 1
      }
      acc
    }
    val rule = rest ++ StyledText.styled(
      " " + widths.map(w => "─" * w).mkString("─┼─"),
      faint
    )
    Block
      .styled(StyledText.join("\n", (line(head) +: rule +: body1.map(line))))
      .copy(
        overflow = Overflow.Truncate
      )
  }

  /** `t` in `base`, after `first`, each line break followed by `rest`. A link's text is
    * followed by where it goes, faint, unless the text is the address.
    */
  private def words(t: Inline, base: Style, first: StyledText, rest: StyledText): StyledText = {
    var acc = first
    var linked = ""
    var i = 0
    while (i < t.spans.length) {
      val span = t.spans(i)
      val style = marked(base, span.marks)
      val pieces = span.text.split("\n", -1)
      var j = 0
      while (j < pieces.length) {
        if (j > 0) acc = acc ++ "\n" ++ rest
        acc = acc ++ StyledText.styled(pieces(j), style)
        j += 1
      }
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
