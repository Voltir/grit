package grit.tui.model.block

import scala.annotation.unused

import grit.tui.model.surface.Style
import grit.tui.model.text.{Span, StyledText}

/** How a block's overlong lines meet the pane width.
  *
  * `Wrap` lays every column out across as many rows as it takes. `Truncate` keeps one
  * row per logical line and cuts at the right edge in display columns -- never mid-glyph
  * -- and is display-only: the logical text, and so what a selection copies, is whole.
  * The modal is where the truncated tail stays reachable.
  */
enum Overflow {
  case Wrap
  case Truncate
}

/** Where a tool call is in its life. Plain data, like everything here: the spinner is
  * the tick counter, the outcome is a flag and a note, and nothing carries a callback.
  */
enum ToolState {
  case Running(tick: Long)
  case Done(ok: Boolean, note: String)
}

/** One transcript block: a group of logical rows that measures itself into text, to be
  * wrapped at projection time.
  *
  * The rows a block contributes are its [[text]] joined by newlines, and a `DocPos`
  * offset indexes that string exactly as it indexed a plain entry -- which is why the
  * whole selection model survives blocks without changing. A block that needs its
  * continuation rows indented composes the indent into the text (a tool result under
  * its `●` summary), so what is painted and what is selected never diverge.
  *
  * A block is the unit `DocPos.entry` indexes, so a block that grows rows -- a tool call
  * expanding -- moves no sibling position and invalidates no sibling wrap-cache key.
  */
sealed trait Block {

  /** The logical, unwrapped text of this block: its rows joined by newlines. */
  def text: String

  /** Styles attached to ranges of [[text]], in its own unwrapped coordinates.
    *
    * Empty by default: a block is plain unless it says otherwise, and grit.tui never says
    * otherwise on an app's behalf. What a `+` line or a failed tool call should *look*
    * like is the app's vocabulary, supplied as the style parameters below -- grit.tui knows
    * only where the pieces begin and end, which is the part an app cannot compute
    * without re-deriving the string.
    */
  def spans: Vector[Span] = Vector.empty

  /** The style laid under this block's whole rows, edge to edge.
    *
    * Distinct from a span covering [[text]]: a span stops where the text does, and a
    * block that wants to read as a tinted slab -- the user's turn, a tool call -- has to
    * carry its ground across the pane's full width, including the blank right-hand end
    * of a short line. Spans layer over it.
    */
  def ground: Style = Style.plain

  /** The ground for the row that holds `offset`, for a block whose rows do not all
    * share one -- a diff, where the tint *is* which lines changed.
    *
    * Defaults to [[ground]], so a block with a single ground says nothing here. The
    * argument is an offset into [[text]] rather than a row number because a block does
    * not know the width it will be wrapped at, and the offset is the coordinate it
    * already indexes by.
    */
  def groundAt(@unused offset: Int): Style = ground

  /** The horizontal-overflow strategy for this block's lines. */
  def overflow: Overflow = Overflow.Wrap
}

object Block {

  /** Braille dot patterns, the conventional terminal spinner -- the same cycle the
    * status-bar `Spinner` widget uses. Duplicated rather than imported because the
    * transcript model must not reach the widget layer for ten characters.
    */
  val frames: Vector[Char] = Vector('⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏')

  /** The glyph for `tick` -- safe for any value, including negative. */
  def glyph(tick: Long): Char = frames(math.floorMod(tick, frames.length).toInt)

  /** Plain text. The streaming case: [[Text.append]] grows the trailing entry, so exactly
    * one entry's wrapped rows are re-paid.
    */
  final case class Text(
      override val text: String,
      override val spans: Vector[Span] = Vector.empty,
      override val ground: Style = Style.plain
  ) extends Block {

    /** This block with `more` appended, unstyled. Existing spans keep their offsets --
      * appending never disturbs what came before it, which is what makes the streaming
      * tail safe to re-wrap alone.
      */
    def append(more: String): Text = copy(text = text + more)

    /** This block with `more` appended, its spans shifted past the existing text. */
    def append(more: StyledText): Text = {
      val joined = StyledText(text, spans) ++ more
      copy(text = joined.text, spans = joined.spans)
    }
  }

  /** The rule painted before every submitted prompt. Logically empty: it is a rule,
    * not content, so it copies as an empty line and is painted to the pane's width at
    * projection time.
    */
  final case class Separator(override val ground: Style = Style.plain) extends Block {
    override val text: String = ""
  }

  /** A diff. Lines arrive with their own `+`/`-`/space prefixes -- that is content, and
    * copying a diff without them would be wrong.
    */
  final case class Diff(
      lines: Vector[String],
      override val overflow: Overflow = Overflow.Wrap,
      style: DiffStyle = DiffStyle.plain
  ) extends Block {

    override def text: String = lines.mkString("\n")

    override def ground: Style = style.ground

    /** The tint of the line `offset` falls in: on a diff the ground is per line, because
      * which lines changed is exactly what the tint says. A line's ground runs the full
      * width of the row, so a short `-` line is a full-width stripe rather than a stripe
      * that stops where the text does.
      */
    override def groundAt(offset: Int): Style = {
      var base = 0
      var i = 0
      var found = style.ground
      while (i < lines.length && base <= offset) {
        val line = lines(i)
        if (offset >= base && offset <= base + line.length) { found = style.of(line) }
        base += line.length + 1
        i += 1
      }
      found
    }

    /** One span per line, chosen by the line's own prefix.
      *
      * grit.tui knows a diff line starting with `+` is an addition; it does not know that
      * an addition is green. [[DiffStyle]] is where the app says so, and its default
      * says nothing at all.
      */
    override def spans: Vector[Span] = {
      if (
        style.added == Style.plain && style.removed == Style.plain &&
        style.context == Style.plain
      ) { Vector.empty }
      else {
        val out = Vector.newBuilder[Span]
        var base = 0
        var i = 0
        while (i < lines.length) {
          val line = lines(i)
          val st = style.of(line)
          if (st != Style.plain && line.nonEmpty) out += Span(base, base + line.length, st)
          base += line.length + 1 // the '\n' the join adds
          i += 1
        }
        out.result()
      }
    }
  }

  /** How an app dresses the three kinds of diff line. Plain by default: a diff that was
    * never given colours paints exactly as it did before colours existed.
    */
  final case class DiffStyle(
      added: Style = Style.plain,
      removed: Style = Style.plain,
      context: Style = Style.plain,
      ground: Style = Style.plain
  ) {

    /** The style for one line, by its prefix. */
    def of(line: String): Style =
      if (line.startsWith("+")) added
      else if (line.startsWith("-")) removed
      else context
  }

  object DiffStyle {
    val plain: DiffStyle = DiffStyle()
  }

  /** A tool call: the affordance that separates a coding-agent transcript from a chat
    * log. Collapsed it is one summary line -- spinner while running, status glyph and
    * counts when done -- and expanded it is the summary plus its result, indented under
    * it. Expansion re-wraps *this* block only.
    */
  final case class Tool(
      name: String,
      detail: String,
      state: ToolState,
      expanded: Boolean = false,
      result: Vector[String] = Vector.empty,
      style: ToolStyle = ToolStyle.plain
  ) extends Block {

    /** The block's rows and their styles, built together.
      *
      * One builder produces both, which is the whole reason this is not two methods:
      * the offsets a span needs are the ones the string concatenation already knows,
      * and re-deriving them by searching the finished string is how a span ends up one
      * character off from the glyph it was meant to cover.
      */
    private lazy val content: StyledText = {
      val (mark, markStyle) = state match {
        case ToolState.Running(tick) => (Block.glyph(tick).toString, style.running)
        case ToolState.Done(ok, _) => (if (ok) "✓" else "✗", if (ok) style.ok else style.failed)
      }
      val note = state match {
        case ToolState.Done(_, n) => n
        case _ => ""
      }
      val summary = StyledText.of(
        StyledText.styled(mark, markStyle),
        StyledText(" "),
        StyledText.styled(name, style.name),
        StyledText("("),
        StyledText.styled(detail, style.detail),
        StyledText(")")
      )
      val withNote =
        if (note.isEmpty) summary
        else summary ++ StyledText(" · ") ++ StyledText.styled(note, style.note)
      val whole =
        if (expanded && result.nonEmpty) {
          // The indent is composed into the text, not painted around it, so what is
          // shown and what a selection copies stay the same string.
          val body = result.map(l => "  " + l).mkString("\n")
          withNote ++ StyledText("\n") ++ StyledText.styled(body, style.result)
        } else { withNote }
      whole
    }

    override def text: String = content.text

    override def spans: Vector[Span] = content.spans

    override def ground: Style = style.ground

    /** The next spinner frame. A finished tool has nothing to tick; this is that tool. */
    def tick: Tool = state match {
      case ToolState.Running(t) => copy(state = ToolState.Running(t + 1))
      case _ => this
    }

    /** The tool returned -- success or error value, both final -- with its full result,
      * which is what expansion shows.
      */
    def finish(ok: Boolean, note: String, result: Vector[String] = Vector.empty): Tool =
      copy(state = ToolState.Done(ok, note), result = result)

    /** This tool, expanded or collapsed. */
    def withExpanded(e: Boolean): Tool =
      if (e == expanded) this else copy(expanded = e)

    /** This tool dressed by `s`. */
    def styled(s: ToolStyle): Tool = copy(style = s)
  }

  /** How an app dresses the pieces of a tool call. Plain by default, like everything
    * else here: grit.tui locates the spinner, the name, the path and the note; what any of
    * them *means* is the app's to say.
    */
  final case class ToolStyle(
      running: Style = Style.plain,
      ok: Style = Style.plain,
      failed: Style = Style.plain,
      name: Style = Style.plain,
      detail: Style = Style.plain,
      note: Style = Style.plain,
      result: Style = Style.plain,
      ground: Style = Style.plain
  )

  object ToolStyle {
    val plain: ToolStyle = ToolStyle()
  }

  /** A tool call that has just been issued, spinner at `tick`. */
  def tool(name: String, detail: String, tick: Long = 0L): Tool =
    Tool(name, detail, ToolState.Running(tick))

  /** A block of styled text -- the general constructor, and the one an app reaches for
    * when it has a vocabulary grit.tui does not share. Everything a transcript wants that
    * is not a diff or a tool call is built from this.
    */
  def styled(content: StyledText): Text =
    Text(content.text, content.spans)
}
