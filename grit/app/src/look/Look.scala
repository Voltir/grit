package grit.app.look

import grit.tui.components.editor.Editor
import grit.tui.components.layout.Border
import grit.tui.components.overlay.{Modal, Popup}
import grit.tui.components.view.View
import grit.tui.model.block.Block
import grit.tui.model.surface.{Size, Style, Surface}
import grit.tui.model.text.{StyledText, Width}
import grit.turn.Turn.Step

/** The chat screen's styles, from `theme`. Colour and weight only, never the terminal's
  * dim or italic: terminals draw those as they like, and a meaning carried by them can
  * vanish. Who is speaking is a rune as well as a colour ([[Look.Runes]]), so it reads
  * without the colour.
  */
final case class Look(theme: Theme) {
  import Look.Runes

  /** Under the whole screen, so no cell is left to the terminal's own colours. */
  def ground: Style = Style.bg(theme.ground) + Style.fg(theme.ink)

  /** The user's message on the slab, after their rune, its rail running down every row
    * the message takes. The rune and the rail stand beside the text: a copy is what was
    * sent.
    */
  def user(text: String): Block.Text =
    Block
      .styled(StyledText.styled(text, Style.fg(theme.ink) + Style.Bold))
      .beside(
        StyledText.styled("▌", Style.fg(theme.user)) ++
          StyledText.styled(s"${Runes.User} ", Style.fg(theme.user) + Style.Bold),
        StyledText.styled("▌", Style.fg(theme.user)) ++ "  "
      )
      .copy(ground = Style.bg(theme.slab))

  /** A reply, from its markdown: one block per paragraph, list item, listing and so on. */
  def assistant(text: String): Vector[Block] = ProseLook(this).reply(text)

  /** A reply as it streams, from as much of its markdown as has come ([[ProseLook]]),
    * with a caret that blinks with `tick`.
    */
  def streaming(text: String, tick: Long): Vector[Block] =
    ProseLook(this).streaming(
      text,
      StyledText.styled(if (tick % 8 < 5) "▍" else " ", Style.fg(theme.grit))
    )

  /** The line under the transcript while a turn runs: a rune of the Futhark for each
    * `tick`, in turn.
    */
  def thinking(tick: Long): Block.Text =
    Block.styled(
      StyledText.styled(s"  ${Runes.futhark(tick)} ", Style.fg(theme.user) + Style.Bold) ++
        StyledText.styled("grit is thinking…", Style.fg(theme.faint))
    )

  /** The line while something slow is under way, `what`: four runes turning about a
    * still centre, one step per `tick`.
    */
  def ward(tick: Long, what: String): Block.Text = {
    val ring = Runes.ward(tick)
    Block.styled(
      StyledText("  ") ++
        StyledText.styled(ring(0).toString, Style.fg(theme.faint)) ++
        StyledText.styled(ring(1).toString, Style.fg(theme.user)) ++
        StyledText.styled(Runes.Idle, Style.fg(theme.grit) + Style.Bold) ++
        StyledText.styled(ring(2).toString, Style.fg(theme.user)) ++
        StyledText.styled(ring(3).toString, Style.fg(theme.faint)) ++
        StyledText.styled(s" $what", Style.fg(theme.faint))
    )
  }

  /** One line of a turn's tool loop, faint after Tiwaz: its first line only. */
  def tool(text: String): Block.Text =
    Block.styled(
      StyledText.styled(s"  ${Runes.Tool} ", Style.fg(theme.faint) + Style.Bold) ++
        StyledText.styled(text.linesIterator.nextOption().getOrElse(""), Style.fg(theme.faint))
    )

  /** The line while the model is writing a call to the tool `name`. */
  def calling(name: String): Block.Text = tool(s"calling $name…")

  def failure(reason: String): Block.Text =
    Block.styled(
      StyledText.styled(s"${Runes.Failure} ", Style.fg(theme.failure) + Style.Bold) ++
        StyledText.styled(reason, Style.fg(theme.failure))
    )

  def separator: Block.Separator = Block.Separator(Style.fg(theme.rail), '━', Runes.Turn)

  /** The top bar: grit's name in runes on the accent, then `title`, then `session`,
    * faint, when there is one.
    */
  def header(title: String, session: String = ""): View = Look.Header(title, theme, session)

  def status: Style = Style.fg(theme.statusFg) + Style.bg(theme.statusBg) + Style.Bold

  /** A dialog titled `title` over the screen: lit on the slab, the screen behind it
    * pushed back to the rail's colour (never the terminal's dim). One that `fit`s is as
    * tall as its body, up to the most any dialog takes.
    */
  def modal(title: String, fit: Boolean = false): Modal =
    Modal(
      title,
      rows = 24,
      cols = 60,
      panel = modalGround,
      chrome = Style.fg(theme.grit) + Style.Bold,
      behind = Style.fg(theme.rail),
      fit = fit
    )

  /** Under a dialog's body. */
  def modalGround: Style = Style.bg(theme.slab) + Style.fg(theme.ink)

  /** Under the turn panel: the slab, so the panel reads as its own column without a
    * rule beside the transcript's scrollbar.
    */
  def sidebar: Style = Style.bg(theme.slab) + Style.fg(theme.ink)

  def scrollRail: Style = Style.fg(theme.rail)
  def scrollThumb: Style = Style.fg(theme.thumb)

  /** The prompt: two thin rules, the draft between them after the thorn. */
  def prompt: Editor =
    styled(Editor("", 0, focused = true, border = Look.Rules, gutter = s"${Runes.Prompt} "))

  /** `editor`, draft and history kept, in this look's colours. */
  def styled(editor: Editor): Editor =
    editor.copy(
      body = Style.fg(theme.ink),
      chrome = Style.fg(theme.faint),
      gutterStyle = Style.fg(theme.user) + Style.Bold
    )

  /** The command palette over the prompt: `items` on the slab, framed in grit's colour,
    * the selection lit as the header is.
    */
  def palette(items: Vector[String], query: String, selected: Int): Popup =
    Popup(
      items,
      query,
      selected,
      maxRows = 8,
      maxCols = 44,
      item = Style.bg(theme.slab) + Style.fg(theme.ink),
      selectedStyle = Style.bg(theme.headerBg) + Style.fg(theme.headerFg) + Style.Bold,
      chrome = Style.bg(theme.slab) + Style.fg(theme.grit)
    )

  /** A heading in a dialog. */
  def heading(text: String): Block.Text =
    Block.styled(StyledText.styled(s" $text", Style.fg(theme.faint) + Style.Bold))

  /** A dialog's row: `key` (a command, or a key to press) and what it does. */
  def binding(key: String, what: String): Block.Text =
    Block.styled(
      StyledText.styled(s" ${key.padTo(Look.KeyCols, ' ')} ", Style.fg(theme.grit) + Style.Bold) ++
        StyledText.styled(what, Style.fg(theme.ink))
    )

  /** A turn's summary, under its reply: faint, after Laguz (the summarising rune) set
    * under grit's, its wrapped rows hung under its text; its whitespace, line breaks
    * included, run together.
    */
  def summary(text: String): Block.Text =
    Block
      .styled(StyledText.styled(text.trim.split("\\s+").mkString(" "), Style.fg(theme.faint)))
      .beside(StyledText.styled(" ᛚ ", Style.fg(theme.faint) + Style.Bold), StyledText("   "))
}

object Look {

  /** The width of a dialog's key column ([[Look.binding]]). */
  val KeyCols = 23

  /** Rules above and below, edge to edge, and a blank cell for each side. */
  val Rules: Border = Border('─', '─', '─', '─', '─', ' ')

  /** The Elder Futhark runes grit draws with, each chosen for its old meaning. */
  object Runes {

    /** grit, written in Elder Futhark. */
    val Name = "ᚷᚱᛁᛏ"

    /** Mannaz: humankind. The user's marker. */
    val User = "ᛗ"

    /** Thurisaz: the thorn, pointed like `>`. The prompt's marker. */
    val Prompt = "ᚦ"

    /** Ansuz: the god's voice. grit's marker. */
    val Grit = "ᚨ"

    /** Hagalaz: hail, disruption. A failure. */
    val Failure = "ᚺ"

    /** Isa: ice, stillness. Nothing in flight. */
    val Idle = "ᛁ"

    /** The runic cross: the mark between turns. */
    val Turn = "᛭"

    /** The Elder Futhark in its order: the thinking spinner's frames. */
    val Futhark: Vector[Char] = "ᚠᚢᚦᚨᚱᚲᚷᚹᚺᚾᛁᛃᛇᛈᛉᛊᛏᛒᛖᛗᛚᛜᛞᛟ".toVector

    /** The frame for `tick`, safe for any value. */
    def futhark(tick: Long): Char = Futhark(math.floorMod(tick, Futhark.length).toInt)

    /** A turn step's rune and what it does, by the step's name ([[Step]]): Othala,
      * the inheritance, while memory is assembled; Ansuz while the model answers; Jera,
      * the harvest, while what came back is recorded; Laguz, the flow, while the turn is
      * summarised; Tiwaz while a tool runs. A tool loop's step is known by its family
      * (`call-model:2` answers). A name grit does not know shows as itself.
      */
    def step(name: String): String = Step.family(name).getOrElse(name) match {
      case Step.Classify => "ᛈ placing"
      case Step.Assemble => "ᛟ assembling"
      case Step.CallModel | Step.CallModelAgain | Step.CallModelPlain => "ᚨ answering"
      case Step.RecordTopic | Step.RecordWindow | Step.RecordVerdict | Step.RecordCall |
          Step.Append | Step.AppendSummary =>
        "ᛃ recording"
      case Step.Tool => s"$Tool using a tool"
      case Step.Summarise => "ᛚ summarising"
      case other => s"$Idle $other"
    }

    /** Tiwaz: the god who keeps his word. A turn's use of a tool. */
    val Tool = "ᛏ"

    /** The rune alone, as [[step]] chooses it. */
    def stepRune(name: String): String = step(name).takeWhile(_ != ' ')

    /** Algiz, Othala, Dagaz and Ansuz: the ward's ring. */
    val Ward: Vector[Char] = "ᛉᛟᛞᚨ".toVector

    /** The ring turned `tick` steps. */
    def ward(tick: Long): Vector[Char] =
      Ward.indices.toVector.map(i => Ward(math.floorMod(i + tick, Ward.length).toInt))
  }

  /** The top bar: the name on the accent, fading into the title on the slab, and the
    * session after it, faint.
    */
  final case class Header(title: String, theme: Theme, session: String = "") extends View {

    def measure(avail: Size): Size = Size(math.min(1, avail.rows), avail.cols)

    def render(size: Size): Surface =
      if (size.rows < 1) Surface.blank(size)
      else {
        val quiet = Style.fg(theme.faint) + Style.bg(theme.slab)
        val parts = Vector(
          s" ${Runes.Name} " -> (Style.fg(theme.headerFg) + Style.bg(theme.headerBg) + Style.Bold),
          "▓▒░" -> (Style.fg(theme.headerBg) + Style.bg(theme.slab)),
          s" $title " -> (Style.fg(theme.ink) + Style.bg(theme.slab) + Style.Bold),
          (if (session.isEmpty) "" else s"· $session ") -> quiet,
          "▓▒░" -> (Style.fg(theme.slab) + Style.bg(theme.ground))
        )
        parts
          .foldLeft((Surface.blank(size), 0)) { case ((s, col), (text, style)) =>
            (s.write(0, col, text, style), col + Width.of(text))
          }
          ._1
      }
  }
}
