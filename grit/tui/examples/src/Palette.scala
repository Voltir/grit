package grit.tui.examples

import grit.tui.components.editor.Editor
import grit.tui.components.overlay.Popup
import grit.tui.model.block.Block
import grit.tui.model.surface.{Color, Style}
import grit.tui.model.text.StyledText

/** The demo's colours, and the vocabulary it dresses its transcript in.
  *
  * **Nothing in `grit/tui/` knows this file exists.** The library ships `Color`, a `Style`
  * that composes, and spans that survive wrapping; it ships no theme, no palette and no
  * notion that a transcript has a user turn and an assistant turn. Those are an app's,
  * and this is the app saying so -- the same position `Border` takes, where a value is
  * supplied rather than a subtype registered.
  *
  * So the taxonomy below -- user, assistant, thinking, tool, diff -- is grit's shape,
  * prototyped here. A different app with different chunks writes a different file and
  * changes nothing in the library. That is the property this file exists to demonstrate,
  * and `Transcript.scala` is the only place that consumes it.
  *
  * A cool ground with a warm accent set, dark-terminal first: the transcript separates
  * into chunks by tint rather than by rules, so the chrome stays quiet and the eye finds
  * a turn boundary without a border being drawn for it.
  */
object Palette {

  /* ---- ink ---------------------------------------------------------------------- */

  val Ink: Color = Color.hex("#c0caf5")
  val Muted: Color = Color.hex("#7a86ab")
  val Faint: Color = Color.hex("#565f89")

  /* ---- accents ------------------------------------------------------------------ */

  val Iris: Color = Color.hex("#bb9af7") // the user, and the modal's frame
  val Sky: Color = Color.hex("#7dcfff") // the assistant, and paths
  val Steel: Color = Color.hex("#7aa2f7") // chrome that is doing its job
  val Leaf: Color = Color.hex("#9ece6a") // it worked
  val Rose: Color = Color.hex("#f7768e") // it did not
  val Amber: Color = Color.hex("#e0af68") // it is still happening

  /* ---- grounds ------------------------------------------------------------------ */

  val Panel: Color = Color.hex("#24283b") // the modal
  val Behind: Color = Color.hex("#16161e") // the app under the modal
  val Slab: Color = Color.hex("#292e42") // the user's turn, and the status bar
  val ToolSlab: Color = Color.hex("#22262f") // a tool call
  val AddSlab: Color = Color.hex("#1e2b22")
  val DelSlab: Color = Color.hex("#2b1e24")
  val Rail: Color = Color.hex("#3b4261")

  /* ---- the demo's chunk vocabulary ----------------------------------------------- */

  /** The user's turn: a tinted slab, its `you>` marker in iris.
    *
    * Full-width tint is the separator, which is why this is a block `ground` and not a
    * span -- a span stops where the text does, and a short line would leave the slab
    * ragged at the right-hand end.
    */
  def user(text: String): Block.Text = {
    val marker = "you> "
    Block
      .styled(
        StyledText.styled(marker, Style.fg(Iris) + Style.Bold) ++
          StyledText.styled(text, Style.fg(Ink))
      )
      .copy(ground = Style.bg(Slab))
  }

  /** The assistant's turn: no slab at all. Bare prose between the tinted blocks is what
    * makes the tinted blocks read as punctuation.
    */
  def assistant(text: String, rev: Long = 0L): Block.Text =
    Block.styled(
      StyledText.styled("grit> ", Style.fg(Sky) + Style.Bold) ++
        StyledText.styled(text, Style.fg(Ink)),
      rev
    )

  /** Reasoning: recessive, and italic so it is distinguishable from prose without a
    * marker of its own. grit.tui has no name for this; the demo does.
    */
  def thinking(text: String): Block.Text =
    Block.styled(StyledText.styled("· " + text, Style.fg(Faint) + Style.Italic))

  /** The rule before a submitted prompt. */
  def separator: Block.Separator = Block.Separator(Style.fg(Rail) + Style.Dim)

  /** A tool call. The spinner is amber while it runs, the mark green or rose when it
    * lands, the path sky, the note and the result recessive -- and the whole block sits
    * on its own slab, a shade off the user's.
    */
  val tool: Block.ToolStyle = Block.ToolStyle(
    running = Style.fg(Amber) + Style.Bold,
    ok = Style.fg(Leaf) + Style.Bold,
    failed = Style.fg(Rose) + Style.Bold,
    name = Style.fg(Ink) + Style.Bold,
    detail = Style.fg(Sky),
    note = Style.fg(Faint),
    result = Style.fg(Muted),
    ground = Style.bg(ToolSlab)
  )

  /** A diff. The sign is content, so it is coloured rather than replaced. */
  val diff: Block.DiffStyle = Block.DiffStyle(
    added = Style.fg(Leaf) + Style.bg(AddSlab),
    removed = Style.fg(Rose) + Style.bg(DelSlab),
    context = Style.fg(Faint),
    ground = Style.bg(ToolSlab)
  )

  /* ---- chrome -------------------------------------------------------------------- */

  /** The header, whose ground *is* the streaming indicator: steel while the transcript
    * streams, amber while it is paused. One colour carrying one piece of state, which
    * is the thing a monochrome grid could not do at all.
    */
  def header(streaming: Boolean): Style =
    Style.fg(Color.hex("#1a1b26")) + Style.Bold +
      Style.bg(if (streaming) Steel else Amber)

  val status: Style = Style.fg(Color.hex("#a9b1d6")) + Style.bg(Slab)

  val scrollRail: Style = Style.fg(Rail)
  val scrollThumb: Style = Style.fg(Steel)

  val promptChrome: Style = Style.fg(Sky)
  val promptTitle: Style = Style.fg(Ink) + Style.Bold
  val promptBody: Style = Style.fg(Ink)

  val modalPanel: Style = Style.bg(Panel)
  val modalChrome: Style = Style.fg(Iris)
  val modalBehind: Style = Style.bg(Behind) + Style.Dim

  val popupItem: Style = Style.fg(Ink) + Style.bg(Panel)
  val popupSelected: Style = Style.fg(Color.hex("#1a1b26")) + Style.Bold + Style.bg(Sky)
  val popupChrome: Style = Style.fg(Steel) + Style.bg(Panel)

  /* ---- dressed components ---------------------------------------------------- */

  // The three styles above are never named outside this file, and neither are the ones
  // above them: a component that needs dressing is *built* here rather than dressed at
  // its call site, which is the difference between a palette and a list of colours the
  // app has to remember to apply.

  /** The prompt: an empty draft, focused, dressed. */
  def prompt: Editor =
    Editor(
      "",
      0,
      focused = true,
      title = "prompt",
      body = promptBody,
      chrome = promptChrome,
      titleStyle = promptTitle
    )

  /** A completion list over `items`, dressed. */
  def popup(items: Vector[String]): Popup =
    Popup(items, item = popupItem, selectedStyle = popupSelected, chrome = popupChrome)

  /** The modal's own document, which is prose and not a transcript. */
  val helpText: Style = Style.fg(Ink)
}
