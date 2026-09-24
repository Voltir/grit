package grit.app

import grit.tui.components.editor.Editor
import grit.tui.components.layout.*
import grit.tui.components.overlay.*
import grit.tui.components.pane.*
import grit.tui.components.widget.{ScrollPane, StatusBar}
import grit.tui.model.block.Block
import grit.tui.model.input.{Input, Key}
import grit.tui.model.select.Doc
import grit.tui.model.surface.*
import grit.tui.model.text.{StyledText, WrapCache}
import grit.tui.runtime.Effect
import grit.tui.runtime.std.*

/** grit's chat screen: a transcript, a prompt and a status line. Pure, like every
  * grit.tui app: a submission leaves as [[ChatScreen.Msg.Send]] through `Effect.ToHost`,
  * and what the engine did comes back as messages ([[ChatHost]] sends them).
  */
object ChatScreen {

  private val HeaderPane = PaneId.of("header")
  private val BodyPane = PaneId.of("body")
  private val TranscriptPane = PaneId.of("transcript")
  private val Bar = PaneId.of("scrollbar")
  private val PromptPane = PaneId.of("prompt")
  private val StatusPane = PaneId.of("status")

  private val chrome = Stack.of(
    HeaderPane -> Region.Fixed(1),
    BodyPane -> Region.Flex(5),
    PromptPane -> Region.Fit(min = 3, upTo = 0.5),
    StatusPane -> Region.Fixed(1)
  )

  private val transcript =
    ScrollPane(TranscriptPane, Bar, ChatPalette.scrollRail, ChatPalette.scrollThumb)

  /** One message of the conversation so far: the user's, or a reply. */
  final case class Said(user: Boolean, text: String)

  /** `waiting` counts turns sent and not yet answered. */
  final case class State(
      panes: Panes,
      editor: Editor,
      waiting: Int,
      status: String,
      title: String,
      std: Std.State
  )

  enum Msg {

    /** Enter in the prompt. */
    case Submit

    /** For the host: send the conversation so far. */
    case Load

    /** From the host: the conversation so far, oldest first. */
    case Loaded(said: Vector[Said])

    /** For the host: record `text` as the user's message and start its turn. */
    case Send(text: String)

    /** From the host: the turn for the last message sent is queued. */
    case Started(turn: String)

    /** From the host: a turn's reply. */
    case Replied(text: String)

    /** From the host: a turn, or sending one, failed. */
    case Failed(reason: String)
  }

  /** The screen, titled `title` (the model it talks to). */
  final class App(title: String)
      extends StdBase[State, Msg]
      with Hotkeys[State, Msg]
      with Ambient[State, Msg]
      with Selecting[State, Msg]
      with Prompting[State, Msg]
      with FreeKeys[State, Msg] {

    val layers: Vector[Layer] =
      Vector(hotkeyLayer, ambientLayer, promptLayer, freeLayer, pageLayer, pointerLayer)

    val steps: Vector[Step] = Vector(ambientStep, scrollStep, editStep, pointerStep)

    def init: (State, Effect[Std | Msg]) = {
      val panes = Panes
        .of(TextPane(TranscriptPane, Doc.empty, cache = WrapCache.empty(80)))
        .focusOn(TranscriptPane)
      (State(panes, ChatPalette.prompt, 0, "loading", title, Std.State()), Effect.ToHost(Msg.Load))
    }

    val panes: State -> Panes = s => s.panes
    val withPanes: (State, Panes) -> State = (s, p) => s.copy(panes = p)
    val std: State -> Std.State = s => s.std
    val withStd: (State, Std.State) -> State = (s, c) => s.copy(std = c)
    val withEditor: (State, Editor) -> State = (s, e) => s.copy(editor = e)

    protected val ownUpdate: (Msg, State) -> (State, Effect[Std | Msg]) = (msg, state) =>
      msg match {
        case Msg.Submit =>
          val draft = state.editor.text.trim
          if (draft.isEmpty) (state, Effect.NoOp)
          else if (draft == "/quit") (state, Effect.Quit)
          else {
            val sent = append(
              state.copy(editor = state.editor.submitted, waiting = state.waiting + 1),
              Vector(ChatPalette.separator, ChatPalette.user(draft))
            )
            (sent.copy(status = "sending"), Effect.ToHost(Msg.Send(draft)))
          }
        case Msg.Send(_) | Msg.Load => (state, Effect.NoOp)
        case Msg.Loaded(said) =>
          val blocks = said.flatMap {
            case Said(true, text) => Vector(ChatPalette.separator, ChatPalette.user(text))
            case Said(false, text) => Vector(ChatPalette.assistant(text))
          }
          (append(state.copy(status = ""), blocks), Effect.NoOp)
        case Msg.Started(turn) => (state.copy(status = s"turn $turn"), Effect.NoOp)
        case Msg.Replied(text) =>
          val done = state.copy(waiting = math.max(0, state.waiting - 1), status = "")
          (append(done, Vector(ChatPalette.assistant(text))), Effect.NoOp)
        case Msg.Failed(reason) =>
          val done = state.copy(waiting = math.max(0, state.waiting - 1), status = "failed")
          (append(done, Vector(ChatPalette.failure(reason))), Effect.NoOp)
      }

    override val hotkeys: (State, Input) -> Option[Std | Msg] = (_, input) =>
      input match {
        case Input.Keyboard(Key.Ctrl('q')) => Some(Std.Quit)
        case _ => None
      }

    override val prompt: State -> Option[(PaneId, Editor)] = s => Some((PromptPane, s.editor))

    override val onPrompt: State -> State = s => relaid(s, s.std.size)

    override val free: (State, Input) -> Option[Msg] = (_, input) =>
      input match {
        case Input.Keyboard(Key.Enter) => Some(Msg.Submit)
        case _ => None
      }

    override val scrolls: State -> Vector[ScrollPane] = _ => Vector(transcript)

    val onResize: (State, Size) -> State = (state, size) => relaid(state, size)

    override val onCopy: (State, Option[String], Boolean) -> State = (state, text, _) =>
      state.copy(status = text.fold("nothing selected")(t => s"copied ${t.length} chars"))

    val view: State -> (Size -> Frame) = state =>
      size =>
        Frame(Surface.blank(size).blit(screen(state).render(size), Pos(0, 0)))
          .caretIn(PromptPane, box => state.editor.caretPos(box.cols, box.rows), when = true)

    override def firstFrame(size: Size): Frame = {
      val (state, _) = init
      view(update(Std.Resized(size), state)._1)(size)
    }

    /** Appends `blocks` to the transcript, re-wrapped at the size it was painted at. */
    private def append(state: State, blocks: Vector[Block]): State =
      state.copy(panes =
        state.panes.modify(TranscriptPane)(doc => blocks.foldLeft(doc)(_.append(_)))
      )

    private def relaid(state: State, size: Size): State =
      state.copy(panes =
        state.panes.layoutIn(screen(state).placed(size).nest(BodyPane, transcript.split))
      )

    private def screen(state: State): Regions =
      chrome.views(
        StatusBar(Vector(s" grit -- ${state.title}"), Vector(), ChatPalette.header),
        transcript.views(state.panes),
        state.editor,
        StatusBar(
          Vector(" ctrl-q quit ", " enter sends ", state.status),
          Vector(if (state.waiting > 0) s"waiting on ${state.waiting}" else "idle"),
          ChatPalette.status
        )
      )
  }
}

/** The chat screen's styles. A trimmed copy of `grit.tui.examples.Palette`, which
  * `grit.app` cannot depend on.
  */
object ChatPalette {

  private val Ink = Color.hex("#c0caf5")
  private val Iris = Color.hex("#bb9af7")
  private val Sky = Color.hex("#7dcfff")
  private val Steel = Color.hex("#7aa2f7")
  private val Rose = Color.hex("#f7768e")
  private val Slab = Color.hex("#292e42")
  private val Rail = Color.hex("#3b4261")

  def user(text: String): Block.Text =
    Block
      .styled(
        StyledText.styled("you> ", Style.fg(Iris) + Style.Bold) ++ StyledText
          .styled(text, Style.fg(Ink))
      )
      .copy(ground = Style.bg(Slab))

  def assistant(text: String): Block.Text =
    Block.styled(
      StyledText.styled("grit> ", Style.fg(Sky) + Style.Bold) ++ StyledText.styled(
        text,
        Style.fg(Ink)
      )
    )

  def failure(reason: String): Block.Text =
    Block.styled(StyledText.styled(s"! $reason", Style.fg(Rose) + Style.Italic))

  def separator: Block.Separator = Block.Separator(Style.fg(Rail) + Style.Dim)

  val header: Style = Style.fg(Color.hex("#1a1b26")) + Style.Bold + Style.bg(Steel)
  val status: Style = Style.fg(Color.hex("#a9b1d6")) + Style.bg(Slab)
  val scrollRail: Style = Style.fg(Rail)
  val scrollThumb: Style = Style.fg(Steel)

  def prompt: Editor =
    Editor(
      "",
      0,
      focused = true,
      title = "message",
      body = Style.fg(Ink),
      chrome = Style.fg(Sky),
      titleStyle = Style.fg(Ink) + Style.Bold
    )

}
