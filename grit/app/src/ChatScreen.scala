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

  /** One message of the conversation: the user's, or a reply. */
  final case class Said(user: Boolean, text: String)

  /** `thinking` is whether the conversation has a turn in progress: its line is the
    * transcript's last block while it is. `rev` is the revision the next block is given.
    */
  final case class State(
      panes: Panes,
      editor: Editor,
      thinking: Boolean,
      rev: Long,
      status: String,
      title: String,
      std: Std.State
  )

  /** What the transcript is made of, before each piece is given its revision. */
  private enum Piece {
    case Separator
    case User(text: String)
    case Reply(text: String)
    case Failure(reason: String)
    case Thinking
  }

  enum Msg {

    /** Enter in the prompt. */
    case Submit

    /** For the host: follow the conversation, from its beginning. */
    case Load

    /** For the host: record `text` as the user's message and start its turn. */
    case Send(text: String)

    /** From the host: messages new to the conversation, oldest first, and whether a turn
      * is in progress.
      */
    case Arrived(said: Vector[Said], thinking: Boolean)

    /** From the host: sending failed, or a turn ended with no reply. */
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
      (
        State(panes, ChatPalette.prompt, thinking = false, 1L, "loading", title, Std.State()),
        Effect.ToHost(Msg.Load)
      )
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
          // Shown when the store has it, like everything else in the transcript.
          else
            (
              state.copy(editor = state.editor.submitted, status = "sent"),
              Effect.ToHost(Msg.Send(draft))
            )
        case Msg.Send(_) | Msg.Load => (state, Effect.NoOp)
        case Msg.Arrived(said, thinking) =>
          val pieces = said.flatMap {
            case Said(true, text) => Vector(Piece.Separator, Piece.User(text))
            case Said(false, text) => Vector(Piece.Reply(text))
          }
          (withTail(state.copy(status = ""), pieces, thinking), Effect.NoOp)
        case Msg.Failed(reason) =>
          (withTail(state, Vector(Piece.Failure(reason)), state.thinking), Effect.NoOp)
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

    /** Appends `blocks` to the transcript, keeping the thinking line last while
      * `thinking`. Re-wrapped at the size the transcript was painted at.
      *
      * Each block is built with a revision of its own. The pane's wrap cache keys a block
      * by its position and revision and never compares text, so a block that takes the
      * thinking line's position must not share its revision, or the old rows are painted.
      */
    private def withTail(state: State, pieces: Vector[Piece], thinking: Boolean): State = {
      val all = pieces ++ Option.when(thinking)(Piece.Thinking)
      val built = all.zipWithIndex.map { case (piece, i) => block(piece, state.rev + i) }
      state.copy(
        thinking = thinking,
        rev = state.rev + all.size,
        panes = state.panes.modify(TranscriptPane) { doc =>
          val body = if (state.thinking) Doc(doc.blocks.dropRight(1)) else doc
          built.foldLeft(body)(_.append(_))
        }
      )
    }

    private def block(piece: Piece, rev: Long): Block = piece match {
      case Piece.Separator => ChatPalette.separator
      case Piece.User(text) => ChatPalette.user(text, rev)
      case Piece.Reply(text) => ChatPalette.assistant(text, rev)
      case Piece.Failure(reason) => ChatPalette.failure(reason, rev)
      case Piece.Thinking => ChatPalette.thinking(rev)
    }

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
          Vector(if (state.thinking) "thinking" else "idle"),
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
  private val Faint = Color.hex("#565f89")
  private val Iris = Color.hex("#bb9af7")
  private val Sky = Color.hex("#7dcfff")
  private val Steel = Color.hex("#7aa2f7")
  private val Rose = Color.hex("#f7768e")
  private val Slab = Color.hex("#292e42")
  private val Rail = Color.hex("#3b4261")

  /** Every block takes a revision: see `withTail` for why each must be new. */
  def user(text: String, rev: Long): Block.Text =
    Block
      .styled(
        StyledText.styled("you> ", Style.fg(Iris) + Style.Bold) ++ StyledText
          .styled(text, Style.fg(Ink)),
        rev
      )
      .copy(ground = Style.bg(Slab))

  def assistant(text: String, rev: Long): Block.Text =
    Block.styled(
      StyledText.styled("grit> ", Style.fg(Sky) + Style.Bold) ++ StyledText
        .styled(text, Style.fg(Ink)),
      rev
    )

  def thinking(rev: Long): Block.Text =
    Block.styled(StyledText.styled("grit is thinking…", Style.fg(Faint) + Style.Italic), rev)

  def failure(reason: String, rev: Long): Block.Text =
    Block.styled(StyledText.styled(s"! $reason", Style.fg(Rose) + Style.Italic), rev)

  /** Always revision 0; every other block's revision is at least 1. */
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
