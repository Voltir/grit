package grit.app.chat

import grit.tui.components.editor.Editor
import grit.tui.components.tree.Node.*
import grit.tui.components.tree.{Node, OnInput, PaneKey, Scroller}
import grit.tui.components.widget.StatusBar
import grit.tui.model.block.Block
import grit.tui.model.input.{Input, Key}
import grit.tui.model.select.Doc
import grit.tui.model.surface.*
import grit.tui.model.text.StyledText
import grit.tui.runtime.app.Effect

/** grit's chat screen: a transcript, a prompt and a status line. Pure, like every
  * grit.tui app: a submission leaves as [[ChatScreen.Msg.Send]] through `Effect.ToHost`,
  * and what the engine did comes back as messages ([[ChatHost]] sends them).
  */
object ChatScreen {

  private val Transcript = PaneKey.of("transcript")

  /** One message of the conversation: the user's, or a reply. */
  final case class Said(user: Boolean, text: String)

  /** `thinking` is whether the conversation has a turn in progress: its line is the
    * transcript's last block while it is.
    */
  final case class State(
      transcript: Doc,
      reader: Scroller.State,
      editor: Editor,
      thinking: Boolean,
      status: String,
      title: String
  )

  enum Msg extends caps.Pure {

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

    /** The prompt, edited. */
    case Edited(editor: Editor)

    /** The transcript, scrolled, selected or copied from. */
    case Reader(m: Scroller.Msg)

    case Quit
  }

  /** The screen, titled `title` (the model it talks to). */
  final class App(title: String) extends grit.tui.runtime.app.App[State, Msg] {

    def init: (State, Effect[Msg]) =
      (
        State(Doc.empty, Scroller.init, ChatPalette.prompt, thinking = false, "loading", title),
        Effect.ToHost(Msg.Load)
      )

    def update(msg: Msg, s: State): (State, Effect[Msg]) =
      msg match {
        case Msg.Submit =>
          val draft = s.editor.text.trim
          if (draft.isEmpty) (s, Effect.NoOp)
          else if (draft == "/quit") (s, Effect.Quit)
          // Shown when the store has it, like everything else in the transcript.
          else
            (s.copy(editor = s.editor.submitted, status = "sent"), Effect.ToHost(Msg.Send(draft)))
        case Msg.Send(_) | Msg.Load => (s, Effect.NoOp)
        case Msg.Arrived(said, thinking) =>
          val blocks = said.flatMap {
            case Said(true, text) => Vector(ChatPalette.separator, ChatPalette.user(text))
            case Said(false, text) => Vector(ChatPalette.assistant(text))
          }
          (withTail(s.copy(status = ""), blocks, thinking), Effect.NoOp)
        case Msg.Failed(reason) =>
          (withTail(s, Vector(ChatPalette.failure(reason)), s.thinking), Effect.NoOp)
        case Msg.Edited(e) => (s.copy(editor = e), Effect.NoOp)
        case Msg.Reader(Scroller.Msg.Copied(text, _)) =>
          if (text.isEmpty) (s.copy(status = "nothing selected"), Effect.NoOp)
          else (s.copy(status = s"copied ${text.length} chars"), Effect.CopyOut(text))
        case Msg.Reader(m) => (s.copy(reader = Scroller.update(m, s.reader)), Effect.NoOp)
        case Msg.Quit => (s, Effect.Quit)
      }

    /** `blocks` appended, the thinking line kept last while `thinking`. A reply that takes
      * the thinking line's place is a different block, so the runtime's wrap memo wraps it
      * afresh: nothing here has to be told apart by revision.
      */
    private def withTail(s: State, blocks: Vector[Block], thinking: Boolean): State = {
      val body = if (s.thinking) s.transcript.blocks.dropRight(1) else s.transcript.blocks
      s.copy(
        transcript = Doc(body ++ blocks ++ Option.when(thinking)(ChatPalette.thinking)),
        thinking = thinking
      )
    }

    private def hotkeys: OnInput[Msg] = {
      case Input.Keyboard(Key.Ctrl('q')) => Some(Msg.Quit)
      case _ => None
    }

    private def enter: OnInput[Msg] = {
      case Input.Keyboard(Key.Enter) => Some(Msg.Submit)
      case _ => None
    }

    def view(s: State): Node[Msg] =
      column(
        fixed(1) -> paint(StatusBar(Vector(s" grit -- ${s.title}"), Vector(), ChatPalette.header)),
        flex(5) -> Scroller
          .view(
            Transcript,
            s.transcript,
            s.reader,
            bar = Some((ChatPalette.scrollRail, ChatPalette.scrollThumb))
          )
          .map(Msg.Reader(_)),
        fit(3, 0.5) -> Node.editor(s.editor).onEdit(Msg.Edited(_)),
        fixed(1) -> paint(
          StatusBar(
            Vector(" ctrl-q quit ", " enter sends ", s.status),
            Vector(if (s.thinking) "thinking" else "idle"),
            ChatPalette.status
          )
        )
      ).onKey(enter).onKeyFirst(hotkeys)
  }
}

/** The chat screen's styles. Its own, not the library's: a theme is an app's choice.
  * The colours are the examples' (`grit.tui.examples.Palette`), which `grit.app` cannot
  * depend on.
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

  def user(text: String): Block.Text =
    Block
      .styled(
        StyledText.styled("you> ", Style.fg(Iris) + Style.Bold) ++ StyledText
          .styled(text, Style.fg(Ink))
      )
      .copy(ground = Style.bg(Slab))

  def assistant(text: String): Block.Text =
    Block.styled(
      StyledText.styled("grit> ", Style.fg(Sky) + Style.Bold) ++ StyledText
        .styled(text, Style.fg(Ink))
    )

  val thinking: Block.Text =
    Block.styled(StyledText.styled("grit is thinking…", Style.fg(Faint) + Style.Italic))

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
