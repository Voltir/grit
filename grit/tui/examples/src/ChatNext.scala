package grit.tui.examples

import grit.tui.components.editor.Editor
import grit.tui.components.widget.StatusBar
import grit.tui.model.block.Block
import grit.tui.model.input.{Input, Key}
import grit.tui.model.select.Doc
import grit.tui.model.surface.{Color, Style}
import grit.tui.model.text.StyledText
import grit.tui.node.*
import grit.tui.node.Node.*
import grit.tui.runtime.{Effect, Host, Mailbox}

/** `grit.app.ChatScreen` on the view tree (`grit.tui.node`): the same header, transcript
  * with a scrollbar, growing prompt, status bar, and thinking line replaced by the reply.
  * Driven by [[ChatNext.FakeHost]] -- no engine, no Postgres, no model.
  */
object ChatNext {

  final case class Said(user: Boolean, text: String)

  final case class State(
      transcript: Doc,
      reader: Scroller.State,
      editor: Editor,
      thinking: Boolean,
      status: String,
      title: String
  )

  enum Msg extends caps.Pure {
    case Submit
    case Load
    case Send(text: String)
    case Arrived(said: Vector[Said], thinking: Boolean)
    case Failed(reason: String)
    case Edited(e: Editor)
    case Reader(m: Scroller.Msg)
    case Quit
  }

  final class App(title: String) extends NodeApp[State, Msg] {

    def init: (State, Effect[Msg]) =
      (
        State(Doc.empty, Scroller.init, Chat.prompt, thinking = false, "loading", title),
        Effect.ToHost(Msg.Load)
      )

    def update(msg: Msg, s: State): (State, Effect[Msg]) =
      msg match {
        case Msg.Submit =>
          val draft = s.editor.text.trim
          if (draft.isEmpty) (s, Effect.NoOp)
          else if (draft == "/quit") (s, Effect.Quit)
          else
            (s.copy(editor = s.editor.submitted, status = "sent"), Effect.ToHost(Msg.Send(draft)))
        case Msg.Send(_) | Msg.Load => (s, Effect.NoOp)
        case Msg.Arrived(said, thinking) =>
          val blocks = said.flatMap {
            case Said(true, text) => Vector(Chat.separator, Chat.user(text))
            case Said(false, text) => Vector(Chat.assistant(text))
          }
          (withTail(s.copy(status = ""), blocks, thinking), Effect.NoOp)
        case Msg.Failed(reason) =>
          (withTail(s, Vector(Chat.failure(reason)), s.thinking), Effect.NoOp)
        case Msg.Edited(e) => (s.copy(editor = e), Effect.NoOp)
        case Msg.Reader(Scroller.Msg.Copied(text, _)) =>
          (s.copy(status = s"copied ${text.length} chars"), Effect.CopyOut(text))
        case Msg.Reader(m) => (s.copy(reader = Scroller.update(m, s.reader)), Effect.NoOp)
        case Msg.Quit => (s, Effect.Quit)
      }

    /** `blocks` appended, the thinking line kept last while `thinking`. No revisions: the
      * runtime's wrap memo compares blocks, so a reply taking the thinking line's place is
      * a different block and wraps as one.
      */
    private def withTail(s: State, blocks: Vector[Block], thinking: Boolean): State = {
      val body = if (s.thinking) s.transcript.blocks.dropRight(1) else s.transcript.blocks
      s.copy(
        transcript = Doc(body ++ blocks ++ Option.when(thinking)(Chat.thinking)),
        thinking = thinking
      )
    }

    private def keys: OnInput[Msg] = {
      case Input.Keyboard(Key.Ctrl('q')) => Some(Msg.Quit)
      case _ => None
    }

    private def enter: OnInput[Msg] = {
      case Input.Keyboard(Key.Enter) => Some(Msg.Submit)
      case _ => None
    }

    def view(s: State): Node[Msg] =
      column(
        fixed(1) -> paint(StatusBar(Vector(s" grit -- ${s.title}"), Vector(), Chat.header)),
        flex(5) -> Scroller
          .view(
            PaneKey.of("transcript"),
            s.transcript,
            s.reader,
            bar = Some((Chat.rail, Chat.thumb))
          )
          .map(Msg.Reader(_)),
        fit(3, 0.5) -> Node.editor(s.editor).onEdit(Msg.Edited(_)),
        fixed(1) -> paint(
          StatusBar(
            Vector(" ctrl-q quit ", " enter sends ", s.status),
            Vector(if (s.thinking) "thinking" else "idle"),
            Chat.status
          )
        )
      ).onKey(enter).onKeyFirst(keys)
  }

  /** A host with a script instead of an engine: `Load` answers with a short history,
    * `Send` shows the message with a thinking line, and replies after `replyMs`.
    */
  final class FakeHost(replyMs: Long) extends Host[Msg] {
    def receive(msg: Msg, mailbox: Mailbox[Msg]): Unit = msg match {
      case Msg.Load =>
        mailbox.offer(
          Msg.Arrived(
            Vector(
              Said(true, "hello"),
              Said(false, "hi -- this transcript is scripted by ChatNext.FakeHost.")
            ),
            thinking = false
          )
        )
      case Msg.Send(text) =>
        mailbox.offer(Msg.Arrived(Vector(Said(true, text)), thinking = true))
        val _ = Thread.ofVirtual().start { () =>
          Thread.sleep(replyMs)
          mailbox.offer(Msg.Arrived(Vector(Said(false, s"stub reply to: $text")), thinking = false))
        }
      case _ => ()
    }
  }

  def main(args: Array[String]): Unit = {
    val _ = args
    NodeRuntime.run(new App("fake"), new FakeHost(800L))
  }
}

/** The chat styles: `grit.app.ChatPalette`, which examples cannot depend on. */
private object Chat {
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
      StyledText.styled("grit> ", Style.fg(Sky) + Style.Bold) ++ StyledText.styled(
        text,
        Style.fg(Ink)
      )
    )
  val thinking: Block.Text =
    Block.styled(StyledText.styled("grit is thinking…", Style.fg(Faint) + Style.Italic))
  def failure(reason: String): Block.Text =
    Block.styled(StyledText.styled(s"! $reason", Style.fg(Rose) + Style.Italic))
  val separator: Block.Separator = Block.Separator(Style.fg(Rail) + Style.Dim)
  val header: Style = Style.fg(Color.hex("#1a1b26")) + Style.Bold + Style.bg(Steel)
  val status: Style = Style.fg(Color.hex("#a9b1d6")) + Style.bg(Slab)
  val rail: Style = Style.fg(Rail)
  val thumb: Style = Style.fg(Steel)
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
