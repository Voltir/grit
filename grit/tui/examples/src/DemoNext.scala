package grit.tui.examples

import grit.tui.components.editor.Editor
import grit.tui.components.overlay.{Modal, Popup}
import grit.tui.components.pane.Anchor
import grit.tui.components.widget.StatusBar
import grit.tui.model.block.Block
import grit.tui.model.input.{Input, Key}
import grit.tui.model.select.Doc
import grit.tui.model.text.StyledText
import grit.tui.node.*
import grit.tui.node.Node.*
import grit.tui.runtime.{Effect, TimerId}

/** Demo2 on the view tree (`grit.tui.node`): the same screen -- a streaming transcript
  * with a scrollbar, a modal over it, a completion popup, a growing prompt -- written as
  * `view: State -> Node[Msg]`. `scripts/tui-gate` runs against it with
  * `MAIN=grit.tui.examples.DemoNext`.
  */
object DemoNext extends NodeApp[DemoNext.State, DemoNext.Msg] {

  private val HelpModal = Modal(
    "help -- try dragging out of me",
    rows = 9,
    cols = 46,
    panel = Palette.modalPanel,
    chrome = Palette.modalChrome,
    behind = Palette.modalBehind
  )

  private val commands = Vector("/help", "/stream", "/clear", "/tools", "/quit")
  private val Streamer = TimerId.of("stream")
  private val StreamMs = 300L

  /** Real state only: two documents' contents and where the user is in each, the draft,
    * and the app's own modes. No rect, no viewport, no cache, no machine fragment.
    */
  final case class State(
      transcript: Doc,
      reader: Scroller.State,
      help: Scroller.State,
      modal: Boolean,
      streaming: Boolean,
      rev: Long,
      status: String,
      editor: Editor,
      popup: Option[Popup]
  )

  enum Msg extends caps.Pure {
    case Transcript(m: Scroller.Msg)
    case Help(m: Scroller.Msg)
    case Edited(e: Editor)
    case PopupTo(p: Option[Popup])
    case Chose(item: String)
    case ToggleModal
    case ToggleStream
    case ToggleExpand
    case Submit
    case PopupOpen
    case Tick
    case PromptPress
    case Quit
  }

  def init: (State, Effect[Msg]) =
    (
      State(
        Transcript.seeded,
        Scroller.init,
        Scroller.State(anchor = Anchor.At(grit.tui.model.select.DocPos.zero)),
        modal = false,
        streaming = true,
        rev = 0L,
        status = "streaming",
        editor = Palette.prompt,
        popup = None
      ),
      Effect.After(Streamer, StreamMs, Msg.Tick)
    )

  private val slash: Map[String, Msg] =
    Map("/stream" -> Msg.ToggleStream, "/tools" -> Msg.ToggleExpand, "/help" -> Msg.ToggleModal)

  def update(msg: Msg, s: State): (State, Effect[Msg]) =
    msg match {
      case Msg.Transcript(Scroller.Msg.Copied(text, expired)) => copied(s, text, expired)
      case Msg.Transcript(m) =>
        val touched = m match {
          case Scroller.Msg.Scrolled(_) => s.copy(status = "")
          case _ => s
        }
        (touched.copy(reader = Scroller.update(m, s.reader)), Effect.NoOp)
      case Msg.Help(Scroller.Msg.Copied(text, expired)) => copied(s, text, expired)
      case Msg.Help(m) => (s.copy(help = Scroller.update(m, s.help)), Effect.NoOp)

      case Msg.Edited(e) =>
        (s.copy(editor = e, status = "", popup = s.popup.map(_.withQuery(e.text))), Effect.NoOp)
      case Msg.PopupTo(p) => (s.copy(popup = p, status = ""), Effect.NoOp)
      case Msg.Chose(item) =>
        (
          s.copy(
            popup = None,
            editor = s.editor.copy(text = item, caret = item.length, histPos = None)
          ),
          Effect.NoOp
        )

      case Msg.ToggleModal => (s.copy(modal = !s.modal, status = ""), Effect.NoOp)
      case Msg.ToggleStream =>
        val on = !s.streaming
        (
          s.copy(streaming = on, status = if (on) "streaming" else "paused"),
          if (on) Effect.After(Streamer, StreamMs, Msg.Tick) else Effect.Cancel(Streamer)
        )
      case Msg.ToggleExpand =>
        (s.copy(transcript = Transcript.toggleExpansion(s.transcript)), Effect.NoOp)
      case Msg.PopupOpen =>
        (
          s.copy(
            popup = Some(Palette.popup(commands).withQuery(s.editor.text)),
            status = "complete"
          ),
          Effect.NoOp
        )
      case Msg.Submit => submit(s)
      case Msg.Tick =>
        if (!s.streaming) (s, Effect.NoOp)
        else {
          val rev = s.rev + 1
          (
            s.copy(rev = rev, transcript = Transcript.stream(s.transcript, rev)),
            Effect.After(Streamer, StreamMs, Msg.Tick)
          )
        }
      case Msg.PromptPress => (s.copy(status = ""), Effect.NoOp)
      case Msg.Quit => (s, Effect.Quit)
    }

  private def copied(s: State, text: String, expired: Boolean): (State, Effect[Msg]) = {
    val lines = text.count(_ == '\n') + 1
    val note =
      s"copied ${text.length} chars, $lines lines" + (if (expired) " (drag left the window)"
                                                      else "")
    (s.copy(status = note), Effect.CopyOut(text))
  }

  private def submit(s: State): (State, Effect[Msg]) = {
    val draft = s.editor.text
    val typed = s.copy(editor = s.editor.submitted)
    if (draft.startsWith("/")) {
      draft.trim match {
        case "/clear" =>
          (
            typed.copy(status = "cleared", transcript = Doc.empty, reader = Scroller.init),
            Effect.NoOp
          )
        case "/quit" => (typed, Effect.Quit)
        case c =>
          slash
            .get(c)
            .fold((typed.copy(status = s"no such command: $c"), Effect.NoOp))(m => update(m, typed))
      }
    } else { (typed.copy(transcript = Transcript.submitted(s.transcript, draft)), Effect.NoOp) }
  }

  /* ---- the screen ------------------------------------------------------------------ */

  private def hotkeys: OnInput[Msg] = {
    case Input.Keyboard(Key.Ctrl('q')) => Some(Msg.Quit)
    case Input.Keyboard(Key.Alt('m')) => Some(Msg.ToggleModal)
    case Input.Keyboard(Key.Alt('s')) => Some(Msg.ToggleStream)
    case Input.Keyboard(Key.Alt('t')) => Some(Msg.ToggleExpand)
    case _ => None
  }

  /** Enter and Tab: only once everything on the focus path has declined them. */
  private def free: OnInput[Msg] = {
    case Input.Keyboard(Key.Enter) => Some(Msg.Submit)
    case Input.Keyboard(Key.Tab) => Some(Msg.PopupOpen)
    case _ => None
  }

  private def popupRoute: Popup.Route -> Option[Msg] = {
    case Popup.Route.Stay(p) => Some(Msg.PopupTo(Some(p)))
    case Popup.Route.Chose(item) => Some(Msg.Chose(item))
    case Popup.Route.Dismissed => Some(Msg.PopupTo(None))
    case Popup.Route.Pass(_) => None
  }

  def view(s: State): Node[Msg] = {
    val prompt = Node.editor(s.editor).onEdit(Msg.Edited(_)).onPress(_ => Some(Msg.PromptPress))
    val screen = column(
      fixed(1) -> paint(
        StatusBar(
          Vector(" grit.tui -- the grit screen"),
          Vector(if (s.streaming) "streaming" else "paused"),
          Palette.header(s.streaming)
        )
      ),
      flex(5) -> Scroller
        .view(
          PaneKey.of("transcript"),
          s.transcript,
          s.reader,
          bar = Some((Palette.scrollRail, Palette.scrollThumb))
        )
        .map(Msg.Transcript(_)),
      fit(3, 0.5) -> s.popup.fold(prompt)(p => prompt.floating(p, popupRoute)),
      fixed(1) -> paint(statusBar(s))
    ).onKey(free)
    // The hotkeys go outside the dialog: a modal captures its subtree, and the focus path
    // runs through whatever encloses it -- precedence is where a handler sits in the tree.
    screen
      .when(s.modal)(
        _.dialog(
          HelpModal,
          Scroller.view(PaneKey.of("help"), help, s.help, focused = true).map(Msg.Help(_)),
          Some(Msg.ToggleModal)
        )
      )
      .onKeyFirst(hotkeys)
  }

  private def statusBar(s: State): StatusBar = {
    val anchor = s.reader.anchor match {
      case Anchor.Bottom => "tail"
      case Anchor.At(p) => s"@${p.entry}"
    }
    val live =
      if (s.reader.selection.isDefined || s.help.selection.isDefined) "selecting" else s.status
    StatusBar(
      Vector(
        " ctrl-q quit ",
        " alt-m modal ",
        " alt-s stream ",
        " alt-t tools ",
        " tab completes ",
        live
      ),
      Vector(anchor),
      Palette.status
    )
  }

  private val help: Doc =
    Doc(
      Vector(
        "This is a second TextPane, composited over the",
        "transcript with its own scroll position and its",
        "own document.",
        "",
        "Start a drag in here and run the pointer down",
        "over the transcript: the selection stays in this",
        "pane and clamps to this document. A drag belongs",
        "to the pane it began in.",
        "",
        "Esc or alt-m closes."
      ).map(l => Block.styled(StyledText.styled(l, Palette.helpText)))
    )
}
