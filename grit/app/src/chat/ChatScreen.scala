package grit.app.chat

import grit.app.look.Look
import grit.core.id.TurnSeq
import grit.core.message.Tokens
import grit.tui.components.editor.Editor
import grit.tui.components.pane.Anchor
import grit.tui.components.tree.Node.*
import grit.tui.components.tree.{Node, OnInput, PaneKey, Scroller}
import grit.tui.components.widget.StatusBar
import grit.tui.model.block.Block
import grit.tui.model.input.{Input, Key}
import grit.tui.model.select.{Doc, DocPos}
import grit.tui.runtime.app.{Effect, TimerId}

/** grit's chat screen: a transcript beside the turn panel, a prompt and a status line.
  * Pure, like every grit.tui app: a submission leaves as [[ChatScreen.Msg.Send]] through
  * `Effect.ToHost`, and what the engine did comes back as messages ([[ChatHost]] sends
  * them).
  */
object ChatScreen {

  private val Transcript = PaneKey.of("transcript")

  private val Panel = PaneKey.of("turn-panel")

  private val Opened = PaneKey.of("turn-modal")

  /** One message of the conversation, the user's or a reply, and the turn it belongs to. */
  final case class Said(user: Boolean, text: String, turn: TurnSeq = TurnSeq(0))

  /** `said` is the transcript as the store has it. Below it, while the engine is
    * `opening`, is the ward; while a turn is in progress, the spinner, and the status line
    * names the turn's `step`, which began at tick `stepSince`. `tick` turns the runes and
    * times the step. `owners` is the turn each block of `said` belongs to, if any. Beside
    * it, while `panel` is on and the screen is wide enough, the turn panel shows `turn`:
    * the latest, or the one `pinned` by a click. While `modal`, the turn is opened over
    * the screen, scrolled by `modalReader`.
    */
  final case class State(
      said: Vector[Block],
      reader: Scroller.State,
      editor: Editor,
      opening: Boolean,
      step: Option[String],
      stepSince: Long,
      tick: Long,
      status: String,
      title: String,
      turn: Option[TurnView] = None,
      panel: Boolean = true,
      panelReader: Scroller.State = Scroller.State(Anchor.At(DocPos.zero)),
      owners: Vector[Option[TurnSeq]] = Vector.empty,
      pinned: Option[TurnSeq] = None,
      modal: Boolean = false,
      modalReader: Scroller.State = Scroller.State(Anchor.At(DocPos.zero))
  ) {

    /** Whether a turn is in progress. */
    def thinking: Boolean = step.nonEmpty

    /** How long the step has run, in milliseconds, as the ticks count it. */
    def stepMs: Long = (tick - stepSince) * TickMs

    /** Whether anything on screen is animated, so the tick must keep coming. */
    def animated: Boolean = opening || thinking
  }

  /** The one timer that turns the runes. */
  val Runes: TimerId = TimerId.of("runes")

  /** How often the runes turn. */
  val TickMs = 120L

  enum Msg extends caps.Pure {

    /** Enter in the prompt. */
    case Submit

    /** For the host: follow the conversation, from its beginning. */
    case Load

    /** For the host: record `text` as the user's message and start its turn. */
    case Send(text: String)

    /** From the host: messages new to the conversation, oldest first, and the step of the
      * turn in progress (`None` when none is).
      */
    case Arrived(said: Vector[Said], step: Option[String])

    /** From the host: sending failed, a turn ended with no reply, or the engine would not
      * open.
      */
    case Failed(reason: String)

    /** From the host: the engine is open. */
    case Opened

    /** The runes turn one step. */
    case Tick

    /** From the host: the turn the panel shows, as it now stands. */
    case Turn(view: TurnView)

    /** Ctrl-B: the turn panel shown, or hidden. */
    case TogglePanel

    /** The turn panel, scrolled, selected or copied from. */
    case PanelReader(m: Scroller.Msg)

    /** For the host: show `turn` in the panel, or the latest when `None`. */
    case Show(turn: Option[TurnSeq])

    /** Escape: the panel follows the latest turn again. */
    case Unpin

    /** The opened turn, scrolled, selected or copied from. */
    case ModalReader(m: Scroller.Msg)

    /** Escape over the opened turn. */
    case CloseModal

    /** The prompt, edited. */
    case Edited(editor: Editor)

    /** The transcript, scrolled, selected or copied from. */
    case Reader(m: Scroller.Msg)

    case Quit
  }

  /** The screen, titled `title` (the model it talks to); `budget` is what the assembler
    * may spend on earlier turns, which the panel measures windows against.
    */
  final class App(title: String, look: Look, budget: Tokens)
      extends grit.tui.runtime.app.App[State, Msg] {

    private val panel = TurnPanel(look, budget)

    def init: (State, Effect[Msg]) =
      (
        State(
          Vector.empty,
          Scroller.init,
          look.prompt,
          opening = true,
          step = None,
          stepSince = 0,
          tick = 0,
          "",
          title
        ),
        Effect.Batch(Vector(Effect.ToHost(Msg.Load), Effect.After(Runes, TickMs, Msg.Tick)))
      )

    def update(msg: Msg, s: State): (State, Effect[Msg]) =
      msg match {
        case Msg.Submit =>
          val draft = s.editor.text.trim
          if (draft.isEmpty) (s, Effect.NoOp)
          else if (draft == "/quit") (s, Effect.Quit)
          // Shown when the store has it, like everything else in the transcript.
          // A new turn is the one to watch: the panel lets go of any pinned one.
          else
            (
              s.copy(editor = s.editor.submitted, status = "sent", pinned = None),
              if (s.pinned.isEmpty) Effect.ToHost(Msg.Send(draft))
              else
                Effect.Batch(Vector(Effect.ToHost(Msg.Send(draft)), Effect.ToHost(Msg.Show(None))))
            )
        case Msg.Send(_) | Msg.Load | Msg.Show(_) => (s, Effect.NoOp)
        case Msg.Arrived(said, step) =>
          val blocks = said.flatMap {
            case Said(true, text, t) => Vector(look.separator -> t, look.user(text) -> t)
            case Said(false, text, t) => Vector(look.assistant(text) -> t)
          }
          val since = if (step == s.step) s.stepSince else s.tick
          animate(
            s,
            s.copy(
              said = s.said ++ blocks.map(_(0)),
              owners = s.owners ++ blocks.map(b => Some(b(1))),
              step = step,
              stepSince = since,
              status = ""
            )
          )
        case Msg.Failed(reason) =>
          animate(
            s,
            s.copy(
              said = s.said :+ look.failure(reason),
              owners = s.owners :+ None,
              opening = false
            )
          )
        case Msg.Opened => animate(s, s.copy(opening = false))
        case Msg.Tick =>
          // A tick that lands after the animation stopped ends the chain there.
          if (s.animated) (s.copy(tick = s.tick + 1), Effect.After(Runes, TickMs, Msg.Tick))
          else (s, Effect.NoOp)
        case Msg.Edited(e) => (s.copy(editor = e), Effect.NoOp)
        case Msg.Reader(Scroller.Msg.Copied(text, _)) =>
          if (text.isEmpty) (s.copy(status = "nothing selected"), Effect.NoOp)
          else (s.copy(status = s"copied ${text.length} chars"), Effect.CopyOut(text))
        case Msg.Reader(Scroller.Msg.Clicked(at)) => clicked(s, at.entry)
        case Msg.Reader(m) => (s.copy(reader = Scroller.update(m, s.reader)), Effect.NoOp)
        case Msg.Unpin =>
          if (s.pinned.isEmpty) (s, Effect.NoOp)
          else (s.copy(pinned = None), Effect.ToHost(Msg.Show(None)))
        case Msg.ModalReader(Scroller.Msg.Copied(text, _)) =>
          if (text.isEmpty) (s.copy(status = "nothing selected"), Effect.NoOp)
          else (s.copy(status = s"copied ${text.length} chars"), Effect.CopyOut(text))
        case Msg.ModalReader(m) =>
          (s.copy(modalReader = Scroller.update(m, s.modalReader)), Effect.NoOp)
        case Msg.CloseModal => (s.copy(modal = false), Effect.NoOp)
        case Msg.Turn(view) => (s.copy(turn = Some(view)), Effect.NoOp)
        case Msg.TogglePanel => (s.copy(panel = !s.panel), Effect.NoOp)
        case Msg.PanelReader(Scroller.Msg.Copied(text, _)) =>
          if (text.isEmpty) (s.copy(status = "nothing selected"), Effect.NoOp)
          else (s.copy(status = s"copied ${text.length} chars"), Effect.CopyOut(text))
        case Msg.PanelReader(m) =>
          (s.copy(panelReader = Scroller.update(m, s.panelReader)), Effect.NoOp)
        case Msg.Quit => (s, Effect.Quit)
      }

    /** A click on transcript block `entry`: on a message, its turn in the panel (the
      * latest turn unpins); on the thinking line, the running turn opened.
      */
    private def clicked(s: State, entry: Int): (State, Effect[Msg]) =
      s.owners.lift(entry).flatten match {
        case Some(turn) =>
          val pin = Option.when(!s.owners.flatten.lastOption.contains(turn))(turn)
          if (pin == s.pinned) (s, Effect.NoOp)
          else (s.copy(pinned = pin, panelReader = top), Effect.ToHost(Msg.Show(pin)))
        case None if entry == s.said.size && s.thinking && !s.opening =>
          (
            s.copy(modal = true, modalReader = top, pinned = None),
            if (s.pinned.isEmpty) Effect.NoOp else Effect.ToHost(Msg.Show(None))
          )
        case None => (s, Effect.NoOp)
      }

    private val top = Scroller.State(Anchor.At(DocPos.zero))

    /** `after`, with the rune timer started if it now has something to turn, or
      * cancelled if it no longer has. The runtime replaces a timer re-armed under the same
      * id, so the chain never forks.
      */
    private def animate(before: State, after: State): (State, Effect[Msg]) =
      if (after.animated && !before.animated) (after, Effect.After(Runes, TickMs, Msg.Tick))
      else if (!after.animated && before.animated) (after, Effect.Cancel(Runes))
      else (after, Effect.NoOp)

    /** The transcript: what was said, then the ward or the spinner. A tail that changes
      * each tick is a different block each tick, so the runtime's wrap memo re-wraps that
      * one row and nothing above it.
      */
    private def transcript(s: State): Doc =
      Doc(
        s.said ++
          Option.when(s.opening)(look.ward(s.tick, "opening the engine…")) ++
          Option.when(s.thinking && !s.opening)(look.thinking(s.tick))
      )

    private def hotkeys: OnInput[Msg] = {
      case Input.Keyboard(Key.Ctrl('q')) => Some(Msg.Quit)
      case Input.Keyboard(Key.Ctrl('b')) => Some(Msg.TogglePanel)
      case _ => None
    }

    private def enter: OnInput[Msg] = {
      case Input.Keyboard(Key.Enter) => Some(Msg.Submit)
      case Input.Keyboard(Key.Escape) => Some(Msg.Unpin)
      case _ => None
    }

    /** The transcript, with the turn panel beside it when it is on and there is room. */
    private def body(s: State): Node[Msg] = {
      val bar = Some((look.scrollRail, look.scrollThumb))
      val reading = Scroller.view(Transcript, transcript(s), s.reader, bar = bar).map(Msg.Reader(_))
      if (!s.panel) reading
      else {
        val shown = s.turn.filter(_.running.nonEmpty).fold(0L)(_ => s.stepMs)
        val turn = Scroller
          .view(
            Panel,
            Doc(panel.blocks(s.turn, shown, s.pinned.nonEmpty)),
            s.panelReader,
            bar = bar
          )
          .map(Msg.PanelReader(_))
        wide(TurnPanel.ShownFrom)(
          row(flex(20) -> reading, fixed(TurnPanel.Cols) -> turn.grounded(look.sidebar)),
          reading
        )
      }
    }

    /** The running turn, opened: what was asked, then all the panel says of it. */
    private def opened(s: State): Node[Msg] = {
      val shown = s.turn.filter(_.running.nonEmpty).fold(0L)(_ => s.stepMs)
      Scroller
        .view(Opened, Doc(panel.opened(s.turn, shown)), s.modalReader)
        .map(Msg.ModalReader(_))
        .grounded(look.modalGround)
    }

    def view(s: State): Node[Msg] = {
      val screen = column(
        fixed(1) -> paint(look.header(s.title)),
        flex(5) -> body(s),
        fit(3, 0.5) -> Node.editor(s.editor).onEdit(Msg.Edited(_)),
        fixed(1) -> paint(
          // Where grit is goes left, where the bar keeps it; the hints give way first.
          StatusBar(
            Vector(
              s.step match {
                case _ if s.opening => s" ${Look.Runes.Ward(0)} opening"
                case Some(step) => s" ${Look.Runes.step(step)} · ${TurnPanel.seconds(s.stepMs)}"
                case None => s" ${Look.Runes.Idle} idle"
              },
              s.status
            ),
            Vector("enter sends", "ctrl-b panel", "ctrl-q quit "),
            look.status
          )
        )
      ).onKey(enter)
      val title = s.turn.fold("turn")(v => s"turn ${TurnPanel.number(v.turn)}")
      // The hotkeys wrap the dialog, so ctrl-q still quits over it.
      screen
        .when(s.modal)(_.dialog(look.modal(title), opened(s), Some(Msg.CloseModal)))
        .onKeyFirst(hotkeys)
        .grounded(look.ground)
    }
  }
}
