package grit.app.chat

import grit.app.look.Look
import grit.tui.components.editor.Editor
import grit.tui.components.tree.Node.*
import grit.tui.components.tree.{Node, OnInput, PaneKey, Scroller}
import grit.tui.components.widget.StatusBar
import grit.tui.model.block.Block
import grit.tui.model.input.{Input, Key}
import grit.tui.model.select.Doc
import grit.tui.runtime.app.{Effect, TimerId}

/** grit's chat screen: a transcript, a prompt and a status line. Pure, like every
  * grit.tui app: a submission leaves as [[ChatScreen.Msg.Send]] through `Effect.ToHost`,
  * and what the engine did comes back as messages ([[ChatHost]] sends them).
  */
object ChatScreen {

  private val Transcript = PaneKey.of("transcript")

  /** One message of the conversation: the user's, or a reply. */
  final case class Said(user: Boolean, text: String)

  /** `said` is the transcript as the store has it. Below it, while the engine is
    * `opening`, is the ward; while `thinking` (a turn in progress), the spinner. `tick`
    * turns both.
    */
  final case class State(
      said: Vector[Block],
      reader: Scroller.State,
      editor: Editor,
      opening: Boolean,
      thinking: Boolean,
      tick: Long,
      status: String,
      title: String
  ) {

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

    /** From the host: messages new to the conversation, oldest first, and whether a turn
      * is in progress.
      */
    case Arrived(said: Vector[Said], thinking: Boolean)

    /** From the host: sending failed, a turn ended with no reply, or the engine would not
      * open.
      */
    case Failed(reason: String)

    /** From the host: the engine is open. */
    case Opened

    /** The runes turn one step. */
    case Tick

    /** The prompt, edited. */
    case Edited(editor: Editor)

    /** The transcript, scrolled, selected or copied from. */
    case Reader(m: Scroller.Msg)

    case Quit
  }

  /** The screen, titled `title` (the model it talks to). */
  final class App(title: String, look: Look) extends grit.tui.runtime.app.App[State, Msg] {

    def init: (State, Effect[Msg]) =
      (
        State(
          Vector.empty,
          Scroller.init,
          look.prompt,
          opening = true,
          thinking = false,
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
          else
            (s.copy(editor = s.editor.submitted, status = "sent"), Effect.ToHost(Msg.Send(draft)))
        case Msg.Send(_) | Msg.Load => (s, Effect.NoOp)
        case Msg.Arrived(said, thinking) =>
          val blocks = said.flatMap {
            case Said(true, text) => Vector(look.separator, look.user(text))
            case Said(false, text) => Vector(look.assistant(text))
          }
          animate(s, s.copy(said = s.said ++ blocks, thinking = thinking, status = ""))
        case Msg.Failed(reason) =>
          animate(s, s.copy(said = s.said :+ look.failure(reason), opening = false))
        case Msg.Opened => animate(s, s.copy(opening = false))
        case Msg.Tick =>
          // A tick that lands after the animation stopped ends the chain there.
          if (s.animated) (s.copy(tick = s.tick + 1), Effect.After(Runes, TickMs, Msg.Tick))
          else (s, Effect.NoOp)
        case Msg.Edited(e) => (s.copy(editor = e), Effect.NoOp)
        case Msg.Reader(Scroller.Msg.Copied(text, _)) =>
          if (text.isEmpty) (s.copy(status = "nothing selected"), Effect.NoOp)
          else (s.copy(status = s"copied ${text.length} chars"), Effect.CopyOut(text))
        case Msg.Reader(m) => (s.copy(reader = Scroller.update(m, s.reader)), Effect.NoOp)
        case Msg.Quit => (s, Effect.Quit)
      }

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
      case _ => None
    }

    private def enter: OnInput[Msg] = {
      case Input.Keyboard(Key.Enter) => Some(Msg.Submit)
      case _ => None
    }

    def view(s: State): Node[Msg] =
      column(
        fixed(1) -> paint(look.header(s.title)),
        flex(5) -> Scroller
          .view(
            Transcript,
            transcript(s),
            s.reader,
            bar = Some((look.scrollRail, look.scrollThumb))
          )
          .map(Msg.Reader(_)),
        fit(3, 0.5) -> Node.editor(s.editor).onEdit(Msg.Edited(_)),
        fixed(1) -> paint(
          StatusBar(
            Vector(" ctrl-q quit ", " enter sends ", s.status),
            Vector(
              if (s.opening) s"${Look.Runes.Ward(0)} opening "
              else if (s.thinking) s"${Look.Runes.futhark(s.tick)} thinking "
              else s"${Look.Runes.Idle} idle"
            ),
            look.status
          )
        )
      ).onKey(enter).onKeyFirst(hotkeys).grounded(look.ground)
  }
}
