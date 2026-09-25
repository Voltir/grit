package grit.app.chat

import grit.app.chat.Commands.Picked
import grit.app.look.{Look, Theme}
import grit.core.id.TurnSeq
import grit.core.message.Tokens
import grit.tui.components.editor.Editor
import grit.tui.components.overlay.Popup
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

  private val HelpPane = PaneKey.of("help")

  /** One message of the conversation, the user's or a reply, and the turn it belongs to. */
  final case class Said(user: Boolean, text: String, turn: TurnSeq = TurnSeq(0))

  /** One entry of the transcript, before it is styled: something said, or a failure. */
  enum Entry extends caps.Pure {
    case Spoken(said: Said)
    case Failure(reason: String)
  }

  /** What has been heard of `turn`'s reply while it streams: its reasoning and text so far. */
  final case class Hearing(turn: TurnSeq, reasoning: String, text: String)

  /** The dialog over the screen: the running turn, opened, or the help. */
  enum Dialog extends caps.Pure {
    case Turn, Help
  }

  /** `said` is the transcript as the store has it: `entries`, styled in `theme`, which
    * every colour on the screen comes from. Below it, while the engine is
    * `opening`, is the ward; while a turn is in progress, the spinner, and the status line
    * names the turn's `step`, which began at tick `stepSince`. `tick` turns the runes and
    * times the step. `owners` is the turn each block of `said` belongs to, if any. Beside
    * it, while `panel` is on and the screen is wide enough, the turn panel shows `turn`:
    * the latest, or the one `pinned` by a click. A `modal` dialog opens over the screen,
    * scrolled by `modalReader`. While `palette` is open, the command list floats over the
    * prompt with that row selected; a draft put `aside` to open it comes back when it
    * closes.
    */
  final case class State(
      said: Vector[Block],
      entries: Vector[Entry],
      theme: Theme,
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
      modal: Option[Dialog] = None,
      modalReader: Scroller.State = Scroller.State(Anchor.At(DocPos.zero)),
      palette: Option[Int] = None,
      aside: Option[String] = None,
      hearing: Option[Hearing] = None
  ) {

    /** The running turn's reply as heard so far, while it has any text. */
    def streaming: Option[Hearing] =
      hearing.filter(h => thinking && h.text.nonEmpty && owners.flatten.lastOption.contains(h.turn))

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

    /** Ctrl-P: the command palette opened. */
    case OpenPalette

    /** The palette's selection moved to a row, or (`None`) the palette dismissed. */
    case PaletteTo(selected: Option[Int])

    /** A row of the palette chosen. */
    case Chose(item: String)

    /** `/theme`: every colour from `theme` from now on. */
    case SetTheme(theme: Theme)

    /** `/help`: the commands and keys, in a dialog. */
    case OpenHelp

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

    /** From the host: the running turn's reply, as heard so far. */
    case Heard(hearing: Hearing)

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

    /** Escape over a dialog. */
    case CloseModal

    /** The prompt, edited. */
    case Edited(editor: Editor)

    /** The transcript, scrolled, selected or copied from. */
    case Reader(m: Scroller.Msg)

    case Quit
  }

  /** The keys the screen binds, and what each does, as the help lists them. */
  val Keys: Vector[(String, String)] = Vector(
    "enter" -> "send, or run a /command",
    "ctrl-p" -> "the command palette",
    "ctrl-b" -> "show or hide the turn panel",
    "ctrl-q" -> "quit",
    "esc" -> "close a list or dialog; unpin",
    "click a message" -> "its turn in the panel",
    "click the thinking line" -> "open the running turn"
  )

  /** The screen, titled `title` (the model it talks to), first in `theme`; `budget` is
    * what the assembler may spend on earlier turns, which the panel measures windows
    * against.
    */
  final class App(title: String, theme: Theme, budget: Tokens)
      extends grit.tui.runtime.app.App[State, Msg] {

    def init: (State, Effect[Msg]) =
      (
        State(
          Vector.empty,
          Vector.empty,
          theme,
          Scroller.init,
          Look(theme).prompt,
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
          // A command, never a message: one that will not run says why, and goes nowhere.
          else if (draft.startsWith("/")) command(ran(s, draft), draft)
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
          val since = if (step == s.step) s.stepSince else s.tick
          // A recorded reply takes the place of what was heard of it.
          val heard = s.hearing.filterNot(h => said.exists(r => !r.user && r.turn == h.turn))
          animate(
            s,
            recorded(s, said.map(Entry.Spoken(_)))
              .copy(step = step, stepSince = since, status = "", hearing = heard)
          )
        case Msg.Heard(h) => (s.copy(hearing = Some(h)), Effect.NoOp)
        case Msg.Failed(reason) =>
          animate(s, recorded(s, Vector(Entry.Failure(reason))).copy(opening = false))
        case Msg.Opened => animate(s, s.copy(opening = false))
        case Msg.Tick =>
          // A tick that lands after the animation stopped ends the chain there.
          if (s.animated) (s.copy(tick = s.tick + 1), Effect.After(Runes, TickMs, Msg.Tick))
          else (s, Effect.NoOp)
        case Msg.Edited(e) => (edited(s, e), Effect.NoOp)
        case Msg.OpenPalette =>
          if (s.modal.nonEmpty || s.palette.nonEmpty) (s, Effect.NoOp)
          else if (s.editor.text.startsWith("/")) (s.copy(palette = Some(0)), Effect.NoOp)
          else
            (
              s.copy(
                editor = s.editor.copy(text = "/", caret = 1, histPos = None),
                palette = Some(0),
                aside = Option.when(s.editor.text.nonEmpty)(s.editor.text)
              ),
              Effect.NoOp
            )
        case Msg.PaletteTo(Some(row)) => (s.copy(palette = Some(row)), Effect.NoOp)
        case Msg.PaletteTo(None) =>
          (s.copy(palette = None, aside = None, editor = restored(s)), Effect.NoOp)
        case Msg.Chose(item) =>
          Commands.pick(s.editor.text, item) match {
            case Picked.Fill(draft) =>
              (
                s.copy(
                  editor = s.editor.copy(text = draft, caret = draft.length, histPos = None),
                  palette = Some(0)
                ),
                Effect.NoOp
              )
            case Picked.Run(line) => command(ran(s, line), line)
          }
        case Msg.SetTheme(theme) =>
          val look = Look(theme)
          (
            s.copy(
              theme = theme,
              said = s.entries.flatMap(styled(look, _)).map(_(0)),
              editor = look.styled(s.editor),
              status = s"theme ${theme.key}"
            ),
            Effect.NoOp
          )
        case Msg.OpenHelp => (s.copy(modal = Some(Dialog.Help), modalReader = top), Effect.NoOp)
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
        case Msg.CloseModal => (s.copy(modal = None), Effect.NoOp)
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
            s.copy(modal = Some(Dialog.Turn), modalReader = top, pinned = None),
            if (s.pinned.isEmpty) Effect.NoOp else Effect.ToHost(Msg.Show(None))
          )
        case None => (s, Effect.NoOp)
      }

    private val top = Scroller.State(Anchor.At(DocPos.zero))

    /** `entries` added to the transcript, styled in the state's theme. */
    private def recorded(s: State, entries: Vector[Entry]): State = {
      val blocks = entries.flatMap(styled(Look(s.theme), _))
      s.copy(
        said = s.said ++ blocks.map(_(0)),
        entries = s.entries ++ entries,
        owners = s.owners ++ blocks.map(_(1))
      )
    }

    /** An entry's blocks in `look`, each with the turn it belongs to. */
    private def styled(look: Look, entry: Entry): Vector[(Block, Option[TurnSeq])] =
      entry match {
        case Entry.Spoken(Said(true, text, t)) =>
          Vector(look.separator -> Some(t), look.user(text) -> Some(t))
        case Entry.Spoken(Said(false, text, t)) => Vector(look.assistant(text) -> Some(t))
        case Entry.Failure(reason) => Vector(look.failure(reason) -> None)
      }

    /** The prompt edited to `e`. Typing `/` into an empty prompt opens the palette, and a
      * change of draft moves its selection back to the top; a draft that is no longer a
      * command closes it, bringing back a draft it put aside if the prompt is now empty.
      */
    private def edited(s: State, e: Editor): State = {
      val opens = s.palette.isEmpty && s.editor.text.isEmpty && e.text == "/"
      if (!e.text.startsWith("/")) {
        if (s.palette.isEmpty) s.copy(editor = e)
        else {
          val back = if (e.text.isEmpty) restored(s.copy(editor = e)) else e
          s.copy(editor = back, palette = None, aside = None)
        }
      } else if (opens || (s.palette.nonEmpty && e.text != s.editor.text))
        s.copy(editor = e, palette = Some(0))
      else s.copy(editor = e)
    }

    /** The prompt as it was before the palette put it aside, or as it is. */
    private def restored(s: State): Editor =
      s.aside.fold(s.editor)(a => s.editor.copy(text = a, caret = a.length, histPos = None))

    /** The palette closed on `line`, which goes into the prompt's history; the prompt is
      * left empty, or as it was before the palette put it aside.
      */
    private def ran(s: State, line: String): State = {
      val cleared = s.editor.copy(text = line).submitted
      s.copy(editor = restored(s.copy(editor = cleared)), palette = None, aside = None)
    }

    /** `line` run as a command, or why it would not run, in the status line. */
    private def command(s: State, line: String): (State, Effect[Msg]) =
      Commands.run(line) match {
        case Right(msg) => update(msg, s)
        case Left(reason) => (s.copy(status = reason), Effect.NoOp)
      }

    /** `after`, with the rune timer started if it now has something to turn, or
      * cancelled if it no longer has. The runtime replaces a timer re-armed under the same
      * id, so the chain never forks.
      */
    private def animate(before: State, after: State): (State, Effect[Msg]) =
      if (after.animated && !before.animated) (after, Effect.After(Runes, TickMs, Msg.Tick))
      else if (!after.animated && before.animated) (after, Effect.Cancel(Runes))
      else (after, Effect.NoOp)

    /** The transcript: what was said, then the ward, or the running turn's reply as it
      * streams, or the spinner until it does. A tail that changes is a different block, so
      * the runtime's wrap memo re-wraps that one and nothing above it.
      */
    private def transcript(s: State, look: Look): Doc =
      Doc(
        s.said ++
          Option.when(s.opening)(look.ward(s.tick, "opening the engine…")) ++
          Option.when(s.thinking && !s.opening)(
            s.streaming.fold(look.thinking(s.tick))(h => look.streaming(h.text, s.tick))
          )
      )

    private def hotkeys: OnInput[Msg] = {
      case Input.Keyboard(Key.Ctrl('q')) => Some(Msg.Quit)
      case Input.Keyboard(Key.Ctrl('b')) => Some(Msg.TogglePanel)
      case Input.Keyboard(Key.Ctrl('p')) => Some(Msg.OpenPalette)
      case _ => None
    }

    private def paletteRoute: Popup.Route -> Option[Msg] = {
      case Popup.Route.Stay(p) => Some(Msg.PaletteTo(Some(p.selected)))
      case Popup.Route.Chose(item) => Some(Msg.Chose(item))
      case Popup.Route.Dismissed => Some(Msg.PaletteTo(None))
      case Popup.Route.Pass(_) => None
    }

    /** The palette over `s`'s draft, when it is open and has something to offer. */
    private def palette(s: State, look: Look): Option[Popup] =
      s.palette
        .flatMap(row =>
          Commands.listing(s.editor.text).map(l => look.palette(l.items, l.query, row))
        )
        .filter(!_.isEmpty)

    private def enter: OnInput[Msg] = {
      case Input.Keyboard(Key.Enter) => Some(Msg.Submit)
      case Input.Keyboard(Key.Escape) => Some(Msg.Unpin)
      case _ => None
    }

    /** The transcript, with the turn panel beside it when it is on and there is room. */
    private def body(s: State, look: Look, panel: TurnPanel): Node[Msg] = {
      val bar = Some((look.scrollRail, look.scrollThumb))
      val reading =
        Scroller.view(Transcript, transcript(s, look), s.reader, bar = bar).map(Msg.Reader(_))
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
    private def opened(s: State, look: Look, panel: TurnPanel): Node[Msg] = {
      val shown = s.turn.filter(_.running.nonEmpty).fold(0L)(_ => s.stepMs)
      Scroller
        .view(Opened, Doc(panel.opened(s.turn, shown, s.hearing)), s.modalReader)
        .map(Msg.ModalReader(_))
        .grounded(look.modalGround)
    }

    /** The commands and the keys, as the help dialog lists them, scrolled by `reader`. */
    private def help(look: Look, reader: Scroller.State): Node[Msg] =
      Scroller
        .view(
          HelpPane,
          Doc(
            Vector(look.heading("commands")) ++
              Commands.all.map(c => look.binding(c.name, c.about)) ++
              Vector(look.heading(""), look.heading("keys")) ++
              Keys.map((k, what) => look.binding(k, what))
          ),
          reader
        )
        .map(Msg.ModalReader(_))
        .grounded(look.modalGround)

    def view(s: State): Node[Msg] = {
      val look = Look(s.theme)
      val panel = TurnPanel(look, budget)
      val prompt = Node.editor(s.editor).onEdit(Msg.Edited(_))
      val screen = column(
        fixed(1) -> paint(look.header(s.title)),
        flex(5) -> body(s, look, panel),
        fit(3, 0.5) -> palette(s, look).fold(prompt)(p => prompt.floating(p, paletteRoute)),
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
            Vector("enter sends", "ctrl-p commands", "ctrl-b panel", "ctrl-q quit "),
            look.status
          )
        )
      ).onKey(enter)
      val title = s.turn.fold("turn")(v => s"turn ${TurnPanel.number(v.turn)}")
      val over = s.modal match {
        case None => screen
        case Some(Dialog.Turn) =>
          screen.dialog(look.modal(title), opened(s, look, panel), Some(Msg.CloseModal))
        case Some(Dialog.Help) =>
          screen.dialog(look.modal("help"), help(look, s.modalReader), Some(Msg.CloseModal))
      }
      // The hotkeys wrap the dialog, so ctrl-q still quits over it.
      over
        .onKeyFirst(hotkeys)
        .grounded(look.ground)
    }
  }
}
