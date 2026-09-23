package grit.tui.examples

import grit.tui.runtime.*
import grit.tui.runtime.std.{Std, StdApp}
import grit.tui.components.editor.Editor
import grit.tui.components.layout.*
import grit.tui.components.overlay.*
import grit.tui.components.pane.*
import grit.tui.components.widget.{ScrollPane, StatusBar}
import grit.tui.model.input.{Input, Key}
import grit.tui.model.block.Block
import grit.tui.model.select.Doc
import grit.tui.model.surface.*
import grit.tui.model.text.{StyledText, WrapCache}

/** The grit screen -- a streaming transcript, a modal over it, selection with copy --
  * driven through the component algebra, through a real terminal. `./gate` runs six
  * scripted pty scenarios against it and checks the painted result.
  *
  * The app rides the `Std` layer ([[StdApp]]): quit, resize delivery, scroll, and the
  * whole pointer/selection lifecycle are the machine's -- `Msg` here names only what is
  * genuinely this app's, and `Msg = Std | Msg` is the union the machine dispatches on.
  * What remains of the app is decisions, one hook each:
  *
  *   - **the hotkeys** (alt-m, alt-s, alt-t), bound before capture like quit;
  *   - **which modal, popup and prompt are open**, and what their routes are called;
  *   - **which panes scroll** -- one `ScrollPane` in `scrolls`, and the layer owns the
  *     track from there: the wheel over it, the thumb grab, the drag and the release;
  *   - **the layout on resize**, the status note on copy, and what a submission means.
  *
  * Nothing derived is stored. The layout is the view and speaks one vocabulary; the
  * pointer messages carry the frame they were resolved against; a pane scrolls at the
  * size it was last painted at; and the transcript's row index -- what the scrollbar's
  * geometry speaks -- is the library's, maintained by the same funnel that maintains
  * the wrap cache ([[Panes.withDoc]] reconciles it by revision), so `State` holds no
  * index, no rect, no size and no placement map:
  *
  *   - **The layout is the view, and it speaks one vocabulary.** Every region is named
  *     by the [[PaneId]] it is found by in `onInput`: `chrome.views(...)` composes the
  *     four children and paints them, and the placement map falls out of the painting --
  *     each child is blitted under its own region's name, so `at(PromptPane)` is
  *     answered by the frame rather than by a field that has to be maintained alongside
  *     it.
  *   - **One pointer message carries the frame it was resolved against.** The machine's
  *     `Std.Pointer(e, at)` and the autoscroll tick's `Std.Edge(at)` carry the same
  *     `Placements`, which is why a drag needs no remembered rects at all.
  *   - **A pane scrolls at the size it was last painted at**, which `Panes` already
  *     knows: `panes.rendered(id).size`. No `size` field either.
  *
  * The view itself is a curried `State -> (Size -> Frame)`, composed from the overlay
  * combinators: a base frame, the modal dimmed over it, the popup floated over that,
  * the caret placed from the prompt's painted rect. No wrapping happens anywhere in it
  * (rule 7): every viewport it reads was produced by `update`, and placing is
  * arithmetic.
  *
  * ```./app grit.tui.examples.Demo2```   -- `./gate` runs the six scenarios against it.
  */
object Demo2 {

  /* ---- panes, and the regions named after them ---------------------------------- */

  /** One vocabulary: the name a thing is laid out under is the name it is found by in
    * `onInput`, and both are [[PaneId]]s. A composed layout blits each child under its
    * region's name, so naming the region after the pane is what makes the pane findable
    * -- there is no second map to keep in step, which is the whole point.
    */
  private val HeaderPane = PaneId.of("header")
  private val BodyPane = PaneId.of("body")
  private val TranscriptPane = PaneId.of("transcript")
  private val Bar = PaneId.of("scrollbar")
  private val PromptPane = PaneId.of("prompt")
  private val StatusPane = PaneId.of("status")
  private val PopupPane = PaneId.of("popup")
  private val Help = PaneId.of("help")

  /** The prompt grows with its draft and the body yields to it, down to a floor.
    *
    * `Fit` asks the editor how tall its draft is and caps the answer twice: at half the
    * screen, and at whatever leaves the body its five rows. `Flex(5)` is what that
    * second cap reads -- on any screen tall enough it reserves nothing anyone wanted,
    * and on a short one it is what stops a pasted essay from squeezing the transcript
    * to nothing.
    */
  private val chrome = Stack.of(
    HeaderPane -> Region.Fixed(1),
    BodyPane -> Region.Flex(5),
    PromptPane -> Region.Fit(min = 3, upTo = 0.5),
    StatusPane -> Region.Fixed(1)
  )

  /** The transcript and its scrollbar as one thing: the split, the bar's geometry, the
    * wheel redirect and the thumb grab all live in here now, and `scrolls` below is the
    * whole of what the app has to say about them.
    */
  private val transcript =
    ScrollPane(TranscriptPane, Bar, Palette.scrollRail, Palette.scrollThumb)

  /** The modal over the transcript: it owns the geometry, the chrome and the routing
    * rule, and the app owns what goes in it -- a `TextPane` like any other.
    */
  private val HelpModal = Modal(
    "help -- try dragging out of me",
    rows = 9,
    cols = 46,
    panel = Palette.modalPanel,
    chrome = Palette.modalChrome,
    behind = Palette.modalBehind
  )

  /** The commands the completion popup offers. */
  private val commands = Vector("/help", "/stream", "/clear", "/tools", "/quit")

  private val Streamer = TimerId.of("stream")

  private val StreamMs = 300L

  /** Eight fields, and not one of them is derived: no rect, no size, no index, no
    * placement map. The machine's own fragment is the ninth, and it is the layer's, not
    * this app's.
    */
  final case class State(
      panes: Panes,
      modal: Boolean,
      streaming: Boolean,
      rev: Long,
      status: String,
      editor: Editor,
      popup: Option[Popup],
      std: Std.State
  )

  /** The app's own half of the message union. Everything the machine can say -- quit,
    * resize, scroll, the pointer's whole life -- arrives as a `Std` instead.
    */
  enum Msg {
    case ToggleModal
    case ToggleStream
    case ToggleExpand
    case Submit
    case PopupOpen
    case PopupChose(item: String)
    case Tick

    /** A press on the prompt: the note is cleared, nothing else. */
    case PromptPress
  }

  object DemoApp extends StdApp[State, Msg] {

    def init: (State, Effect[Std | Msg]) = {
      val panes = Panes
        .of(
          TextPane(TranscriptPane, Transcript.seeded, cache = WrapCache.empty(80)),
          TextPane(Help, help, cache = WrapCache.empty(40))
        )
        .focusOn(TranscriptPane)
      val state = State(
        panes = panes,
        modal = false,
        streaming = true,
        rev = 0L,
        status = "streaming",
        editor = Palette.prompt,
        popup = None,
        std = Std.State()
      )
      // No size yet -- the runtime delivers it as the first Resized, which is where the
      // panes are first wrapped. Wrapping in `view` is what rule 7 forbids.
      (state, Effect.After(Streamer, StreamMs, Msg.Tick))
    }

    /* ---- the machine's hooks ------------------------------------------------------ */

    val panes: State -> Panes = s => s.panes
    val withPanes: (State, Panes) -> State = (s, p) => s.copy(panes = p)
    val std: State -> Std.State = s => s.std
    val withStd: (State, Std.State) -> State = (s, c) => s.copy(std = c)
    val withEditor: (State, Editor) -> State = (s, e) => s.copy(editor = e)
    val withPopup: (State, Option[Popup]) -> State = (s, p) => s.copy(popup = p)

    /** The app's own half: a modal toggle, a stream toggle, an expansion, a submission,
      * a completion, and the stream's tick. No pointer case survives here.
      */
    protected val ownUpdate: (Msg, State) -> (State, Effect[Std | Msg]) = (own, state) =>
      own match {
        case Msg.ToggleModal =>
          val opened = !state.modal
          (
            state.copy(
              modal = opened,
              panes = state.panes.focusOn(if (opened) Help else TranscriptPane),
              editor = state.editor.copy(focused = !opened),
              status = ""
            ),
            Effect.NoOp
          )

        case Msg.ToggleStream =>
          val on = !state.streaming
          (
            state.copy(streaming = on, status = if (on) "streaming" else "paused"),
            if (on) { Effect.After(Streamer, StreamMs, Msg.Tick) }
            else { Effect.Cancel(Streamer) }
          )

        case Msg.ToggleExpand => (modifyDoc(state)(Transcript.toggleExpansion), Effect.NoOp)

        case Msg.PopupOpen =>
          val p = Palette.popup(commands).withQuery(state.editor.text)
          (state.copy(popup = Some(p), status = "complete"), Effect.NoOp)

        // What a choice *means* -- the list is already closed by the time this runs.
        case Msg.PopupChose(item) =>
          val editor = state.editor.copy(text = item, caret = item.length, histPos = None)
          (state.copy(editor = editor), Effect.NoOp)

        case Msg.Submit => submit(state)

        case Msg.Tick =>
          if (!state.streaming) { (state, Effect.NoOp) }
          else {
            val rev = state.rev + 1
            (
              modifyDoc(state.copy(rev = rev))(doc => Transcript.stream(doc, rev)),
              Effect.After(Streamer, StreamMs, Msg.Tick)
            )
          }

        case Msg.PromptPress => (state.copy(status = ""), Effect.NoOp)
      }

    /* ---- routing participants ------------------------------------------------------ */

    /** The app's chassis keys, bound before capture like quit always was: ctrl-q quits
      * through the layer (a hotkey may produce either half of the union), alt-m/s/t are
      * the app's own modes. A modal must not be able to eat any of them.
      */
    override val hotkeys: (State, Input) -> Option[Std | Msg] = (_, input) =>
      input match {
        case Input.Keyboard(Key.Ctrl('q')) => Some(Std.Quit)
        case Input.Keyboard(Key.Alt('m')) => Some(Msg.ToggleModal)
        case Input.Keyboard(Key.Alt('s')) => Some(Msg.ToggleStream)
        case Input.Keyboard(Key.Alt('t')) => Some(Msg.ToggleExpand)
        case _ => None
      }

    override val modal: State -> Option[(Modal, PaneId)] = s =>
      if (s.modal) { Some((HelpModal, Help)) }
      else { None }

    val onModalClose: State -> Msg = _ => Msg.ToggleModal

    override val popup: State -> Option[(Popup, PaneId)] = s => s.popup.map(p => (p, PopupPane))

    override val onChose: String -> Option[Msg] = item => Some(Msg.PopupChose(item))

    override val prompt: State -> Option[(PaneId, Editor)] = s => Some((PromptPane, s.editor))

    /** Typing, arrowing the list, dismissing it: all the layer's, and all of them stale
      * the note. The same sentence `onScroll` next door says.
      *
      * And all of them re-lay out, because the prompt's height is now one of its own
      * outputs: a draft that wrapped onto a second row takes a row from the body, and
      * the transcript wraps and scrolls at the height it was laid out at. Hit-testing,
      * the popup's anchor and the caret need nothing here -- all three read the
      * placements off the frame last painted, so they follow the box on their own.
      */
    override val onPrompt: State -> State = s => relaid(s.copy(status = ""), s.std.size)

    /** Enter submits and Tab completes -- only after every component has declined, which
      * is what keeps Enter from submitting through an open modal.
      */
    override val free: (State, Input) -> Option[Msg] = (_, input) =>
      input match {
        case Input.Keyboard(Key.Enter) => Some(Msg.Submit)
        case Input.Keyboard(Key.Tab) => Some(Msg.PopupOpen)
        case _ => None
      }

    /** The one scrollable screen this app has. Everything the thumb used to need --
      * the press, the drag, the wheel over the track, the row-to-anchor mapping -- is
      * the layer's from here.
      */
    override val scrolls: State -> Vector[ScrollPane] = _ => Vector(transcript)

    /** The screen's size -- already the paintable screen, one column narrower than the
      * terminal, because the runtime translated rule 2 before this app ever saw it --
      * laid out into both panes at the sizes they will paint at, and the transcript's
      * row index reconciled in the same pass, because wrapping width is one of the two
      * things it depends on.
      */
    val onResize: (State, Size) -> State = (state, size) => relaid(state, size)

    /** Every pane laid out at the size it will paint at.
      *
      * The same tree `view` paints, resolved once: `screen` is that tree, and `nest` is
      * what `views` already did for painting, so the layout is described here and
      * nowhere else. It is `screen(state).placed` rather than `chrome.resolve` because
      * the prompt is a `Region.Fit` now -- how tall it is is a question only the editor
      * can answer, so the resolve has to be the one holding it.
      */
    private def relaid(state: State, size: Size): State = {
      val placed = screen(state).placed(size).nest(BodyPane, transcript.split)
      val laid = HelpModal.place(size) match {
        // The modal is not a region of the stack -- it is a compositing operation, and
        // it places itself.
        case Some(rect) => state.panes.layout(Help, rect.size)
        case None => state.panes
      }
      state.copy(panes = laid.layoutIn(placed))
    }

    /** The screen as one view: the stack with a child in every region.
      *
      * A `def` of state and not a `val` because a `Fit` region is resolved against its
      * children, so the tree cannot be built once and reused -- the editor in it is the
      * measurement.
      */
    private def screen(state: State): Regions =
      chrome.views(
        headerBar(state),
        transcript.views(state.panes),
        state.editor,
        statusBar(state)
      )

    /** The only press the app still claims -- and it claims nothing but a note. */
    override val onPress: (State, Pos, Placements) -> Option[Msg] = (_, pos, at) =>
      at.at(pos) match {
        case Some((PromptPane, _)) => Some(Msg.PromptPress)
        case _ => None
      }

    override val onScroll: State -> State = s => s.copy(status = "")

    /** The copy note: what was selected, and whether the drag expired or was released. */
    override val onCopy: (State, Option[String], Boolean) -> State = (state, text, expired) => {
      val note = text match {
        case Some(t) =>
          val lines = t.count(_ == '\n') + 1
          s"copied ${t.length} chars, $lines lines" + (if (expired) " (drag left the window)"
                                                       else "")
        case None => if (expired) "drag expired" else "nothing selected"
      }
      state.copy(status = note)
    }

    /* ---- the transcript ------------------------------------------------------ */

    /** The only place the transcript's document changes: `withDoc` re-wraps the pane at
      * the size it was last painted at and reconciles its row index in the same pass --
      * the recorded pane rule, enforced by the only code path that can change content
      * rather than by a caller remembering to follow `layout`.
      */
    private def modifyDoc(state: State)(f: Doc => Doc): State =
      state.copy(panes = state.panes.modify(TranscriptPane)(f))

    /** The slash commands that *are* a message the app already has. A table rather than
      * three hand-written transitions: `/help` and alt-m had grown two different ideas of
      * what opening the modal does -- one forced it open and left the note standing, the
      * other toggled and cleared it -- which is what two spellings of one transition do
      * given time. `/clear` and `/quit` stay written out below; they genuinely are their
      * own transitions.
      */
    private val slash: Map[String, Msg] =
      Map("/stream" -> Msg.ToggleStream, "/tools" -> Msg.ToggleExpand, "/help" -> Msg.ToggleModal)

    /** The slash commands. What a submission means is the app's, never the editor's. */
    private def command(
        draft: String,
        cleared: Editor,
        state: State
    ): (State, Effect[Std | Msg]) = {
      val typed = state.copy(editor = cleared)
      draft.trim match {
        case "/clear" =>
          (typed.copy(status = "cleared", panes = state.panes.clear(TranscriptPane)), Effect.NoOp)
        case "/quit" => (typed, Effect.Quit)
        case c if slash.contains(c) => ownUpdate(slash(c), typed)
        case other => (typed.copy(status = s"no such command: $other"), Effect.NoOp)
      }
    }

    /** Enter in the prompt: the draft into the transcript behind a separator, the editor
      * into history, and a clean prompt.
      */
    private def submit(state: State): (State, Effect[Std | Msg]) = {
      val draft = state.editor.text
      val cleared = state.editor.submitted
      if (draft.startsWith("/")) { command(draft, cleared, state) }
      else {
        (modifyDoc(state.copy(editor = cleared))(Transcript.submitted(_, draft)), Effect.NoOp)
      }
    }

    /* ---- view -------------------------------------------------------------------- */

    /** The whole screen as one composed [[grit.tui.components.View]], then the two overlays
      * composited over it and the caret placed -- each layer a pure `Frame -> Frame`
      * reading the frame's own placement map.
      *
      * The size this is called with is already the paintable screen -- the runtime
      * translated rule 2 at the boundary, so the frame built here is exactly what may
      * be written, edge to edge, and nothing compensates. The layout is resolved once,
      * by the painting itself: the placement map the next `onInput` will be handed is
      * read back off the frame this same map painted, and the popup and the caret
      * anchor to it. A region starved to nothing painted nothing and is absent from the
      * map -- the honest answer for what there is to anchor to.
      *
      * The overlays are not children of the stack on purpose: a modal is a compositing
      * operation, not a drawing. It dims what is behind it rather than replacing it, and
      * the popup floats over the prompt at a rect neither of them owns. Both still blit
      * under a pane name, so both are findable in the next `onInput`.
      *
      * No wrapping happens here (rule 7): every viewport this reads was produced by
      * `update`, and resolving the layout is arithmetic.
      */
    val view: State -> (Size -> Frame) = state =>
      size => {
        val base = Frame(Surface.blank(size).blit(screen(state).render(size), Pos(0, 0)))
        base
          .dimmedBy(HelpModal, state.panes.view(Help), Help, size, when = state.modal)
          .floating(state.popup.map(p => (p, PopupPane)), anchor = PromptPane, screen = size)
          .caretIn(
            PromptPane,
            box => state.editor.caretPos(box.cols, box.rows),
            when = !state.modal
          )
      }

    /* ---- chrome ------------------------------------------------------------------ */

    /** The header, whose ground is the streaming indicator: the bar itself changes
      * colour rather than a word in it changing. State carried by colour alone is the
      * thing a monochrome grid could not express -- before this, "streaming" and
      * "paused" differed by six characters at the far right of a reverse-video strip.
      */
    private def headerBar(state: State): StatusBar =
      StatusBar(
        Vector(" grit.tui -- the grit screen"),
        Vector(if (state.streaming) "streaming" else "paused"),
        Palette.header(state.streaming)
      )

    private def statusBar(state: State): StatusBar = {
      val pane = state.panes.get(TranscriptPane)
      val anchor = pane.map(_.anchor) match {
        case Some(Anchor.Bottom) => "tail"
        case Some(Anchor.At(p)) => s"@${p.entry}"
        case None => "-"
      }
      val counts = pane.map(p => s"wrap ${p.cache.hits}/${p.cache.misses}").getOrElse("")
      val sel =
        state.panes.drag.map(d => s" sel=${d.selection.start} to ${d.selection.end}").getOrElse("")
      // A live drag is a machine fact, not a note: derived here rather than threaded
      // through every press.
      val live = if (state.panes.drag.isDefined) { "selecting" }
      else { state.status }
      StatusBar(
        Vector(
          " ctrl-q quit ",
          " alt-m modal ",
          " alt-s stream ",
          " alt-t tools ",
          " tab completes ",
          live + sel
        ),
        Vector(anchor, counts),
        Palette.status
      )
    }

    /* ---- content ------------------------------------------------------------------ */

    private def help: Doc =
      // The modal's own document: prose, not a transcript, so it is dressed in one ink
      // rather than in the chunk vocabulary next door.
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

    /** One painted frame, no terminal. The size arrives the same way it does at runtime
      * -- as a `Resized` message -- so there is one path by which this app learns how big
      * it is, and the non-tty snapshot exercises it too.
      */
    override def firstFrame(size: Size): Frame = {
      val (state, _) = init
      view(update(Std.Resized(size), state)._1)(size)
    }
  }

  /** `runMain grit.tui.examples.Demo2` needs a `main` here; the app's own is the real one. */
  def main(args: Array[String]): Unit = DemoApp.main(args)
}
