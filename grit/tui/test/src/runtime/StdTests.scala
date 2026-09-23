package grit.tui.runtime

import utest.*

import grit.tui.runtime.std.{Ambient, Scrolling, Std, StdApp, StdBase}
import grit.tui.components.editor.Editor
import grit.tui.components.overlay.{Modal, Popup}
import grit.tui.components.pane.{Anchor, Panes, TextPane}
import grit.tui.model.input.{Button, Input, Key, Mods, MouseKind, MouseEvent}
import grit.tui.model.select.{Doc, DocPos}
import grit.tui.model.surface.{Frame, PaneId, Placement, Placements, Pos, Rect, Size, Surface}
import grit.tui.model.text.WrapCache

/** The `Std` machine, asserted against returned state and effects -- pure, no terminal.
  *
  * The hooks are pure by type, so a hook cannot record on the side: every observation
  * here is a field of the test app's own `State`, which is the same constraint a real
  * app lives under and the same one `RuntimeTests` observes `update` through.
  */
object StdTests extends TestSuite {

  private val Body = PaneId.of("body")
  private val Track = PaneId.of("track")
  private val Pop = PaneId.of("pop")

  private enum Own {
    case Ping
    case Chose(item: String)
    case Submitted
    case Grab(pos: Pos, at: Placements)
  }

  /** Panes, the machine's fragment, and what the hooks chose to record. */
  final case class S(
      panes: Panes,
      std: Std.State = Std.State(),
      note: String = "",
      pop: Option[Popup] = None,
      modalOpen: Boolean = false,
      editor: Option[Editor] = None
  )

  /** A `lines` document shown whole in a 6-row pane, each line short enough not to wrap. */
  private def stateOf(lines: String*): S = {
    val panes = Panes
      .of(TextPane(Body, Doc.of(lines*), cache = WrapCache.empty(40)))
      .layout(Body, Size(6, 39))
    S(panes = panes.focusOn(Body))
  }

  private def st(modalOpen: Boolean = false, editor: Option[Editor] = None): S =
    stateOf(Ten*).copy(modalOpen = modalOpen, editor = editor)

  private val Ten = Vector("a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9")

  /** Where the last frame put things: the body at the origin, the thumb track beside
    * it. (The popup's placement lives only in `atPop`: it is painted over the body and
    * hit-testing finds the topmost, so leaving it in every map would make every press
    * land on a pane that has no document.)
    */
  private val at = Placements(
    Vector(
      Placement(Body, Rect(0, 0, 6, 39)),
      Placement(Track, Rect(0, 39, 6, 1))
    )
  )

  private val atPop = Placements(
    Vector(
      Placement(Body, Rect(0, 0, 6, 39)),
      Placement(Track, Rect(0, 39, 6, 1)),
      Placement(Pop, Rect(0, 0, 6, 39))
    )
  )

  /** A press in the pane's last row claims -- the scrollbar-thumb shape.
    *
    * A trait rather than an object so a variant can recompose the routing chain and be
    * identical in every other respect; `App` is the default chain, `NoModal` drops one
    * layer out of it.
    */
  private trait TestApp extends StdApp[S, Own] {

    val panes: S -> Panes = s => s.panes
    val withPanes: (S, Panes) -> S = (s, p) => s.copy(panes = p)
    val std: S -> Std.State = s => s.std
    val withStd: (S, Std.State) -> S = (s, c) => s.copy(std = c)
    val withEditor: (S, Editor) -> S = (s, e) => s.copy(editor = Some(e))
    val withPopup: (S, Option[Popup]) -> S = (s, p) => s.copy(pop = p)

    protected val ownUpdate: (Own, S) -> (S, Effect[Std | Own]) = (own, state) =>
      own match {
        case Own.Ping => (state.copy(note = "pinged"), Effect.NoOp)
        case Own.Chose(item) => (state.copy(note = s"chose:$item"), Effect.NoOp)
        case Own.Submitted => (state.copy(note = "submitted"), Effect.NoOp)
        case Own.Grab(pos, _) =>
          (withStd(state, std(state).copy(grabbing = true, pointer = pos)), Effect.NoOp)
      }

    val onModalClose: S -> Own = _ => Own.Ping
    override val onChose: String -> Option[Own] = item => Some(Own.Chose(item))

    override val free: (S, Input) -> Option[Own] = (_, input) =>
      input match {
        case Input.Keyboard(Key.Enter) => Some(Own.Submitted)
        case _ => None
      }

    override val hotkeys: (S, Input) -> Option[Std | Own] = (_, input) =>
      input match {
        case Input.Keyboard(Key.Ctrl('x')) => Some(Own.Ping)
        case _ => None
      }

    val onResize: (S, Size) -> S = (s, size) => s.copy(note = s"resized:${size.rows}x${size.cols}")
    override val onPress: (S, Pos, Placements) -> Option[Own] = (_, pos, at) =>
      if (pos.row == 5) { Some(Own.Grab(pos, at)) }
      else { None }
    override val onGrab: (S, Pos, Placements) -> S = (s, pos, _) =>
      s.copy(note = s"grab:${pos.col}")
    override val onScroll: S -> S = s => s.copy(note = "scrolled")
    override val onPrompt: S -> S = s => s.copy(note = "prompted")
    override val onCopy: (S, Option[String], Boolean) -> S = (s, text, expired) =>
      s.copy(note = s"copied:${text.getOrElse("-")}:${
          if (expired) { "expired" }
          else { "released" }
        }")
    override val wheelTo: (S, PaneId) -> Option[PaneId] = (_, id) =>
      if (id == Track) { Some(Body) }
      else { None }

    override val modal: S -> Option[(Modal, PaneId)] = s =>
      if (s.modalOpen) { Some((Modal("m", 5, 20), Body)) }
      else { None }
    override val prompt: S -> Option[(PaneId, Editor)] = s => s.editor.map(e => (Body, e))
    override val popup: S -> Option[(Popup, PaneId)] = s => s.pop.map(p => (p, Pop))

    // Unused by these tests -- they call `update` and `onInput` directly -- but the App
    // contract wants both, and a stub keeps the test honest about which half it drives.
    def init: (S, Effect[Std | Own]) = (stateOf(Ten*), Effect.NoOp)
    def view: S -> (Size -> Frame) = _ => size => Frame(Surface.blank(size))
  }

  private object App extends TestApp

  /** The same app with the modal taken out of the chain -- the composition point, used
    * as one: nothing else changes, and a modal stops capturing.
    */
  private object NoModal extends TestApp {
    override val layers: Vector[Layer] =
      Vector(hotkeyLayer, ambientLayer, popupLayer, promptLayer, freeLayer, pageLayer, pointerLayer)
  }

  /** The point of the mixin split, as a compiling fact.
    *
    * Three traits, and the members it does *not* supply are the argument: `onModalClose`,
    * `withEditor` and `withPopup` are abstract on `Modals`, `Prompting` and `Completing`,
    * so the indivisible `StdApp` made every app answer them -- an app with no modal still
    * had to say what closing its modal meant. A trait an app does not mix asks it nothing,
    * and this object failing to compile is what that claim looks like when it stops being
    * true.
    */
  private object Minimal extends StdBase[S, Own] with Ambient[S, Own] with Scrolling[S, Own] {
    val panes: S -> Panes = s => s.panes
    val withPanes: (S, Panes) -> S = (s, p) => s.copy(panes = p)
    val std: S -> Std.State = s => s.std
    val withStd: (S, Std.State) -> S = (s, c) => s.copy(std = c)
    protected val ownUpdate: (Own, S) -> (S, Effect[Std | Own]) =
      (_, s) => (s.copy(note = "own"), Effect.NoOp)
    val onResize: (S, Size) -> S = (s, size) => s.copy(note = s"resized:${size.rows}")

    val layers: Vector[Layer] = Vector(ambientLayer, pageLayer)
    val steps: Vector[Step] = Vector(ambientStep, scrollStep)

    def init: (S, Effect[Std | Own]) = (stateOf(Ten*), Effect.NoOp)
    def view: S -> (Size -> Frame) = _ => size => Frame(Surface.blank(size))
  }

  private def key(k: Key): Input = Input.Keyboard(k)

  private def mouse(kind: MouseKind, pos: Pos, shift: Boolean = false): MouseEvent =
    MouseEvent(kind, Button.Left, pos, Mods(shift, false, false))

  private def wheel(pos: Pos, up: Boolean): MouseEvent =
    MouseEvent(
      MouseKind.Wheel,
      if (up) { Button.WheelUp }
      else { Button.WheelDown },
      pos,
      Mods.none
    )

  private def mInput(kind: MouseKind, pos: Pos, shift: Boolean): Input =
    Input.Mouse(mouse(kind, pos, shift))

  val tests = Tests {

    test("quit is nothing but Effect.Quit") {
      val (s, e) = App.update(Std.Quit, stateOf("alpha"))
      assert(e == Effect.Quit)
      assert(s == stateOf("alpha"))
    }

    test("a resize reaches the app's layout and nowhere else") {
      val (s, e) = App.update(Std.Resized(Size(10, 50)), stateOf("alpha"))
      assert(e == Effect.NoOp)
      assert(s.note == "resized:10x50")
    }

    test("scroll, to-top and to-bottom move the focused pane only") {
      val s0 = stateOf(Ten*)
      val (s1, e1) = App.update(Std.Scroll(-2), s0)
      assert(e1 == Effect.NoOp)
      assert(s1.note == "scrolled")
      assert(s1.panes.rendered(Body).top != s0.panes.rendered(Body).top)

      val (s2, _) = App.update(Std.ToTop, s1)
      assert(s2.panes.get(Body).exists(_.anchor == Anchor.At(DocPos(0, 0))))

      val (s3, _) = App.update(Std.ToBottom, s2)
      assert(s3.panes.get(Body).exists(_.anchor == Anchor.Bottom))
    }

    test("with no focus there is nothing to scroll and nothing annotated") {
      val s0 = stateOf(Ten*).copy(panes =
        Panes
          .of(TextPane(Body, Doc.of(Ten*), cache = WrapCache.empty(40)))
          .layout(Body, Size(6, 39))
      )
      val (s1, e) = App.update(Std.Scroll(-2), s0)
      assert(e == Effect.NoOp)
      assert(s1 == s0)
    }

    test("press arms the deadline, drag extends the selection, release copies exactly") {
      val pressed =
        App.update(Std.Pointer(mouse(MouseKind.Press, Pos(0, 0)), at), stateOf("alpha", "beta"))._1
      assert(pressed.std.pointer == Pos(0, 0))
      assert(pressed.std.edge == 0)
      assert(pressed.panes.drag.isDefined)
      assert(
        App
          .update(Std.Pointer(mouse(MouseKind.Press, Pos(0, 0)), at), stateOf("alpha", "beta"))
          ._2 ==
          Effect.batch(
            Effect.Cancel(Std.Autoscroll),
            Effect.After(Std.DragEnd, 1500L, Std.Deadline)
          )
      )

      val (draggedState, draggedEffect) =
        App.update(Std.Pointer(mouse(MouseKind.Drag, Pos(1, 5)), at), pressed)
      assert(draggedState.panes.drag.isDefined)
      assert(
        draggedEffect == Effect.batch(
          Effect.Cancel(Std.Autoscroll),
          Effect.After(Std.DragEnd, 1500L, Std.Deadline)
        )
      )

      val (releasedState, releasedEffect) =
        App.update(Std.Pointer(mouse(MouseKind.Release, Pos(1, 5)), at), draggedState)
      assert(
        releasedEffect == Effect.batch(
          Effect.Cancel(Std.Autoscroll),
          Effect.Cancel(Std.DragEnd),
          Effect.CopyOut("alpha\nbeta")
        )
      )
      assert(releasedState.note == "copied:alpha\nbeta:released")
      assert(releasedState.panes.drag.isEmpty)
      assert(releasedState.std.edge == 0)
    }

    test("a plain release with nothing selected copies nothing") {
      val (s, e) =
        App.update(Std.Pointer(mouse(MouseKind.Release, Pos(0, 0)), at), stateOf("alpha"))
      assert(e == Effect.batch(Effect.Cancel(Std.Autoscroll), Effect.Cancel(Std.DragEnd)))
      assert(s.note == "copied:-:released")
    }

    test("the deadline releases an escaped drag and says so") {
      val pressed =
        App.update(Std.Pointer(mouse(MouseKind.Press, Pos(0, 0)), at), stateOf("alpha", "beta"))._1
      val dragged = App.update(Std.Pointer(mouse(MouseKind.Drag, Pos(1, 5)), at), pressed)._1
      val (s, e) = App.update(Std.Deadline, dragged)
      assert(
        e == Effect.batch(
          Effect.Cancel(Std.Autoscroll),
          Effect.Cancel(Std.DragEnd),
          Effect.CopyOut("alpha\nbeta")
        )
      )
      assert(s.note == "copied:alpha\nbeta:expired")
    }

    test("a tick with nothing live cancels the autoscroll and zeroes the edge") {
      val (s, e) = App.update(Std.Edge(at), stateOf(Ten*))
      assert(e == Effect.Cancel(Std.Autoscroll))
      assert(s.std.edge == 0)
    }

    test("a tick that moves the view re-arms both timers") {
      val pressed = App.update(Std.Pointer(mouse(MouseKind.Press, Pos(0, 0)), at), stateOf(Ten*))._1
      // Parked above the pane's top row: the pointer pulls the view back through the
      // document, so the scroll has somewhere to go.
      val dragged = App.update(Std.Pointer(mouse(MouseKind.Drag, Pos(-1, 0)), at), pressed)._1
      assert(dragged.std.edge == -1)

      val (s2, e) = App.update(Std.Edge(at), dragged)
      assert(
        e == Effect.batch(
          Effect.After(Std.Autoscroll, 45L, Std.Edge(at)),
          Effect.After(Std.DragEnd, 1500L, Std.Deadline)
        )
      )
      assert(s2.panes.rendered(Body).top != dragged.panes.rendered(Body).top)
    }

    test("a tick that moves nothing re-arms the autoscroll but not the deadline") {
      val pressed = App.update(Std.Pointer(mouse(MouseKind.Press, Pos(0, 0)), at), stateOf(Ten*))._1
      // Parked below the pane's bottom row: pulling forward, but the tail is already on
      // screen and the pane is anchored at the bottom, so the scroll moves nothing.
      val dragged = App.update(Std.Pointer(mouse(MouseKind.Drag, Pos(9, 0)), at), pressed)._1
      assert(dragged.std.edge == 1)

      val (_, e) = App.update(Std.Edge(at), dragged)
      assert(e == Effect.After(Std.Autoscroll, 45L, Std.Edge(at)))
    }

    test("the wheel scrolls the hit pane; a redirect sends the track's wheel to its pane") {
      val s0 = stateOf(Ten*)
      val (s1, e1) = App.update(Std.Pointer(wheel(Pos(0, 0), up = true), at), s0)
      assert(e1 == Effect.NoOp)
      assert(s1.note == "scrolled")
      assert(s1.panes.rendered(Body).top != s0.panes.rendered(Body).top)

      val s2 = stateOf(Ten*)
      val (s3, e3) = App.update(Std.Pointer(wheel(Pos(2, 39), up = true), at), s2)
      assert(e3 == Effect.NoOp)
      assert(s3.note == "scrolled")
      assert(s3.panes.rendered(Body).top != s2.panes.rendered(Body).top)
    }

    test("a claimed press defers entirely; its drags and release come back as grabs") {
      val (s1, e1) = App.update(Std.Pointer(mouse(MouseKind.Press, Pos(5, 0)), at), stateOf(Ten*))
      assert(e1 == Effect.NoOp)
      assert(s1.std.grabbing)
      assert(s1.std.pointer == Pos(5, 0))
      assert(s1.panes.drag.isEmpty)

      val (s2, e2) = App.update(Std.Pointer(mouse(MouseKind.Drag, Pos(5, 3)), at), s1)
      assert(e2 == Effect.NoOp)
      assert(s2.note == "grab:3")
      assert(s2.std.pointer == Pos(5, 3))

      val (s3, e3) = App.update(Std.Pointer(mouse(MouseKind.Release, Pos(5, 3)), at), s2)
      assert(e3 == Effect.NoOp)
      assert(!s3.std.grabbing)
    }

    test("hotkeys are bound before capture: the modal cannot eat them") {
      assert(App.onInput(key(Key.Ctrl('x')), st(modalOpen = true), at) == Some(Own.Ping))
    }

    test("the modal's capture routes scroll and close; nothing passes through") {
      assert(App.onInput(key(Key.Escape), st(modalOpen = true), at) == Some(Own.Ping))
      assert(App.onInput(key(Key.Up()), st(modalOpen = true), at) == Some(Std.Scroll(-1)))
      assert(App.onInput(key(Key.PageUp()), st(modalOpen = true), at) == Some(Std.Scroll(-6)))
      // A printable character means nothing to the modal and must not reach the prompt.
      assert(App.onInput(key(Key.Printable('q')), st(modalOpen = true), at) == None)
    }

    test("the terminal's shift-drag is nobody's, modal or not") {
      assert(App.onInput(mInput(MouseKind.Drag, Pos(1, 1), shift = true), st(), at) == None)
      assert(
        App.onInput(
          mInput(MouseKind.Drag, Pos(1, 1), shift = true),
          st(modalOpen = true),
          at
        ) == None
      )
    }

    test("a resize is ambient even behind an open modal") {
      assert(
        App.onInput(Input.Resize(Size(9, 60)), st(modalOpen = true), at) ==
          Some(Std.Resized(Size(9, 60)))
      )
    }

    test("a modal before its first frame swallows, it does not fall through") {
      // `at` holds no placement for the modal's pane, so there is nothing to route
      // against. Enter is the discriminating input: the free keys below would claim it,
      // and an input the modal must eat does not become the app's by arriving early.
      val s0 = st(modalOpen = true)
      assert(App.onInput(key(Key.Enter), s0, Placements(Vector())) == None)
      // The same input, once the modal has a rect, is the modal's to swallow too.
      assert(App.onInput(key(Key.Printable('a')), s0, at) == None)
    }

    test("the chain is a value: an app that drops a layer drops what it claimed") {
      val s0 = st(modalOpen = true, editor = Some(Editor("hi", 2)))
      // With the modal in the chain it captures, and Enter never reaches the free keys.
      assert(App.onInput(key(Key.Enter), s0, at) == None)
      // Without it, the same input walks on to the prompt and the free keys.
      assert(NoModal.onInput(key(Key.Enter), s0, at) == Some(Own.Submitted))
      assert(NoModal.onInput(key(Key.Printable('a')), s0, at) == Some(Std.EditTo(Editor("hia", 3))))
    }

    test("a layer rewrites what the rest of the chain sees") {
      // The popup hands typing on rather than eating it, and the prompt below claims the
      // input the popup passed -- the re-entrant call that used to be, said as data.
      val s0 = st(editor = Some(Editor("/", 1))).copy(pop = Some(Popup.of("/a", "/b")))
      assert(App.onInput(key(Key.Printable('b')), s0, atPop) == Some(Std.EditTo(Editor("/b", 2))))
    }

    test("the popup claims its keys and passes typing through") {
      val s0 = st().copy(pop = Some(Popup.of("/a", "/b")))
      // Moving the selection and dismissing are the layer's own, said as `Std`: an app
      // that mirrored these into its own messages would be re-deriving mechanism.
      assert(App.onInput(key(Key.Up()), s0, atPop).exists(_.isInstanceOf[Std.PopupTo]))
      assert(App.onInput(key(Key.Escape), s0, atPop) == Some(Std.PopupTo(None)))
      // Typing is not the popup's: it passes to the prompt, which is absent here, and
      // then to the free keys, which have no opinion about 'q'.
      assert(App.onInput(key(Key.Printable('q')), s0, atPop) == None)
    }

    test("dismissing the popup closes it, through the layer") {
      val s0 = st().copy(pop = Some(Popup.of("/a", "/b")), note = "complete")
      val (s1, e) = App.update(Std.PopupTo(None), s0)
      assert(s1.pop.isEmpty)
      assert(e == Effect.NoOp)
      // ...and the prompt hook ran, which is where an app hangs its own chrome.
      assert(s1.note == "prompted")
    }

    test("a chosen item closes the list before the app is told what it meant") {
      val s0 = st().copy(pop = Some(Popup.of("/a", "/b")))
      val (s1, _) = App.update(Std.Chose("/b"), s0)
      assert(s1.pop.isEmpty)
      assert(s1.note == "chose:/b")
    }

    test("the editor consumes what it means to edit text; Enter is the app's") {
      val s0 = st(editor = Some(Editor("hi", 2)))
      assert(App.onInput(key(Key.Printable('a')), s0, at) == Some(Std.EditTo(Editor("hia", 3))))
      assert(App.onInput(key(Key.Enter), s0, at) == Some(Own.Submitted))
    }

    test("an edit is stored by the layer, and re-filters the list typed under it") {
      val s0 = st(editor = Some(Editor("/", 1))).copy(pop = Some(Popup.of("/aa", "/bb")))
      val (s1, _) = App.update(Std.EditTo(Editor("/b", 2)), s0)
      assert(s1.editor.contains(Editor("/b", 2)))
      // The list narrowed to what is still reachable: a popup that did not would be
      // offering the wrong things.
      assert(s1.pop.map(_.visible) == Some(Vector("/bb")))
      assert(s1.note == "prompted")
    }

    test("page keys page the focused pane's painted height less two rows") {
      val s0 = st()
      assert(App.onInput(key(Key.PageUp()), s0, at) == Some(Std.Scroll(-4)))
      assert(App.onInput(key(Key.PageDown()), s0, at) == Some(Std.Scroll(4)))
    }

    test("an unclaimed mouse report is one Std.Pointer carrying the frame") {
      val e = MouseEvent(MouseKind.Drag, Button.Left, Pos(1, 1), Mods.none)
      assert(App.onInput(Input.Mouse(e), st(), at) == Some(Std.Pointer(e, at)))
    }

    test("an app may mix three traits and owe the layer nothing it has no use for") {
      // The compile is half the assertion; these are the other half -- what it can still
      // do. A resize and a page reach it, and Enter is nobody's, because it mixed no
      // `FreeKeys` to claim it and no editor to type into.
      val (resized, _) = Minimal.update(Std.Resized(Size(9, 20)), st())
      assert(resized.note == "resized:9")

      assert(Minimal.onInput(key(Key.PageUp(Mods.none)), st(), at) == Some(Std.Scroll(-4)))
      assert(Minimal.onInput(key(Key.Enter), st(), at) == None)

      // A message no step claims changes nothing rather than reaching the app's half.
      val s0 = st()
      val (after, eff) = Minimal.update(Std.Deadline, s0)
      assert(after == s0)
      assert(eff == Effect.NoOp)
    }

    /** The compiler used to check this: `stdUpdate` was one exhaustive match over `Std`,
      * and a new case without a handler would not build. A `Vector[Step]` cannot be
      * checked that way, so the check is rebuilt here out of parts that can be -- and it
      * stays a compile error rather than becoming a convention.
      *
      * `label` is an exhaustive match, so a new `Std` case fails to compile until it is
      * named here; the count then fails until it has a sample; and the claim assertion
      * fails until some step in the default chain answers it. Three links, each one
      * mechanical. That is the trade the split was made with eyes open.
      */
    test("every Std case is claimed by some step in the default chain") {
      def label(m: Std): String = m match {
        case Std.Resized(_) => "Resized"
        case Std.Pointer(_, _) => "Pointer"
        case Std.Edge(_) => "Edge"
        case Std.Deadline => "Deadline"
        case Std.Scroll(_) => "Scroll"
        case Std.ToTop => "ToTop"
        case Std.ToBottom => "ToBottom"
        case Std.EditTo(_) => "EditTo"
        case Std.PopupTo(_) => "PopupTo"
        case Std.Chose(_) => "Chose"
        case Std.Quit => "Quit"
      }

      val samples: Vector[Std] = Vector(
        Std.Resized(Size(6, 39)),
        Std.Pointer(mouse(MouseKind.Move, Pos(0, 0)), at),
        Std.Edge(at),
        Std.Deadline,
        Std.Scroll(1),
        Std.ToTop,
        Std.ToBottom,
        Std.EditTo(Editor("", 0)),
        Std.PopupTo(None),
        Std.Chose("x"),
        Std.Quit
      )

      // One sample per case, and no case sampled twice.
      assert(samples.map(label).distinct.length == samples.length)
      assert(samples.length == 11)

      val unclaimed = samples.filter { m =>
        val chain = App.steps
        var i = 0
        var hit = false
        while (i < chain.length && !hit) {
          if (chain(i)(m, st()).isDefined) { hit = true }
          i += 1
        }
        !hit
      }
      assert(unclaimed.map(label) == Vector.empty[String])
    }
  }
}
