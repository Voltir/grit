package grit.tui.runtime

import grit.tui.components.{Node, OnInput, PaneKey}
import grit.tui.components.Node.*
import grit.tui.components.editor.Editor
import grit.tui.components.overlay.{Modal, Popup}
import grit.tui.components.pane.{Anchor, Scroller}
import grit.tui.model.block.Block
import grit.tui.model.input.{Button, Input, Key, Mods, MouseEvent, MouseKind}
import grit.tui.model.select.{Doc, DocPos}
import grit.tui.model.surface.{Pos, Size, Style}
import utest.*

/** Input routed against the painted tree, through [[Headless]]: two scrolling columns
  * over a growing prompt, with a completion list and a modal when asked for. Each case
  * is a behavior the std layer's tests pinned, restated against the tree.
  */
object RouteTests extends TestSuite {

  final case class S(
      a: Scroller.State = Scroller.init,
      b: Scroller.State = Scroller.init,
      help: Scroller.State = Scroller.init,
      ed: Editor = Editor("", 0, focused = true),
      popup: Option[Popup] = None,
      modal: Boolean = false,
      copied: Vector[(String, Boolean)] = Vector.empty,
      said: Vector[String] = Vector.empty,
      focusA: Boolean = false
  )

  enum M extends caps.Pure {
    case A(m: Scroller.Msg)
    case B(m: Scroller.Msg)
    case Help(m: Scroller.Msg)
    case Ed(e: Editor)
    case Pop(p: Option[Popup])
    case Said(what: String)
    case Close
  }

  private val KA = PaneKey.of("a")
  private val KB = PaneKey.of("b")
  private val Bar = Some((Style.plain, Style.plain))
  private val HelpModal = Modal("help", rows = 6, cols = 30)

  private def prose(tag: String): Doc =
    Doc(Vector.tabulate(60)(i => Block.Text(s"$tag$i: some words that fill the column")))

  private val docA = prose("a")
  private val docB = prose("b")

  object Bench extends App[S, M] {
    def init: (S, Effect[M]) = (S(), Effect.NoOp)

    def update(m: M, s: S): (S, Effect[M]) = m match {
      case M.A(Scroller.Msg.Copied(t, x)) =>
        (s.copy(copied = s.copied :+ (t, x)), Effect.CopyOut(t))
      case M.B(Scroller.Msg.Copied(t, x)) =>
        (s.copy(copied = s.copied :+ (t, x)), Effect.CopyOut(t))
      case M.A(x) => (s.copy(a = Scroller.update(x, s.a)), Effect.NoOp)
      case M.B(x) => (s.copy(b = Scroller.update(x, s.b)), Effect.NoOp)
      case M.Help(x) => (s.copy(help = Scroller.update(x, s.help)), Effect.NoOp)
      case M.Ed(e) => (s.copy(ed = e, popup = s.popup.map(_.withQuery(e.text))), Effect.NoOp)
      case M.Pop(p) => (s.copy(popup = p), Effect.NoOp)
      case M.Said(w) => (s.copy(said = s.said :+ w), Effect.NoOp)
      case M.Close => (s.copy(modal = false), Effect.NoOp)
    }

    private def hotkeys: OnInput[M] = {
      case Input.Keyboard(Key.Ctrl('h')) => Some(M.Said("hotkey"))
      case _ => None
    }

    private def free: OnInput[M] = {
      case Input.Keyboard(Key.Enter) => Some(M.Said("enter"))
      case _ => None
    }

    private def chosen: Popup.Route -> Option[M] = {
      case Popup.Route.Stay(p) => Some(M.Pop(Some(p)))
      case Popup.Route.Chose(item) => Some(M.Said(s"chose $item"))
      case Popup.Route.Dismissed => Some(M.Pop(None))
      case Popup.Route.Pass(_) => None
    }

    def view(s: S): Node[M] = {
      val prompt: Node[M] = {
        val e = Node.editor(if (s.focusA) s.ed.copy(focused = false) else s.ed).onEdit(M.Ed(_))
        s.popup.fold[Node[M]](e)(p => e.floating(p, chosen))
      }
      val base = column(
        flex(4) -> row(
          flex() -> Scroller
            .view(KA, docA, s.a, bar = Bar, focused = s.focusA)
            .map(M.A(_)),
          flex() -> Scroller.view(KB, docB, s.b, bar = Bar).map(M.B(_))
        ),
        fit(3, 0.5) -> prompt
      ).onKey(free)
      val framed =
        if (!s.modal) base
        else
          base.dialog(
            HelpModal,
            Scroller.view(PaneKey.of("help"), prose("h"), s.help).map(M.Help(_)),
            Some(M.Close)
          )
      // Hotkeys wrap the dialog: precedence is position, so they still run under it.
      framed.onKeyFirst(hotkeys)
    }
  }

  private val size = Size(20, 60)

  private def start(s: S = S()): Headless[S, M] =
    Headless(Bench, size, Loop.start[S, M](s), Vector.empty).repainted

  private def mouse(kind: MouseKind, row: Int, col: Int, button: Button = Button.Left): Input =
    Input.Mouse(
      MouseEvent(
        kind,
        if (kind == MouseKind.Release) Button.None else button,
        Pos(row, col),
        Mods.none
      )
    )

  private def wheelUp(row: Int, col: Int): Input = mouse(MouseKind.Wheel, row, col, Button.WheelUp)

  private def key(k: Key): Input = Input.Keyboard(k)

  private def typed(h: Headless[S, M], text: String): Headless[S, M] =
    h.inputs(text.map(c => key(Key.Printable(c)))*)

  /** The rows pane `k`'s text was painted into. */
  private def paneRows(h: Headless[S, M], k: PaneKey): Int =
    h.repainted.loop.painted.pane(k).map(_.text.rows).getOrElse(-1)

  val tests = Tests {
    test("the wheel scrolls the column it is over and leaves the other alone") {
      val h = start().input(wheelUp(5, 10))
      assert(h.state.a.anchor != Anchor.Bottom, h.state.b.anchor == Anchor.Bottom)
    }

    test("the wheel over a track scrolls the pane that track belongs to") {
      // Column a's scrollbar is its last column: 29.
      val h = start().input(wheelUp(5, 29))
      assert(h.state.a.anchor != Anchor.Bottom, h.state.b.anchor == Anchor.Bottom)
    }

    test("a thumb dragged into the other column keeps scrolling the pane it grabbed") {
      val h = start().inputs(mouse(MouseKind.Press, 15, 29), mouse(MouseKind.Drag, 2, 45))
      assert(h.state.a.anchor != Anchor.Bottom, h.state.b.anchor == Anchor.Bottom)
      assert(h.state.a.selection.isEmpty) // a thumb drag is not a text drag
    }

    test("a drag parked below its column autoscrolls that column and no other") {
      val a0 = Anchor.At(DocPos(20, 0))
      val parked = start(S(a = Scroller.State(a0)))
        .inputs(mouse(MouseKind.Press, 5, 5), mouse(MouseKind.Drag, 18, 5))
      assert(parked.timers.contains(Timer.Arm(Tick.Autoscroll, Route.AutoscrollMs)))
      val ticked = parked.repainted.tick(Tick.Autoscroll)
      assert(ticked.state.a.anchor != a0, ticked.state.b.anchor == Anchor.Bottom)
      assert(ticked.state.b.selection.isEmpty) // one pointer: one column selects
    }

    test("a tick with no drag live stops the autoscroll") {
      assert(start().tick(Tick.Autoscroll).timers == Vector(Timer.Disarm(Tick.Autoscroll)))
    }

    test("a press and release in one place copies nothing") {
      val h = start().inputs(mouse(MouseKind.Press, 5, 5), mouse(MouseKind.Release, 5, 5))
      assert(
        h.state.copied.isEmpty,
        !h.effects.exists { case Effect.CopyOut(_) => true; case _ => false }
      )
    }

    test("the deadline ends an escaped drag, copies it, and says it expired") {
      val h = start()
        .inputs(mouse(MouseKind.Press, 5, 2), mouse(MouseKind.Drag, 7, 20))
        .tick(Tick.Deadline)
      assert(h.state.copied.length == 1, h.state.copied.head._2)
      assert(h.state.a.selection.isEmpty)
    }

    test("the modal captures: nothing beneath it sees a key, a press or the wheel") {
      val open = start(S(modal = true))
      val poked = typed(open, "x").inputs(wheelUp(1, 2), mouse(MouseKind.Press, 1, 2))
      assert(poked.state.ed.text == "", poked.state.a == Scroller.init)
      assert(open.input(key(Key.Escape)).state.modal == false)
    }

    test("hotkeys that wrap the dialog run while it is open") {
      assert(start(S(modal = true)).input(key(Key.Ctrl('h'))).state.said == Vector("hotkey"))
    }

    test("the editor takes what edits text; Enter bubbles up to the screen") {
      val h = typed(start(), "hi").input(key(Key.Enter))
      assert(h.state.ed.text == "hi", h.state.said == Vector("enter"))
    }

    test("the popup claims its keys and lets typing through to the editor") {
      val listed = start(S(popup = Some(Popup.of("/help", "/stream", "/stop"))))
      val narrowed = typed(listed, "/st")
      assert(narrowed.state.ed.text == "/st", narrowed.state.popup.map(_.matches.length) == Some(2))
      val chose = narrowed.inputs(key(Key.Down(Mods.none)), key(Key.Enter))
      assert(chose.state.said == Vector("chose /stop")) // Enter was the popup's, not the screen's
      assert(listed.input(key(Key.Escape)).state.popup.isEmpty)
    }

    test("page keys page the focused pane by its painted height less two rows") {
      val focused = start(S(a = Scroller.State(Anchor.At(DocPos(20, 0))), focusA = true))
      val top0 = focused.repainted.loop.painted.pane(KA).map(_.top)
      val paged = focused.repainted.input(key(Key.PageUp(Mods.none))).repainted
      val rows = paneRows(paged, KA)
      assert(top0.zip(paged.loop.painted.pane(KA).map(_.top)).exists((a, b) => a - b == rows - 2))
    }

    test("a draft that wraps takes rows from the body, and gives them back") {
      val h0 = start()
      val grown = typed(h0, "word " * 40)
      assert(paneRows(grown, KA) < paneRows(h0, KA))
      val cleared = grown.message(M.Ed(Editor("", 0, focused = true)))
      assert(paneRows(cleared, KA) == paneRows(h0, KA))
    }

    test("the prompt stops at half the screen however long the draft runs") {
      val h = typed(start(), "word " * 400)
      assert(paneRows(h, KA) >= size.rows / 2 - 1)
    }

    test("the caret is placed in the editor's painted box, and a modal hides it") {
      val (frame, _) = typed(start(), "hi").painted
      assert(frame.cursor.exists(p => p.row >= size.rows - 3 && p.col > 0))
      assert(start(S(modal = true)).painted._1.cursor.isEmpty)
    }

    test("a popup with nothing matched paints nothing") {
      val none = typed(start(S(popup = Some(Popup.of("/help")))), "zz")
      assert(!none.screen.exists(_.contains("/help")))
      assert(start(S(popup = Some(Popup.of("/help")))).screen.exists(_.contains("/help")))
    }
  }
}
