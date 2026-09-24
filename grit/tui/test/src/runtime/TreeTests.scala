package grit.tui.runtime

import grit.tui.components.editor.Editor
import grit.tui.components.overlay.Modal
import grit.tui.model.block.Block
import grit.tui.model.input.{Button, Input, Mods, MouseEvent, MouseKind}
import grit.tui.components.{Node, PaneKey}
import grit.tui.components.pane.{Anchor, Scroller}
import grit.tui.model.select.{Doc, Selection}
import grit.tui.model.surface.{Pos, Rect, Size}
import grit.tui.wire.paint.{Painter, Vt}
import grit.tui.components.Node.*
import utest.*

/** The view tree against the painted grid: rules 5 and 6 read off a [[Vt]], and
  * the wrap memo's hit/miss counts (rule 7's aim) read off the loop.
  */
object TreeTests extends TestSuite {

  /** Two scrollable documents, the second in a modal when `modal`; each has its own
    * scroll and selection state, nested by `Node.map`.
    */
  final case class S(
      a: Doc,
      sa: Scroller.State,
      b: Doc,
      sb: Scroller.State,
      modal: Boolean,
      copied: Vector[String],
      ed: Editor = Editor("", 0)
  )

  enum M extends caps.Pure {
    case A(m: Scroller.Msg)
    case B(m: Scroller.Msg)
    case E(e: Editor)
  }

  val modal: Modal = Modal("m", rows = 6, cols = 30)

  /** The screen under test: a status line, document `a` over an editor, and document `b`
    * in a modal while `modal`. Each test starts it from its own state, not `init`.
    */
  object Two extends App[S, M] {
    def init: (S, Effect[M]) =
      (S(Doc.empty, Scroller.init, Doc.empty, Scroller.init, false, Vector.empty), Effect.NoOp)

    def update(m: M, s: S): (S, Effect[M]) =
      m match {
        case M.A(Scroller.Msg.Copied(t, _)) => (s.copy(copied = s.copied :+ t), Effect.CopyOut(t))
        case M.B(Scroller.Msg.Copied(t, _)) => (s.copy(copied = s.copied :+ t), Effect.CopyOut(t))
        case M.E(e) => (s.copy(ed = e), Effect.NoOp)
        case M.A(x) => (s.copy(sa = Scroller.update(x, s.sa)), Effect.NoOp)
        case M.B(x) => (s.copy(sb = Scroller.update(x, s.sb)), Effect.NoOp)
      }

    def view(s: S): Node[M] = {
      val base = column(
        fixed(1) -> paint(
          grit.tui.components.widget
            .StatusBar(Vector("head"), Vector(), grit.tui.model.surface.Style.plain)
        ),
        flex() -> Scroller
          .view(
            PaneKey.of("a"),
            s.a,
            s.sa,
            bar = Some((grit.tui.model.surface.Style.plain, grit.tui.model.surface.Style.plain))
          )
          .map(M.A(_)),
        fixed(3) -> Node.editor(s.ed).onEdit(M.E(_))
      )
      if (s.modal)
        base.dialog(
          modal,
          Scroller.view(PaneKey.of("b"), s.b, s.sb, focused = true).map(M.B(_)),
          None
        )
      else base
    }
  }

  /** Wrapped rows, wide glyphs and a separator: what rule 5 used to get wrong. */
  val prose: Doc = Doc(
    Vector.tabulate(60) { i =>
      if (i % 5 == 4) Block.Separator()
      else
        Block.Text(s"$i: 漢字 wide glyphs then plain words that wrap over the pane width, entry $i")
    }
  )
  val helpDoc: Doc = Doc.of(
    "the modal's own",
    "document, short",
    "and third line",
    "fourth",
    "fifth",
    "sixth",
    "seventh"
  )

  def mouse(kind: MouseKind, row: Int, col: Int): Input =
    Input.Mouse(
      MouseEvent(
        kind,
        if (kind == MouseKind.Release) Button.None else Button.Left,
        Pos(row, col),
        Mods.none
      )
    )

  final class Sim(s0: S, val size: Size) {
    var loop: Loop[S, M] = Loop.start(s0)
    var effects: Vector[Effect[M]] = Vector.empty
    var frame = Loop.paint(loop, Two, size)._1
    def paint(): Unit = { val (f, l) = Loop.paint(loop, Two, size); frame = f; loop = l }
    def in(i: Input): Unit = { val (l, fx, _) = Loop.input(loop, i, Two); loop = l; effects ++= fx }
    def vt: Vt = { val v = new Vt(size.rows, size.cols + 1); v.feed(Painter.paint(frame, None)); v }
    paint()
  }

  /** The reverse-video text on screen, row by row, whitespace squeezed out. */
  def highlighted(v: Vt): String =
    v.cells.map(_.filter(_.style.reverse).map(_.ch).mkString).mkString.replaceAll("\\s+", "")

  val tests = Tests {
    test("rule 5: the painted highlight is exactly the selection's text") {
      val sim = new Sim(
        S(
          prose,
          Scroller.State(anchor =
            grit.tui.components.pane.Anchor.At(grit.tui.model.select.DocPos(3, 0))
          ),
          helpDoc,
          Scroller.init,
          false,
          Vector.empty
        ),
        Size(20, 40)
      )
      sim.in(mouse(MouseKind.Press, 3, 7))
      sim.in(mouse(MouseKind.Drag, 9, 20))
      sim.in(mouse(MouseKind.Drag, 12, 11))
      sim.paint()
      val sel = sim.loop.state.sa.selection.getOrElse(Selection.empty)
      val want = prose.textOf(sel).replaceAll("\\s+", "")
      val got = highlighted(sim.vt)
      assert(want.nonEmpty, got == want)
      sim.in(mouse(MouseKind.Release, 12, 11))
      sim.paint()
      assert(
        highlighted(sim.vt).isEmpty,
        sim.loop.state.copied.map(_.replaceAll("\\s+", "")) == Vector(want)
      )
    }

    test("rule 6: a drag begun in the modal stays in the modal's document and rect") {
      val sim =
        new Sim(S(prose, Scroller.init, helpDoc, Scroller.init, true, Vector.empty), Size(24, 60))
      val body = modal.place(Size(24, 60)).getOrElse(Rect(0, 0, 0, 0))
      sim.in(mouse(MouseKind.Press, body.top, body.left + 2))
      sim.in(mouse(MouseKind.Drag, body.top + 2, body.left + 10))
      sim.in(mouse(MouseKind.Drag, 22, 58)) // far outside, over the transcript
      sim.paint()
      val v = sim.vt
      val outside = (0 until 24).exists { r =>
        (0 until 60).exists(c => v.cells(r)(c).style.reverse && !body.contains(Pos(r, c)))
      }
      assert(!outside, sim.loop.state.sa.selection.isEmpty)
      sim.in(mouse(MouseKind.Release, 22, 58))
      val copied = sim.loop.state.copied
      assert(
        copied.length == 1,
        helpDoc
          .textOf(Selection(helpDoc.clamp(grit.tui.model.select.DocPos(0, 0)), helpDoc.end))
          .contains(copied(0).take(10))
      )
      assert(!copied(0).contains("wide glyphs"))
      // A whole drag outside the modal is swallowed: no grab, no selection, no copy.
      sim.in(mouse(MouseKind.Press, 22, 5))
      assert(sim.loop.grab == Grab.Idle)
      sim.in(mouse(MouseKind.Drag, 22, 40))
      sim.in(mouse(MouseKind.Release, 22, 40))
      assert(sim.loop.state.copied.length == 1, sim.loop.state.sa.selection.isEmpty)
    }

    test("a batch of inputs between paints: each builds on the last one's result") {
      val sim =
        new Sim(S(prose, Scroller.init, helpDoc, Scroller.init, false, Vector.empty), Size(20, 40))
      "typed".foreach(c => sim.in(Input.Keyboard(grit.tui.model.input.Key.Printable(c))))
      assert(sim.loop.state.ed.text == "typed")
      val top0 = sim.loop.painted.pane(PaneKey.of("a")).map(_.top)
      sim.in(Input.Mouse(MouseEvent(MouseKind.Wheel, Button.WheelUp, Pos(5, 5), Mods.none)))
      sim.in(Input.Mouse(MouseEvent(MouseKind.Wheel, Button.WheelUp, Pos(5, 5), Mods.none)))
      sim.paint()
      val top1 = sim.loop.painted.pane(PaneKey.of("a")).map(_.top)
      assert(top0.zip(top1).exists((a, b) => a - b == 2 * Route.WheelRows))
    }

    test("a screen with nothing focusable still routes keys to its handlers") {
      val keyed: Input -> Option[M] = {
        case Input.Keyboard(grit.tui.model.input.Key.Printable(c)) =>
          Some(M.E(Editor(c.toString, 1)))
        case _ => None
      }
      val bare: Node[M] = paint(
        grit.tui.components.widget
          .StatusBar(Vector("x"), Vector(), grit.tui.model.surface.Style.plain)
      ).onKey(keyed)
      val inDialog: Node[M] = bare.dialog(modal, bare, None)
      val k = Input.Keyboard(grit.tui.model.input.Key.Printable('k'))
      for (tree <- Vector(bare, inDialog)) {
        val (painted, _) = grit.tui.runtime.Paint.layout(tree, Size(20, 40), Memo.empty)
        assert(Route.keys(k, painted) == Some(M.E(Editor("k", 1))))
      }
    }

    test("a second pane under a key already painted shows an error and routes nothing") {
      val k = PaneKey.of("same")
      val twice: Node[M] = column(
        flex() -> Node.doc(k, helpDoc, Anchor.At(grit.tui.model.select.DocPos.zero)),
        flex() -> Node.doc(k, prose, Anchor.At(grit.tui.model.select.DocPos.zero))
      )
      val (frame, painted, _) = grit.tui.runtime.Paint.frame(twice, Size(10, 40), Memo.empty)
      val v = new Vt(10, 41)
      v.feed(Painter.paint(frame, None))
      assert(
        v.text(5).startsWith("duplicate pane key: same"),
        v.text(0).startsWith("the modal's own")
      )
      assert(painted.targets.length == 1, painted.pane(k).map(_.text.top) == Some(0))
    }

    test("rule 7's aim: the memo wraps only what changed") {
      val big = Doc(
        Vector.tabulate(2000)(i =>
          Block.Text(s"block $i with enough words to wrap at forty columns, maybe twice")
        )
      )
      val sim =
        new Sim(S(big, Scroller.init, helpDoc, Scroller.init, false, Vector.empty), Size(30, 40))
      val first = sim.loop.memo.docs.get(PaneKey.of("a")).map(d => (d.hits, d.misses))
      (0 until 20).foreach(_ => sim.paint())
      val still = sim.loop.memo.docs.get(PaneKey.of("a")).map(d => (d.hits, d.misses))
      assert(first == Some((0L, 2000L)), still == first) // twenty repaints: the eq fast path
      // The tail replaced by a different block at the same position: one miss, and it paints.
      val tail = Block.Text("the reply that replaced the thinking line")
      sim.loop = sim.loop.copy(state = sim.loop.state.copy(a = big.updated(1999, tail)))
      sim.paint()
      val after = sim.loop.memo.docs.get(PaneKey.of("a")).map(d => (d.hits, d.misses))
      assert(after == Some((1999L, 2001L)))
      assert(sim.vt.text.exists(_.contains("the reply that replaced")))
      // An equal block rebuilt (not eq): a hit through ==, no wrap.
      sim.loop = sim.loop.copy(state =
        sim.loop.state.copy(a =
          Doc(
            sim.loop.state.a.blocks.map(b =>
              b match {
                case t: Block.Text => t.copy()
                case o => o
              }
            )
          )
        )
      )
      sim.paint()
      assert(sim.loop.memo.docs.get(PaneKey.of("a")).map(_.misses) == Some(2001L))
    }
  }
}
