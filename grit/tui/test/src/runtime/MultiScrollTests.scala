package grit.tui.runtime

import utest.*

import grit.tui.runtime.std.{Ambient, Selecting, Std, StdBase}
import grit.tui.components.layout.{Region, Split}
import grit.tui.components.pane.{Panes, TextPane}
import grit.tui.components.widget.ScrollPane
import grit.tui.model.input.{Button, Input, Mods, MouseEvent, MouseKind}
import grit.tui.model.select.Doc
import grit.tui.model.surface.{Frame, PaneId, Placement, Placements, Pos, Rect, Size, Surface}
import grit.tui.model.text.WrapCache

/** Two independent scroll panes side by side, which is the shape a tabbed or split screen
  * wants: two documents, two anchors, two tracks, one pointer.
  *
  * Nothing in the layer had to be built for this -- every anchor, wrap cache and row index
  * already lives on the pane it belongs to, `scrolls` was already a `Vector`, and
  * `Std.State.track` was already a `PaneId` rather than a boolean. So these tests are not
  * checking a new feature; they are pinning the per-pane behaviour before someone
  * simplifies one of those back into a singleton.
  *
  * The last test is the other half of the truth: there is one pointer, so there is one
  * drag, so **two panes cannot show a selection at once**. That is a shape rather than a
  * rule, and it is the one thing an app of two scrollable columns has to know.
  */
object MultiScrollTests extends TestSuite {

  private val Body = PaneId.of("body")
  private val LeftCol = PaneId.of("left-col")
  private val RightCol = PaneId.of("right-col")
  private val Left = PaneId.of("left")
  private val LeftBar = PaneId.of("left-bar")
  private val Right = PaneId.of("right")
  private val RightBar = PaneId.of("right-bar")

  private val left = ScrollPane(Left, LeftBar)
  private val right = ScrollPane(Right, RightBar)

  private enum Own { case Ping }

  final case class S(panes: Panes, std: Std.State = Std.State(), note: String = "")

  /** Twenty short lines each, so both panes have far more document than window. */
  private val Twenty: Vector[String] = (0 until 20).toVector.map(i => f"row$i%02d")

  private def stateOf: S = {
    val panes = Panes
      .of(
        TextPane(Left, Doc.of(Twenty*), cache = WrapCache.empty(19)),
        TextPane(Right, Doc.of(Twenty*), cache = WrapCache.empty(19))
      )
      .layout(Left, Size(6, 19))
      .layout(Right, Size(6, 19))
    S(panes = panes.focusOn(Left))
  }

  /** A 6x40 screen split down the middle, each half a content pane and a one-column
    * track. Written out rather than resolved so the placements a test reasons about are
    * visible in the test.
    */
  private val at = Placements(
    Vector(
      Placement(Left, Rect(0, 0, 6, 19)),
      Placement(LeftBar, Rect(0, 19, 6, 1)),
      Placement(Right, Rect(0, 20, 6, 19)),
      Placement(RightBar, Rect(0, 39, 6, 1))
    )
  )

  private object App extends StdBase[S, Own] with Ambient[S, Own] with Selecting[S, Own] {
    val panes: S -> Panes = s => s.panes
    val withPanes: (S, Panes) -> S = (s, p) => s.copy(panes = p)
    val std: S -> Std.State = s => s.std
    val withStd: (S, Std.State) -> S = (s, c) => s.copy(std = c)
    protected val ownUpdate: (Own, S) -> (S, Effect[Std | Own]) =
      (_, s) => (s.copy(note = "own"), Effect.NoOp)
    val onResize: (S, Size) -> S = (s, _) => s

    override val scrolls: S -> Vector[ScrollPane] = _ => Vector(left, right)

    val layers: Vector[Layer] = Vector(ambientLayer, pageLayer, pointerLayer)
    val steps: Vector[Step] = Vector(ambientStep, scrollStep, pointerStep)

    def init: (S, Effect[Std | Own]) = (stateOf, Effect.NoOp)
    def view: S -> (Size -> Frame) = _ => size => Frame(Surface.blank(size))
  }

  /** Which document row a pane is reading from -- the observable that says whether *that*
    * pane scrolled, independently of what the other one did.
    */
  private def topOf(s: S, id: PaneId): Int =
    s.panes.rendered(id).top.map(_.entry).getOrElse(-1)

  private def send(s: S, kind: MouseKind, pos: Pos): S =
    App.update(Std.Pointer(MouseEvent(kind, Button.Left, pos, Mods.none), at), s)._1

  private def wheelAt(s: S, pos: Pos, up: Boolean): S = {
    val e = MouseEvent(
      MouseKind.Wheel,
      if (up) { Button.WheelUp }
      else { Button.WheelDown },
      pos,
      Mods.none
    )
    App.update(Std.Pointer(e, at), s)._1
  }

  val tests = Tests {

    test("the wheel scrolls the column it is over and freezes the other") {
      // Both panes start anchored at the bottom of their own document.
      val s0 = stateOf
      val bothTop = topOf(s0, Left)
      // Scaffolding, not a check: both panes are built from the same document at the
      // same size, so this names the shared starting row the rest of the test moves off.
      assert(topOf(s0, Right) == bothTop)

      // Over the right column, at a column no part of the left one occupies.
      val s1 = wheelAt(s0, Pos(2, 30), up = true)
      assert(topOf(s1, Right) < bothTop)
      assert(topOf(s1, Left) == bothTop)

      // And the other way round: the wheel is resolved by hit-testing the pointer, never
      // by focus, which is what makes two columns independent under the mouse.
      val s2 = wheelAt(s1, Pos(2, 5), up = true)
      assert(topOf(s2, Left) < bothTop)
      assert(topOf(s2, Right) == topOf(s1, Right))
    }

    test("a wheel over a track scrolls the pane that track belongs to") {
      val s0 = stateOf
      val s1 = wheelAt(s0, Pos(2, 39), up = true) // the *right* pane's track
      assert(topOf(s1, Right) < topOf(s0, Right))
      assert(topOf(s1, Left) == topOf(s0, Left))
    }

    test("a thumb dragged into the other column keeps scrolling the pane it grabbed") {
      // Grab the left track at its head: the left pane jumps to the top of its document.
      val s0 = stateOf
      val rightBefore = topOf(s0, Right)
      val grabbed = send(s0, MouseKind.Press, Pos(0, 19))
      assert(topOf(grabbed, Left) == 0)
      assert(grabbed.std.track == Some(LeftBar))
      assert(topOf(grabbed, Right) == rightBefore)

      // Now run the pointer across into the right column. The held track is resolved by
      // the id the press stored and never re-hit-tested (rule 6's reasoning, applied to a
      // widget), so the *left* pane follows the row and the right one does not move.
      val dragged = send(grabbed, MouseKind.Drag, Pos(5, 30))
      assert(topOf(dragged, Left) > 0)
      assert(topOf(dragged, Right) == rightBefore)
      // A thumb drag is not a text drag: neither pane began a selection.
      assert(dragged.panes.drag == None)

      // Letting go clears the track and copies nothing.
      val (up, eff) = App.update(
        Std.Pointer(MouseEvent(MouseKind.Release, Button.Left, Pos(5, 30), Mods.none), at),
        dragged
      )
      assert(up.std.track == None)
      assert(eff == Effect.NoOp)
    }

    test("a drag parked below one column autoscrolls that column and freezes the other") {
      val s0 = stateOf
      // Scroll the left pane up so it has somewhere to autoscroll *to*.
      val s1 = wheelAt(s0, Pos(2, 5), up = true)
      val leftStart = topOf(s1, Left)
      val rightStart = topOf(s1, Right)

      // Press in the left pane, then park the pointer below its last row.
      val pressed = send(s1, MouseKind.Press, Pos(1, 4))
      val parked = send(pressed, MouseKind.Drag, Pos(6, 4))
      assert(parked.std.edge == 1)
      assert(parked.panes.drag.map(_.pane) == Some(Left))

      // The tick scrolls the pane the drag began in -- not the focused one, and not the
      // one under the pointer.
      val (ticked, _) = App.update(Std.Edge(at), parked)
      assert(topOf(ticked, Left) > leftStart)
      assert(topOf(ticked, Right) == rightStart)
    }

    test("a nested split lays out both columns and both tracks at their own sizes") {
      val body = Split.of(LeftCol -> Region.Flex(0), RightCol -> Region.Flex(0))
      val placed = body
        .resolve(Size(6, 40))
        .nest(LeftCol, left.split)
        .nest(RightCol, right.split)

      // Four panes fall out of one description, each a content pane beside its track.
      assert(placed.rects.get(Left).map(_.cols) == Some(19))
      assert(placed.rects.get(LeftBar).map(_.cols) == Some(1))
      assert(placed.rects.get(Right).map(_.cols) == Some(19))
      assert(placed.rects.get(RightBar).map(_.cols) == Some(1))
      assert(placed.rects.get(Right).map(_.left) == Some(20))

      // And `layoutIn` lays every one of them out, including the regions that are not
      // panes -- laying out a name with no document is a no-op, not an error.
      val laid = stateOf.panes.layoutIn(placed)
      assert(laid.rendered(Left).size == Size(6, 19))
      assert(laid.rendered(Right).size == Size(6, 19))
    }

    test("there is one pointer, so only one column can show a selection") {
      // A real selection in the left pane.
      val s0 = stateOf
      val s1 = send(send(s0, MouseKind.Press, Pos(0, 0)), MouseKind.Drag, Pos(2, 4))
      assert(s1.panes.selectionIn(Left).isDefined)
      assert(s1.panes.selectionIn(Right) == None)

      // Pressing in the right pane takes the drag with it, and the left pane's highlight
      // goes out. One drag, one visible selection -- a shape rather than a rule, recorded
      // here so a change has to argue with a test.
      val s2 = send(s1, MouseKind.Press, Pos(0, 25))
      assert(s2.panes.selectionIn(Left) == None)
      assert(s2.panes.drag.map(_.pane) == Some(Right))
    }
  }
}
