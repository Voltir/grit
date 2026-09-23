package grit.tui.runtime.std

import grit.tui.runtime.Effect
import grit.tui.components.pane.{Anchor, Panes}
import grit.tui.components.widget.ScrollPane
import grit.tui.model.input.{Button, Input, Key, MouseEvent, MouseKind}
import grit.tui.model.select.DocPos
import grit.tui.model.surface.{PaneId, Placements, Pos}

/** Moving a pane's viewport: the keyboard's paging, the wheel, and the scroll panes whose
  * tracks the layer drives on the app's behalf.
  *
  * Nothing here is app-global. Every anchor, wrap cache and row index already lives on the
  * pane it belongs to, so an app showing two scrollable columns needs no more than two
  * entries in [[scrolls]] -- there is no shared scroll position to collide over. What is
  * singular is the keyboard's focus, because there is one keyboard: [[Std.Scroll]],
  * `ToTop` and `ToBottom` move the focused pane, while the wheel moves whatever is under
  * the pointer.
  */
trait Scrolling[State, Own] extends StdBase[State, Own] {

  /** The view just scrolled by user input or wheel; chrome may want to reflect it.
    * The autoscroll tick deliberately does not pass through here -- a selection in
    * progress is not a scroll the user asked for.
    */
  val onScroll: State -> State = s => s

  /** When the wheel lands on pane `id`, scroll `Some(other)` instead. The general
    * escape hatch for a region that has no document of its own; a scrollbar track named
    * in [[scrolls]] is already redirected and needs nothing here.
    */
  val wheelTo: (State, PaneId) -> Option[PaneId] = (_, _) => None

  /** The scroll panes this app is showing -- a content pane and the track beside it,
    * each as one [[ScrollPane]], and as many of them as the screen has.
    *
    * Naming them here is what lets the layer own the thumb: the wheel over a track, the
    * press that grabs it, the motion that drags it and the release that lets go are all
    * mechanism, and an app that had to route them itself was re-deriving the same forty
    * lines correctly. What stays the app's is *painting* the pair
    * ([[ScrollPane.views]]) and saying which panes are scrollable, which is this hook.
    */
  val scrolls: State -> Vector[ScrollPane] = _ => Vector.empty

  /** Paging the focused pane. Split out from the pointer's layer because it is a
    * keyboard fact: an app that scrolls without ever selecting still wants it.
    */
  val pageLayer: Layer = (input, state, at) =>
    input match {
      case Input.Keyboard(Key.PageUp(_)) => claimed(page(state, at, -1))
      case Input.Keyboard(Key.PageDown(_)) => claimed(page(state, at, 1))
      case _ => Claim.Pass(input)
    }

  val scrollStep: Step = (msg, state) =>
    msg match {
      case Std.Scroll(delta) =>
        panes(state).focus match {
          case None => Some((state, Effect.NoOp))
          case Some(id) => Some(scrolled(state, id, delta))
        }
      case Std.ToTop => Some(anchored(state, Anchor.At(DocPos.zero)))
      case Std.ToBottom => Some(anchored(state, Anchor.Bottom))
      case _ => None
    }

  /** A page is the focused pane's own painted height less two rows of overlap. */
  protected val page: (State, Placements, Int) -> Option[Std | Own] = (state, at, dir) =>
    panes(state).focus.flatMap(id => at(id)).map(r => Std.Scroll(dir * math.max(1, r.rows - 2)))

  protected val scrolled: (State, PaneId, Int) -> (State, Effect[Std | Own]) =
    (state, id, delta) =>
      (onScroll(withPanes(state, panes(state).scrollBy(id, delta))), Effect.NoOp)

  protected val anchored: (State, Anchor) -> (State, Effect[Std | Own]) = (state, anchor) =>
    panes(state).focus match {
      case None => (state, Effect.NoOp)
      case Some(id) => (withPanes(state, panes(state).withAnchor(id, anchor)), Effect.NoOp)
    }

  /** The scroll pane whose track is `id`, if any. Indexed rather than `find`: an
    * iterator combinator is what capture checking rejects here.
    */
  protected val trackOf: (State, PaneId) -> Option[ScrollPane] = (state, id) => {
    val all = scrolls(state)
    var i = 0
    var found: Option[ScrollPane] = None
    while (i < all.length && found.isEmpty) {
      if (all(i).bar == id) { found = Some(all(i)) }
      i += 1
    }
    found
  }

  /** The scroll pane whose track was painted under `pos`, if any. */
  protected val trackAt: (State, Pos, Placements) -> Option[ScrollPane] = (state, pos, at) =>
    at.at(pos).flatMap((id, _) => trackOf(state, id))

  /** `state` with `sp`'s content pane re-anchored to where the thumb at `pos` points. */
  protected val dragTrack: (State, ScrollPane, Pos, Placements) -> State =
    (state, sp, pos, at) => withPanes(state, sp.grabbed(panes(state), pos, at))

  /** The wheel over a pane scrolls what that pane scrolls -- or what [[wheelTo]]
    * redirects it to, for a track that has no document of its own. Resolved by
    * hit-testing the pointer, never by focus, which is why two scrollable columns scroll
    * independently under the mouse.
    */
  protected val wheeled: (State, MouseEvent, Placements) -> (State, Effect[Std | Own]) =
    (state, e, at) => {
      val delta = if (e.button == Button.WheelUp) { -Std.WheelRows }
      else { Std.WheelRows }
      at.at(e.pos) match {
        case Some((id, _)) =>
          val target = trackOf(state, id).flatMap(_.wheelTarget(id))
          scrolled(state, target.orElse(wheelTo(state, id)).getOrElse(id), delta)
        case None => (state, Effect.NoOp)
      }
    }
}

/** The pointer's whole life: a press that claims, focuses or grabs; a drag that selects
  * or moves a thumb; the autoscroll of a drag parked at an edge; and the release that
  * copies.
  *
  * Extends [[Scrolling]] rather than standing beside it because a wheel *is* a scroll
  * arriving as a mouse report, and because a drag held past a pane's edge scrolls that
  * pane. There is one pointer, so there is one drag and one autoscroll chain: the drag
  * carries the pane it began in ([[Panes.drag]]) and every step reads that rather than
  * re-hit-testing (rule 6). Two panes therefore cannot show a selection at once, which is
  * a shape rather than a rule -- and the one thing an app of two scrollable columns has
  * to know.
  */
trait Selecting[State, Own] extends Scrolling[State, Own] {

  /** A press the app claims before the selection machine sees it. `Some` defers the
    * press to `ownUpdate` entirely -- the machine's own press effects (timer cancels,
    * drag start) do not happen, exactly as a hand-routed press would not have them.
    * Drags and releases while the claim holds come back through [[onGrab]]; the layer
    * manages `Std.State.grabbing` around them.
    */
  val onPress: (State, Pos, Placements) -> Option[Own] = (_, _, _) => None

  /** Motion with the button down while an *app*-claimed grab is live. A scrollbar thumb
    * does not come through here -- [[Scrolling.scrolls]] owns that -- so this is for a
    * drag the app claimed in [[onPress]] and nothing else. `Std.State.pointer` is
    * already updated.
    */
  val onGrab: (State, Pos, Placements) -> State = (s, _, _) => s

  /** A drag ended: `text` is what was selected (None for a plain click), `expired`
    * whether the escaped-drag deadline ended it rather than a button release.
    */
  val onCopy: (State, Option[String], Boolean) -> State = (s, _, _) => s

  /** Every mouse report as one `Std.Pointer` carrying the frame it was resolved
    * against, last in the chain: a mouse event nothing above claimed is the pointer's.
    */
  val pointerLayer: Layer = (input, state, at) => {
    val _ = state
    input match {
      case Input.Mouse(e) => Claim.Handled(Std.Pointer(e, at))
      case _ => Claim.Pass(input)
    }
  }

  val pointerStep: Step = (msg, state) =>
    msg match {
      case Std.Pointer(e, at) =>
        e.kind match {
          case MouseKind.Press =>
            onPress(state, e.pos, at) match {
              case Some(own) => Some(ownUpdate(own, state))
              case None => Some(pressed(state, e.pos, at))
            }
          case MouseKind.Drag => Some(dragged(state, e, at))
          case MouseKind.Wheel => Some(wheeled(state, e, at))
          case MouseKind.Release => Some(released(state))
          case MouseKind.Move => Some((state, Effect.NoOp))
        }
      case Std.Edge(at) => Some(autoscroll(state, at))
      case Std.Deadline => Some(finished(state, true))
      case _ => None
    }

  /** Where the dragging pane is reading from, for spotting a tick that moved nothing. */
  private val topOf: Panes -> Option[DocPos] = ps => ps.drag.flatMap(d => ps.rendered(d.pane).top)

  /** Which way the pointer is pulling: -1 above the dragging pane, +1 below it, 0
    * inside. Read against the *dragging* pane's rect, never against whatever is under
    * the mouse.
    */
  private val edgeOf: (State, Pos, Placements) -> Int = (state, pos, at) =>
    panes(state).drag.flatMap(d => at(d.pane)) match {
      case None => 0
      case Some(rect) =>
        if (pos.row < rect.top) { -1 }
        else if (pos.row >= rect.bottom) { 1 }
        else { 0 }
    }

  /** A press nothing claimed: a grab of a scrollbar thumb, or a selection.
    *
    * The thumb is checked first and never falls through to `Panes.onPress` -- a thumb
    * drag is not a text drag, and the track holds no document to begin a selection in.
    * Otherwise: focus and begin a drag in whichever pane is topmost there, and arm the
    * escaped-drag deadline. The autoscroll chain, if any, is dead -- a new press is a
    * new selection.
    */
  protected val pressed: (State, Pos, Placements) -> (State, Effect[Std | Own]) =
    (state, pos, at) => {
      val st = std(state)
      trackAt(state, pos, at) match {
        case Some(sp) =>
          val held = withStd(state, st.copy(pointer = pos, edge = 0, track = Some(sp.bar)))
          (dragTrack(held, sp, pos, at), Effect.Cancel(Std.Autoscroll))
        case None =>
          (
            withStd(
              withPanes(state, panes(state).onPress(at.all, pos)),
              st.copy(pointer = pos, edge = 0)
            ),
            Effect.batch(
              Effect.Cancel(Std.Autoscroll),
              Effect.After(Std.DragEnd, Std.DragEndMs, Std.Deadline)
            )
          )
      }
    }

  /** Motion with the button down. The pointer is not hit-tested: `onDrag` resolves it
    * against the pane the drag began in and clamps it there, so wandering over the
    * transcript while selecting in the modal selects nothing behind it (rule 6).
    * Motion while a track or a claimed grab is live belongs to that, not to selection.
    * The held track is resolved by the id the press stored, never re-hit-tested: the
    * same rule as rule 6, for the same reason -- a thumb dragged off its own track, or
    * across into a second scrollable column, must keep scrolling the pane it grabbed.
    */
  protected val dragged: (State, MouseEvent, Placements) -> (State, Effect[Std | Own]) =
    (state, e, at) => {
      val st = std(state)
      val held = st.track.flatMap(id => trackOf(state, id))
      if (held.isDefined) {
        val moved = withStd(state, st.copy(pointer = e.pos))
        (held.map(sp => dragTrack(moved, sp, e.pos, at)).getOrElse(moved), Effect.NoOp)
      } else if (st.grabbing) {
        val moved = withStd(state, st.copy(pointer = e.pos))
        (onGrab(moved, e.pos, at), Effect.NoOp)
      } else {
        val dir = edgeOf(state, e.pos, at)
        (
          withStd(
            withPanes(state, panes(state).onDrag(at.all, e.pos)),
            st.copy(pointer = e.pos, edge = dir)
          ),
          Effect.batch(
            if (dir == 0) { Effect.Cancel(Std.Autoscroll) }
            else { Effect.After(Std.Autoscroll, Std.AutoscrollMs, Std.Edge(at)) },
            Effect.After(Std.DragEnd, Std.DragEndMs, Std.Deadline)
          )
        )
      }
    }

  /** The button coming up: a held track or a claimed grab just lets go -- neither
    * copies, because neither ever selected anything -- and a text drag finishes.
    */
  protected val released: State -> (State, Effect[Std | Own]) = state => {
    val st = std(state)
    if (st.track.isDefined) { (withStd(state, st.copy(track = None)), Effect.NoOp) }
    else if (st.grabbing) { (withStd(state, st.copy(grabbing = false)), Effect.NoOp) }
    else { finished(state, false) }
  }

  /** A drag ends: what it selected is offered for copying -- `None` for nothing, so a
    * plain click never clears the clipboard -- and both timers stand down.
    */
  protected val finished: (State, Boolean) -> (State, Effect[Std | Own]) = (state, expired) => {
    val (ps, text) = panes(state).onRelease
    val s1 = withStd(withPanes(state, ps), std(state).copy(edge = 0))
    (
      onCopy(s1, text, expired),
      Effect.batch(
        Effect.Cancel(Std.Autoscroll),
        Effect.Cancel(Std.DragEnd),
        text match {
          case Some(t) => Effect.CopyOut(t)
          case None => Effect.NoOp
        }
      )
    )
  }

  /** A tick that actually moved the view re-arms the escaped-drag deadline: a parked
    * pointer autoscrolling is a live drag, not an abandoned one. A tick that moved
    * nothing does not, so a drag held off the bottom scrolls to the end of the
    * document and *then* expires, rather than ticking forever.
    *
    * The pane that scrolls is the drag's own, never the focused one -- which is what
    * keeps a second scrollable column frozen while the first autoscrolls.
    */
  protected val autoscroll: (State, Placements) -> (State, Effect[Std | Own]) = (state, at) => {
    val st = std(state)
    val ps = panes(state)
    if (st.edge == 0 || ps.drag.isEmpty) {
      (withStd(state, st.copy(edge = 0)), Effect.Cancel(Std.Autoscroll))
    } else {
      val scrolledPanes = ps.drag.map(_.pane) match {
        case None => ps
        case Some(id) => ps.scrollBy(id, st.edge)
      }
      val s1 = withPanes(state, scrolledPanes)
      // The head is extended to the pointer again -- still parked at the edge, so the
      // selection grows with the scroll. The anchor is a DocPos and never moves.
      val s2 = withPanes(s1, panes(s1).onDrag(at.all, st.pointer))
      val moved = topOf(panes(s2)) != topOf(ps)
      (
        s2,
        Effect.batch(
          Effect.After(Std.Autoscroll, Std.AutoscrollMs, Std.Edge(at)),
          if (moved) { Effect.After(Std.DragEnd, Std.DragEndMs, Std.Deadline) }
          else { Effect.NoOp }
        )
      )
    }
  }
}
