package grit.tui.runtime

import grit.tui.components.PaneKey

import grit.tui.model.input.{Button, Input, MouseEvent, MouseKind}
import grit.tui.model.select.{DocPos, Selection}
import grit.tui.model.surface.Pos

/** The pointer, as the runtime holds it between events. Mechanism, not app state: which
  * pane owns a drag is decided by where the press landed and nothing the app says.
  */
enum Grab {
  case Idle

  /** A text drag in pane `key`, from `anchor` (fixed) to `head` (follows the pointer);
    * `edge` is -1/+1 while the pointer is parked above/below the pane, 0 inside it.
    */
  case Select(key: PaneKey, anchor: DocPos, head: DocPos, pointer: Pos, edge: Int)

  /** A scrollbar thumb held in pane `key`. Not a text drag: nothing is selected. */
  case Thumb(key: PaneKey)
}

/** The runtime's own two timers, as data. */
enum Tick {
  case Autoscroll, Deadline
}

enum Timer {
  case Arm(tick: Tick, ms: Long)
  case Disarm(tick: Tick)
}

/** What an input meant: the app's messages, in order, the pointer after it, and the
  * runtime's own timers to arm or drop. All plain data.
  */
final case class Routed[+M](msgs: Vector[M], grab: Grab, timers: Vector[Timer])

/** Input routed against what was painted. Pure: the painted tree and the grab in, messages
  * out. Precedence is the tree's own -- topmost first for the pointer, the focus path for
  * keys -- so there is no ordered list of layers to keep.
  */
object Route {

  val AutoscrollMs: Long = 45L
  val DeadlineMs: Long = 1500L
  val WheelRows: Int = 3

  def input[M](in: Input, p: Painted[M], g: Grab): Routed[M] = in match {
    case Input.Keyboard(_) | Input.Paste(_) => Routed(keys(in, p).toVector, g, Vector.empty)
    case Input.Mouse(e) => mouse(e, p, g)
    case _ => Routed(Vector.empty, g, Vector.empty)
  }

  /** Keys: down the focus path through every `first` (hotkeys), back up through every
    * `last` (the leaf's own binding first) until a barrier, then -- only if no barrier
    * stood on the path -- the page keys to the topmost pane that scrolls.
    */
  def keys[M](in: Input, p: Painted[M]): Option[M] = {
    val path = p.focus
    var out: Option[M] = None
    var done = false
    var i = 0
    while (!done && i < path.length) {
      path(i).first.flatMap(h => h(in)) match {
        case Some(m) => out = Some(m); done = true
        case None => ()
      }
      i += 1
    }
    i = path.length - 1
    while (!done && i >= 0) {
      val stop = path(i)
      stop.last.flatMap(h => h(in)) match {
        case Some(m) => out = Some(m); done = true
        case None => if (stop.barrier) { done = true }
      }
      i -= 1
    }
    if (!done) {
      val panes = p.reachablePanes
      in match {
        case Input.Keyboard(grit.tui.model.input.Key.PageUp(_)) =>
          out = firstScroll(panes, dir = -1)
        case Input.Keyboard(grit.tui.model.input.Key.PageDown(_)) =>
          out = firstScroll(panes, dir = 1)
        case _ => ()
      }
    }
    out
  }

  private def firstScroll[M](panes: Vector[PanePaint[M]], dir: Int): Option[M] = {
    var i = 0
    var out: Option[M] = None
    while (out.isEmpty && i < panes.length) {
      val pp = panes(i)
      out = pp.scroll.map(f => f(pp.scrolledBy(dir * math.max(1, pp.text.rows - 2))))
      i += 1
    }
    out
  }

  private def mouse[M](e: MouseEvent, p: Painted[M], g: Grab): Routed[M] = e.kind match {
    case MouseKind.Press => press(e.pos, p)
    case MouseKind.Drag => drag(e.pos, p, g)
    case MouseKind.Release => release(p, g, expired = false)
    case MouseKind.Wheel => Routed(wheel(e, p).toVector, g, Vector.empty)
    case MouseKind.Move => Routed(Vector.empty, g, Vector.empty)
  }

  /** The topmost thing under the press claims it: a thumb, a pane's text (a drag begins),
    * a press handler that answers, or a wall that swallows. A press handler that declines
    * lets the press fall through to what is beneath it.
    */
  private def press[M](pos: Pos, p: Painted[M]): Routed[M] = {
    val stop = Vector(Timer.Disarm(Tick.Autoscroll))
    var i = p.targets.length - 1
    var out: Option[Routed[M]] = None
    while (out.isEmpty && i >= 0) {
      p.targets(i) match {
        case Target.Pane(pp) if pp.bar.exists(_.contains(pos)) =>
          out = Some(Routed(thumbTo(pp, pos).toVector, Grab.Thumb(pp.key), stop))
        case Target.Pane(pp) if pp.text.contains(pos) =>
          val local = Pos(pos.row - pp.text.top, pos.col - pp.text.left)
          out = Some(pp.viewport.docPosAt(local).map(pp.doc.clamp) match {
            case Some(dp) if pp.select.isDefined =>
              Routed(
                pp.select.map(f => f(Some(Selection(dp, dp)))).toVector,
                Grab.Select(pp.key, dp, dp, pos, 0),
                stop :+ Timer.Arm(Tick.Deadline, DeadlineMs)
              )
            case _ => Routed(Vector.empty, Grab.Idle, stop)
          })
        case Target.Press(r, f) if r.contains(pos) =>
          f(pos).foreach(m => out = Some(Routed(Vector(m), Grab.Idle, stop)))
        case Target.Wall(r) if r.contains(pos) => out = Some(Routed(Vector.empty, Grab.Idle, stop))
        case _ => ()
      }
      i -= 1
    }
    out.getOrElse(Routed(Vector.empty, Grab.Idle, stop))
  }

  private def thumbTo[M](pp: PanePaint[M], pos: Pos): Option[M] =
    pp.thumbAt(pos.row).flatMap(a => pp.scroll.map(f => f(a)))

  /** Motion with the button down. Never hit-tested: the grab's own pane, found by key in
    * the latest painted frame, reads the pointer clamped to its own rect (rule 6).
    */
  private def drag[M](pos: Pos, p: Painted[M], g: Grab): Routed[M] = g match {
    case Grab.Idle => Routed(Vector.empty, g, Vector.empty)
    case Grab.Thumb(key) =>
      Routed(p.pane(key).flatMap(pp => thumbTo(pp, pos)).toVector, g, Vector.empty)
    case s: Grab.Select =>
      p.pane(s.key) match {
        case None => Routed(Vector.empty, Grab.Idle, Vector(Timer.Disarm(Tick.Autoscroll)))
        case Some(pp) =>
          val head = headAt(pp, pos).getOrElse(s.head)
          val edge =
            if (pos.row < pp.text.top) -1
            else if (pos.row >= pp.text.bottom) 1
            else 0
          Routed(
            pp.select.map(f => f(Some(Selection.between(s.anchor, head)))).toVector,
            s.copy(head = head, pointer = pos, edge = edge),
            Vector(
              if (edge == 0) Timer.Disarm(Tick.Autoscroll)
              else Timer.Arm(Tick.Autoscroll, AutoscrollMs),
              Timer.Arm(Tick.Deadline, DeadlineMs)
            )
          )
      }
  }

  /** The document position under `pos`, clamped into the pane's rect and its document. */
  private def headAt[M](pp: PanePaint[M], pos: Pos): Option[DocPos] = {
    val r = pp.text
    val local = Pos(clamp(pos.row - r.top, r.rows), clamp(pos.col - r.left, r.cols))
    pp.viewport.docPosAt(local).map(pp.doc.clamp)
  }

  private def clamp(v: Int, extent: Int): Int = math.max(0, math.min(v, math.max(0, extent - 1)))

  /** The button up (or the deadline): the selection is cleared and what it held offered. */
  def release[M](p: Painted[M], g: Grab, expired: Boolean): Routed[M] = {
    val off = Vector(Timer.Disarm(Tick.Autoscroll), Timer.Disarm(Tick.Deadline))
    g match {
      case s: Grab.Select =>
        val msgs = p.pane(s.key).toVector.flatMap { pp =>
          val text = pp.doc.textOf(Selection.between(s.anchor, s.head))
          pp.select.map(f => f(None)).toVector ++
            (if (text.isEmpty) Vector.empty else pp.copy.map(f => f(text, expired)).toVector)
        }
        Routed(msgs, Grab.Idle, off)
      case _ => Routed(Vector.empty, Grab.Idle, off)
    }
  }

  /** One autoscroll tick: the grabbed pane scrolls a row toward the pointer, and the head
    * follows onto the row that arrived -- computed against the scrolled viewport, which the
    * memo can lay out without a paint.
    */
  def autoscroll[M](p: Painted[M], g: Grab): Routed[M] = g match {
    case s: Grab.Select if s.edge != 0 =>
      p.pane(s.key) match {
        case None => Routed(Vector.empty, Grab.Idle, Vector(Timer.Disarm(Tick.Autoscroll)))
        case Some(pp) =>
          val anchor = pp.scrolledBy(s.edge)
          val top = pp.memo.topFor(anchor, pp.text.rows)
          val moved = pp.copy(top = top, viewport = pp.memo.viewport(top, pp.text.size))
          val head = headAt(moved, s.pointer).getOrElse(s.head)
          Routed(
            pp.scroll.map(f => f(anchor)).toVector ++
              pp.select.map(f => f(Some(Selection.between(s.anchor, head)))).toVector,
            s.copy(head = head),
            Vector(Timer.Arm(Tick.Autoscroll, AutoscrollMs)) ++
              (if (top != pp.top) Vector(Timer.Arm(Tick.Deadline, DeadlineMs)) else Vector.empty)
          )
      }
    case _ => Routed(Vector.empty, g, Vector(Timer.Disarm(Tick.Autoscroll)))
  }

  /** The wheel scrolls the topmost pane under it -- its track included -- unless a wall
    * is on top.
    */
  private def wheel[M](e: MouseEvent, p: Painted[M]): Option[M] = {
    val delta = if (e.button == Button.WheelUp) -WheelRows else WheelRows
    var i = p.targets.length - 1
    var out: Option[Option[M]] = None
    while (out.isEmpty && i >= 0) {
      p.targets(i) match {
        case Target.Pane(pp) if pp.text.contains(e.pos) || pp.bar.exists(_.contains(e.pos)) =>
          out = Some(pp.scroll.map(f => f(pp.scrolledBy(delta))))
        case Target.Wall(r) if r.contains(e.pos) => out = Some(None)
        case _ => ()
      }
      i -= 1
    }
    out.flatten
  }
}
