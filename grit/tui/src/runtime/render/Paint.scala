package grit.tui.runtime.render

import grit.tui.components.layout.Stacking
import grit.tui.components.overlay.Popup
import grit.tui.components.pane.Anchor
import grit.tui.components.pane.Viewport
import grit.tui.components.tree.{Node, OnInput, PaneKey}
import grit.tui.components.widget.Scrollbar
import grit.tui.model.input.{Button, Input, Key, Mods, MouseEvent, MouseKind}
import grit.tui.model.select.Doc
import grit.tui.model.select.Selection
import grit.tui.model.surface.{Frame, Pos, Rect, Size, Surface}

/** One stop on the focus path, root first. `first` runs on the way down (capture: hotkeys,
  * a popup's arrows), `last` on the way back up (bubble: the leaf's own editing, then
  * Enter-submits). A `barrier` -- a modal -- ends the bubble there: nothing beneath it
  * sees the key, whether or not anything claimed it.
  */
final case class Stop[+M](first: Option[OnInput[M]], last: Option[OnInput[M]], barrier: Boolean)

/** A painted document pane, with everything input needs to answer against it -- what was
  * painted (the viewport, its top row, the memo that wrapped it) and the handlers,
  * already lifted into the root's message type.
  */
final case class PanePaint[+M](
    key: PaneKey,
    text: Rect,
    bar: Option[Rect],
    doc: Doc,
    memo: DocMemo,
    top: Int,
    viewport: Viewport,
    scroll: Option[Anchor -> M],
    select: Option[Option[Selection] -> M],
    copy: Option[(String, Boolean) -> M]
) {

  /** The anchor that `delta` rows of scrolling from what was painted reads from. */
  def scrolledBy(delta: Int): Anchor = memo.scrolled(top, delta, text.rows)

  /** The anchor a thumb held at screen row `row` points at. */
  def thumbAt(row: Int): Option[Anchor] = bar.flatMap { track =>
    val sb = Scrollbar(memo.total, text.rows, top)
    val target = sb.offsetAtRow(track.rows, row - track.top)
    if (target + text.rows >= memo.total) { Some(Anchor.Bottom) }
    else { memo.posOf(target).map(Anchor.At(_)) }
  }
}

/** A popup waiting to be painted over everything: its host's rect, and its route lifted. */
final case class Float[+M](host: Rect, popup: Popup, route: Popup.Route -> Option[M])

/** Something input can land on, in paint order: later is on top. */
enum Target[+M] {

  /** A document pane: its text area, and its scrollbar track if it has one. */
  case Pane(pane: PanePaint[M])

  /** A press handler over `rect` -- an `On.press`, or a popup's list. */
  case Press(rect: Rect, f: Pos -> Option[M])

  /** A modal's backdrop: every press and wheel that reaches it is swallowed, so nothing
    * painted beneath it can be reached through it.
    */
  case Wall(rect: Rect)
}

/** What the runtime painted, kept so input can be routed against it: the targets in
  * paint order, the focus path, and the screen size.
  */
final case class Painted[+M](size: Size, targets: Vector[Target[M]], focus: Vector[Stop[M]]) {

  /** The topmost painted pane named `key`. A grab resolves through this on every motion,
    * so it always speaks the frame the user is looking at.
    */
  def pane(key: PaneKey): Option[PanePaint[M]] = {
    var i = targets.length - 1
    var found: Option[PanePaint[M]] = None
    while (i >= 0 && found.isEmpty) {
      targets(i) match {
        case Target.Pane(p) if p.key == key => found = Some(p)
        case _ => ()
      }
      i -= 1
    }
    found
  }

  /** Panes above the topmost wall, topmost first: what an unclaimed page key may scroll. */
  def reachablePanes: Vector[PanePaint[M]] = {
    val out = Vector.newBuilder[PanePaint[M]]
    var i = targets.length - 1
    var walled = false
    while (i >= 0 && !walled) {
      targets(i) match {
        case Target.Pane(p) => out += p
        case Target.Wall(_) => walled = true
        case _ => ()
      }
      i -= 1
    }
    out.result()
  }
}

object Painted {
  def empty[M]: Painted[M] = Painted(Size(0, 0), Vector.empty, Vector.empty)
}

/** Layout and painting, done together, at paint time: the tree is resolved against the
  * size it is painted at, every leaf paints into its box, and what was painted is kept.
  * Pure: the only thing threaded is the wrap [[Memo]].
  */
object Paint {

  /** `root` painted at `size`, what was painted, and the memo brought up to date. */
  def frame[M](root: Node[M], size: Size, memo: Memo): (Frame, Painted[M], Memo) =
    walked(root, size, memo, cells = true)

  /** As [[frame]] without painting a cell: the layout and the targets only, for routing
    * the next input of a batch against the state the previous one produced.
    */
  def layout[M](root: Node[M], size: Size, memo: Memo): (Painted[M], Memo) = {
    val (_, p, m) = walked(root, size, memo, cells = false)
    (p, m)
  }

  private def walked[M](
      root: Node[M],
      size: Size,
      memo: Memo,
      cells: Boolean
  ): (Frame, Painted[M], Memo) = {
    val w = new Walk[M](size, memo, cells)
    w.walk(root, Rect(0, 0, size.rows, size.cols), (m: M) => m, Vector.empty)
    w.floats()
    (Frame(w.surface, w.cursor), Painted(size, w.targets.result(), w.focus), Memo(w.memos))
  }

  /** What `n` wants of `avail` -- asked by a `Region.Fit`. */
  def measure[A](n: Node[A], avail: Size): Size = n match {
    case Node.Edit(e, _) => e.measure(avail)
    case Node.Paint(v) => v.measure(avail)
    case Node.On(c, _, _, _) => measure(c, avail)
    case Node.Floating(h, _, _) => measure(h, avail)
    case Node.Mapped(inner, _) => measure(inner, avail)
    case _ => avail
  }

  /** Every leaf's keys, as the leaf itself binds them. */
  private def docKeys[M](p: PanePaint[M]): OnInput[M] = input =>
    p.scroll.flatMap { f =>
      val page = math.max(1, p.text.rows - 2)
      input match {
        case Input.Keyboard(Key.Up(_)) => Some(f(p.scrolledBy(-1)))
        case Input.Keyboard(Key.Down(_)) => Some(f(p.scrolledBy(1)))
        case Input.Keyboard(Key.PageUp(_)) => Some(f(p.scrolledBy(-page)))
        case Input.Keyboard(Key.PageDown(_)) => Some(f(p.scrolledBy(page)))
        case Input.Keyboard(Key.Home(_)) => Some(f(Anchor.At(grit.tui.model.select.DocPos.zero)))
        case Input.Keyboard(Key.End(_)) => Some(f(Anchor.Bottom))
        case _ => None
      }
    }

  /** One paint's scoped mutable locals (STYLE rule 7): the surface, the targets and the
    * focus path as they accumulate.
    */
  private final class Walk[M](size: Size, old: Memo, cells: Boolean) {
    var surface: Surface = Surface.blank(if (cells) size else Size(0, 0))
    var cursor: Option[Pos] = None
    val targets = Vector.newBuilder[Target[M]]
    var focus: Vector[Stop[M]] = Vector.empty

    /** How many times something has claimed the focus path: a focused leaf, a dialog,
      * or an `On` whose subtree claimed nothing.
      */
    private var claims = 0

    private def claim(p: Vector[Stop[M]]): Unit = { focus = p; claims += 1 }
    var memos: Map[PaneKey, DocMemo] = Map.empty
    private var deferred: Vector[Float[M]] = Vector.empty

    /** The popups, painted last so they are on top of everything, with their press
      * targets last for the same reason.
      */
    def floats(): Unit = {
      var i = 0
      while (i < deferred.length) {
        val fl = deferred(i)
        fl.popup.place(fl.host, size).foreach { box =>
          blit(fl.popup.render(box.size), box)
          val popup = fl.popup
          val route = fl.route
          val click: Pos -> Option[M] = pos =>
            route(
              popup
                .route(Input.Mouse(MouseEvent(MouseKind.Press, Button.Left, pos, Mods.none)), box)
            )
          targets += Target.Press(box, click)
        }
        i += 1
      }
    }

    private def blit(s: => Surface, r: Rect): Unit =
      if (cells) surface = surface.blit(s, Pos(r.top, r.left))

    def walk[A](n: Node[A], rect: Rect, lift: A -> M, path: Vector[Stop[M]]): Unit = n match {
      case Node.Box(vertical, parts) =>
        val rects =
          Stacking.rects(parts.map(_(0)), rect, vertical, (j, avail) => measure(parts(j)(1), avail))
        var i = 0
        while (i < parts.length) {
          walk(parts(i)(1), rects(i), lift, path)
          i += 1
        }

      case Node.Paint(view) =>
        if (rect.rows > 0 && rect.cols > 0) blit(view.render(rect.size), rect)

      case d: Node.DocPane[A] => pane(d, rect, lift, path)

      case Node.Edit(editor, edit) =>
        if (rect.rows > 0 && rect.cols > 0) {
          blit(editor.render(rect.size), rect)
          if (editor.focused) {
            cursor = editor
              .caretPos(rect.cols, rect.rows)
              .map(p => Pos(rect.top + p.row, rect.left + p.col))
            val cols = rect.cols
            val keys: OnInput[M] = input =>
              input match {
                case Input.Keyboard(_) | Input.Paste(_) =>
                  edit.flatMap(f => editor.apply(input, cols).map(e => lift(f(e))))
                case _ => None
              }
            claim(path :+ Stop(None, Some(keys), barrier = false))
          }
        }

      case Node.On(child, first, last, press) =>
        val stop = Stop[M](
          first.map(h => (i: Input) => h(i).map(lift)),
          last.map(h => (i: Input) => h(i).map(lift)),
          barrier = false
        )
        press.foreach(h => targets += Target.Press(rect, (p: Pos) => h(p).map(lift)))
        // A screen with nothing focusable inside still has its keys: the handlers end
        // the path when nothing below them claimed it.
        val before = claims
        walk(child, rect, lift, path :+ stop)
        if (claims == before) claim(path :+ stop)

      case Node.Dialog(base, modal, body, close) =>
        walk(base, rect, lift, path)
        val closeKey: OnInput[M] = input =>
          input match {
            case Input.Keyboard(Key.Escape) => close.map(lift)
            case _ => None
          }
        val stop = Stop[M](None, Some(closeKey), barrier = true)
        targets += Target.Wall(rect)
        cursor = None
        claim(path :+ stop)
        modal.place(rect.size).foreach { local =>
          val at = local.translate(Pos(rect.top, rect.left))
          if (cells) surface = modal.render(surface, at)
          walk(body, at, lift, path :+ stop)
        }

      case Node.Floating(host, popup, route) =>
        val box0 = rect
        val keys: OnInput[M] = input =>
          input match {
            case Input.Keyboard(_) =>
              popup.route(input, box0) match {
                case Popup.Route.Pass(_) => None
                case r => route(r).map(lift)
              }
            case _ => None
          }
        walk(host, rect, lift, path :+ Stop(Some(keys), None, barrier = false))
        deferred = deferred :+ Float(rect, popup, (r: Popup.Route) => route(r).map(lift))

      case m: Node.Mapped[x, A] => mapped(m, rect, lift, path)
    }

    private def mapped[X, A](
        m: Node.Mapped[X, A],
        rect: Rect,
        lift: A -> M,
        path: Vector[Stop[M]]
    ): Unit = {
      val f = m.f
      walk[X](m.inner, rect, (x: X) => lift(f(x)), path)
    }

    /** A pane whose key an earlier pane in this frame already took paints an error in its
      * box and routes nothing: the memo and a grab would otherwise confuse the two.
      */
    private def pane[A](d: Node.DocPane[A], rect: Rect, lift: A -> M, path: Vector[Stop[M]]): Unit =
      if (rect.rows > 0 && rect.cols > 0 && memos.contains(d.key)) {
        blit(Surface.blank(rect.size).write(0, 0, s"duplicate pane key: ${d.key.name}"), rect)
      } else if (rect.rows > 0 && rect.cols > 0) {
        val withBar = d.bar.isDefined && rect.cols >= 2
        val text = if (withBar) Rect(rect.top, rect.left, rect.rows, rect.cols - 1) else rect
        val track = if (withBar) Some(Rect(rect.top, rect.right - 1, rect.rows, 1)) else None
        val memo = old.docs.getOrElse(d.key, DocMemo.empty).synced(d.doc, text.cols)
        memos = memos.updated(d.key, memo)
        val top = memo.topFor(d.anchor, text.rows)
        val vp = memo.viewport(top, text.size)
        blit(vp.render(d.selection), text)
        (d.bar, track) match {
          case (Some((rail, thumb)), Some(t)) =>
            blit(Scrollbar(memo.total, text.rows, top, rail, thumb).render(t.size), t)
          case _ => ()
        }
        val painted = PanePaint[M](
          d.key,
          text,
          track,
          d.doc,
          memo,
          top,
          vp,
          d.scroll.map(f => (a: Anchor) => lift(f(a))),
          d.select.map(f => (s: Option[Selection]) => lift(f(s))),
          d.copy.map(f => (t: String, x: Boolean) => lift(f(t, x)))
        )
        targets += Target.Pane(painted)
        if (d.focused) { claim(path :+ Stop(None, Some(docKeys(painted)), barrier = false)) }
      }
  }
}
