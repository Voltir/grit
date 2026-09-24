package grit.tui.components.tree

import grit.tui.components.view.View
import grit.tui.components.layout.Region
import grit.tui.components.editor.Editor
import grit.tui.components.overlay.{Modal, Popup}
import grit.tui.components.pane.Anchor
import grit.tui.model.input.Input
import grit.tui.model.select.{Doc, Selection}
import grit.tui.model.surface.{Pos, Rect, Size, Style}

/** A document pane's identity across frames: the wrap memo and a pointer grab are both
  * found by it, so two panes painted in one frame may not share one (the second paints
  * an error in its place instead).
  */
opaque type PaneKey = String

object PaneKey {
  def of(name: String): PaneKey = name
  extension (k: PaneKey) def name: String = k
}

/** One key handler along the focus path. `None` from a handler means "not mine". */
type OnInput[+M] = Input -> Option[M]

/** A screen, as one tree that carries its own handlers (Elm's `Html msg`).
  *
  * **Every handler is a pure function** -- `A -> M`, the capture-checked pure arrow -- so
  * a handler cannot close over the terminal, the scheduler or any other capability: the
  * tree is data plus pure functions, and `Effect` (plain data, rule 3) stays the only way
  * out. Layout is not in here at all: the runtime resolves it when it paints, and routes
  * input against what it painted.
  */
sealed trait Node[+M]

object Node {

  /** Children stacked down (`vertical`) or across, each taking what its [[Region]] demands,
    * resolved by the same arithmetic as the named layouts (`Stacking`). A child needs no
    * name: it is where it is, so there is nothing to look it up by.
    */
  final case class Box[+M](vertical: Boolean, parts: Vector[(Region, Node[M])]) extends Node[M]

  /** A passive `View` painted into its box: chrome, a status bar. Routes nothing. */
  final case class Paint(view: View) extends Node[Nothing]

  /** A scrollable document. `key` is its identity across frames ([[PaneKey]]). Scroll position and selection are the
    * app's state, handed in; a scroll or a selection the user makes comes back through
    * the handlers as the *new* value, already computed against what was painted.
    */
  final case class DocPane[+M](
      key: PaneKey,
      doc: Doc,
      anchor: Anchor,
      selection: Option[Selection] = None,
      bar: Option[(Style, Style)] = None,
      focused: Boolean = false,
      scroll: Option[Anchor -> M] = None,
      select: Option[Option[Selection] -> M] = None,
      copy: Option[(String, Boolean) -> M] = None
  ) extends Node[M]

  /** The prompt editor. Its own keys edit it; an edit comes back through `edit`. Its
    * height is what `Region.Fit` asks it for.
    */
  final case class Edit[+M](editor: Editor, edit: Option[Editor -> M]) extends Node[M]

  /** `child` with handlers of its own: `first` runs before anything below it on the focus
    * path (capture phase: hotkeys), `last` after everything below it declined (bubble
    * phase: Enter submits), `press` for a press that lands in it and that nothing inside
    * it claimed. When nothing inside `child` is focused, this ends the focus path itself,
    * so a screen with no editor or focused pane still has its keys.
    */
  final case class On[+M](
      child: Node[M],
      first: Option[OnInput[M]] = None,
      last: Option[OnInput[M]] = None,
      press: Option[Pos -> Option[M]] = None
  ) extends Node[M]

  /** `base`, with `modal` over it: centred, the base dimmed, and **every input captured**
    * -- nothing below it on the screen sees a press, a wheel, or a key. `close` is what
    * Escape means.
    */
  final case class Dialog[+M](base: Node[M], modal: Modal, body: Node[M], close: Option[M])
      extends Node[M]

  /** `host`, with a completion list floating over it (above, or below when there is no
    * room), painted on top of everything else. Its arrows/Enter/Tab/Escape run before
    * anything inside `host` (so they win over the editor); everything else passes on.
    */
  final case class Floating[+M](host: Node[M], popup: Popup, route: Popup.Route -> Option[M])
      extends Node[M]

  /** A component's tree, its messages lifted into its parent's. */
  final case class Mapped[A, +M](inner: Node[A], f: A -> M) extends Node[M]

  /* ---- constructors ------------------------------------------------------------ */

  def column[M](parts: (Region, Node[M])*): Node[M] = Box(vertical = true, parts.toVector)
  def row[M](parts: (Region, Node[M])*): Node[M] = Box(vertical = false, parts.toVector)
  def paint(view: View): Node[Nothing] = Paint(view)

  def doc(key: PaneKey, doc: Doc, anchor: Anchor): DocPane[Nothing] = DocPane(key, doc, anchor)
  def editor(e: Editor): Edit[Nothing] = Edit(e, None)

  def fixed(n: Int): Region = Region.Fixed(n)
  def flex(min: Int = 0): Region = Region.Flex(min)
  def fit(min: Int, upTo: Double): Region = Region.Fit(min, upTo)

  /* ---- handlers ---------------------------------------------------------------- */

  extension [M](n: DocPane[M]) {
    def selected(s: Option[Selection]): DocPane[M] = n.copy(selection = s)
    def scrollbar(rail: Style, thumb: Style): DocPane[M] = n.copy(bar = Some((rail, thumb)))
    def focus(on: Boolean = true): DocPane[M] = n.copy(focused = on)
    def onScroll[N >: M](f: Anchor -> N): DocPane[N] = n.copy(scroll = Some(f))
    def onSelect[N >: M](f: Option[Selection] -> N): DocPane[N] = n.copy(select = Some(f))

    /** The text a finished drag selected, and whether the escaped-drag deadline ended
      * it rather than a release.
      */
    def onCopy[N >: M](f: (String, Boolean) -> N): DocPane[N] = n.copy(copy = Some(f))
  }

  extension [M](n: Edit[M]) {
    def onEdit[N >: M](f: Editor -> N): Edit[N] = n.copy(edit = Some(f))
  }

  extension [M](n: Node[M]) {
    def onKeyFirst[N >: M](f: OnInput[N]): Node[N] = On(n, first = Some(f))
    def onKey[N >: M](f: OnInput[N]): Node[N] = On(n, last = Some(f))
    def onPress[N >: M](f: Pos -> Option[N]): Node[N] = On(n, press = Some(f))
    def map[N](f: M -> N): Node[N] = Mapped(n, f)
    def dialog[N >: M](modal: Modal, body: Node[N], close: Option[N]): Node[N] =
      Dialog(n, modal, body, close)
    def floating[N >: M](popup: Popup, route: Popup.Route -> Option[N]): Node[N] =
      Floating(n, popup, route)
    def when[N >: M](cond: Boolean)(f: Node[M] -> Node[N]): Node[N] = if (cond) f(n) else n
  }

  /** Where a modal's body goes in a screen of `size` -- the dialog's geometry, reused. */
  def modalBody(m: Modal, size: Size): Option[Rect] = m.place(size)
}
