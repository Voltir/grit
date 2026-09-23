package grit.tui.components.pane

import grit.tui.components.layout.Placed
import grit.tui.components.{Passive, View}
import grit.tui.model.select.{Doc, Drag, Selection}
import grit.tui.model.surface.{Hit, PaneId, Placement, Pos, Rect, Size, Surface}
import grit.tui.model.text.WrapCache

/** The panes an app is showing, what each was last laid out to show, which one has
  * focus, and the one drag that may be in flight.
  *
  * The viewport is owned here, not on the pane ([[TextPane]] carries only the document,
  * the anchor and the wrap cache), because hit-testing must invert *what was painted* --
  * and what was painted is what [[layout]] last stored, not what a fresh computation
  * would produce. The content mutators keep the two from drifting: [[withDoc]] and
  * [[withAnchor]] re-lay the pane out at the size it was last painted at, so a viewport
  * describing an older document is not something a caller can forget to refresh. A pane
  * never laid out relayouts to empty.
  *
  * The drag lives here rather than in a pane because there is one pointer: two panes
  * cannot both be dragging, and making that a shape rather than a rule removes the state
  * where they are.
  *
  * [[onPress]] and [[onDrag]] are where rule 6 is spent. A press hit-tests; a drag never
  * does. Once the button is down the pointer is resolved against the *drag's own* pane's
  * placement and clamped into that pane's document, so motion over another pane -- or off
  * the screen entirely -- extends the selection to the edge of where it started and no
  * further. A selection cannot escape a modal into what is painted behind it, because
  * there is no code path that would let it.
  */
final case class Panes(
    panes: Vector[TextPane],
    views: Map[PaneId, Viewport] = Map.empty,
    focus: Option[PaneId] = None,
    drag: Option[Drag] = None
) {

  def get(id: PaneId): Option[TextPane] = panes.find(_.id == id)

  /** What `id` was last laid out to show -- the viewport [[view]] paints and input maps
    * back through. `Viewport.empty` for a pane never laid out.
    *
    * This reads [[views]] rather than recomputing on purpose: between the frame and the
    * click the document may have grown, and re-laying out to answer a click would move
    * the selection under the pointer.
    */
  def rendered(id: PaneId): Viewport = views.getOrElse(id, Viewport.empty)

  /** The document row index of `id` -- one `DocPos` per wrapped row, what the
    * scrollbar's geometry speaks and a thumb drag reads back through. Maintained by the
    * same funnel that maintains the viewport ([[withDoc]], [[resetDoc]], [[layout]]),
    * so it is never asked to be rebuilt by hand. Empty for a pane never laid out.
    */
  def rowIndex(id: PaneId): RowIndex = get(id).map(_.index).getOrElse(RowIndex.empty)

  def focused: Option[TextPane] = focus.flatMap(get)

  def focusOn(id: PaneId): Panes = copy(focus = Some(id))

  /** `pane` stored in place of the one with its id (appended if new), with `vp` as the
    * viewport it was laid out to.
    */
  private def stored(pane: TextPane, vp: Viewport): Panes = {
    val ps = panes.indexWhere(_.id == pane.id) match {
      case -1 => panes :+ pane
      case i => panes.updated(i, pane)
    }
    copy(panes = ps, views = views.updated(pane.id, vp))
  }

  /** One pane laid out at `size`, its wrapped rows, row index and viewport stored back.
    * Called from `update`: this is the wrapping that rule 7 keeps out of `view`. The
    * index reconciles at the new width first, because a width change makes every row
    * count it holds wrong.
    */
  def layout(id: PaneId, size: Size): Panes =
    get(id)
      .map { p =>
        val (_, p1) = p.reindexedAt(math.max(1, size.cols))
        val (vp, np) = p1.viewport(size)
        stored(np, vp)
      }
      .getOrElse(this)

  /** Every pane `placed` holds a rect for, laid out at that rect's size.
    *
    * The bulk form of [[layout]], and the one an `onResize` wants: a resolved layout
    * names every region of the screen, and the panes among them are exactly the ones
    * that need re-wrapping. A region that is not a pane is skipped rather than
    * complained about -- a header is a region and not a document, and a layout is
    * allowed to hold both.
    */
  def layoutIn(placed: Placed): Panes =
    placed.rects.foldLeft(this) { (acc, entry) => acc.layout(entry._1, entry._2.size) }

  /** `id` showing `doc`, immediately re-laid out at the size it was last painted at.
    * This is the only path a pane's content changes through: the recorded pane rule --
    * a pane re-wraps at the size it was last painted at -- enforced by the shape rather
    * than by a caller remembering to follow [[layout]]. The row index reconciles in the
    * same pass, so a stale one is unrepresentable here too.
    */
  def withDoc(id: PaneId, doc: Doc): Panes =
    get(id).map(p => relaid(p.withDoc(doc))).getOrElse(this)

  /** `id` showing whatever `f` makes of the document it holds, re-laid out as
    * [[withDoc]] does. The read-modify-write every app writes out by hand, and the
    * reason to have it is that the hand-written form has to remember to fetch the pane
    * first and to do nothing when there is none.
    */
  def modify(id: PaneId)(f: Doc => Doc): Panes =
    get(id).map(p => withDoc(id, f(p.doc))).getOrElse(this)

  /** `id` showing `doc` from scratch, its wrap cache and row index dropped along with
    * the document they wrapped. For wholesale replacement -- a `/clear` -- where entry
    * indices restart at zero and per-entry revisions cannot say that nothing of the old
    * wrapping survives: a fresh block carrying a stale revision would hit the slot it
    * used to occupy and paint the text it replaced.
    */
  def resetDoc(id: PaneId, doc: Doc): Panes =
    get(id)
      .map(p =>
        relaid(p.copy(doc = doc, cache = WrapCache.empty(p.cache.width), index = RowIndex.empty))
      )
      .getOrElse(this)

  /** `id` emptied wholesale: the named path to [[resetDoc]] for a `/clear`. */
  def clear(id: PaneId): Panes = resetDoc(id, Doc.empty)

  /** `id` reading from `anchor`, re-laid out exactly as [[withDoc]]. */
  def withAnchor(id: PaneId, anchor: Anchor): Panes =
    get(id).map(p => relaid(p.withAnchor(anchor))).getOrElse(this)

  /** `id` scrolled `delta` rows (negative walks back through the document), at the size
    * it was last painted at -- which is the size every caller used to pass by hand.
    */
  def scrollBy(id: PaneId, delta: Int): Panes =
    get(id)
      .map { p =>
        val (vp, np) = p.scrolledBy(delta, rendered(id).size)
        stored(np, vp)
      }
      .getOrElse(this)

  /** `p` re-laid out at the viewport size it already has, its row index reconciled
    * against the document it now holds. A pane never laid out skips the reconcile:
    * nothing is wrapped yet, and the first [[layout]] builds the index at a real width
    * rather than at the degenerate one an unpainted viewport would suggest.
    */
  private def relaid(p: TextPane): Panes = {
    val size = rendered(p.id).size
    val indexed =
      if (size == Size(0, 0)) { p }
      else { p.reindexedAt(math.max(1, size.cols))._2 }
    val (vp, np) = indexed.viewport(size)
    stored(np, vp)
  }

  /** The selection to paint in `id` -- Some only for the pane the drag belongs to. */
  def selectionIn(id: PaneId): Option[Selection] =
    drag.filter(_.pane == id).map(_.selection)

  /** `id`'s laid-out viewport as a [[View]] -- the adapter every app of this library
    * would otherwise copy for each pane it shows: the rows as last laid out, the drag's
    * own selection masked over them, nothing else. It is `Passive` because a pane's
    * input is not a keystroke it can answer alone: a drag belongs to the pane it began
    * in (rule 6), which only `Panes` can route.
    */
  def view(id: PaneId): View = new Passive {
    def measure(avail: Size): Size = avail
    def render(size: Size): Surface =
      Surface.blank(size).blit(rendered(id).render(selectionIn(id)), Pos(0, 0), id)
  }

  /** A button press at a screen position: focus and begin a drag in whichever pane is
    * topmost there, or clear the drag if the press landed on nothing.
    */
  def onPress(places: Vector[Placement], pos: Pos): Panes =
    Hit.paneAt(places, pos).flatMap { (id, local) =>
      get(id).flatMap(p => rendered(id).docPosAt(local).map(dp => (p, dp)))
    } match {
      case Some((p, dp)) => copy(focus = Some(p.id), drag = Some(Drag.start(p.id, dp, p.doc)))
      case None => copy(drag = None)
    }

  /** Motion with the button down. Deliberately does not hit-test: the pointer is read in
    * the coordinates of the pane the drag began in, clamped to that pane's painted rect.
    */
  def onDrag(places: Vector[Placement], pos: Pos): Panes =
    drag.flatMap(d => get(d.pane).map(p => (d, p))) match {
      case None => this
      case Some((d, p)) =>
        rectOf(places, d.pane) match {
          case None => this
          case Some(rect) =>
            val local = Pos(
              clamp(pos.row - rect.top, rect.rows),
              clamp(pos.col - rect.left, rect.cols)
            )
            rendered(d.pane).docPosAt(local) match {
              case Some(to) => copy(drag = Some(d.extend(to, p.doc)))
              case None => this
            }
        }
    }

  /** The button coming up: the drag ends, and what it selected is offered for copying.
    * None when nothing was selected, so a plain click never clears the clipboard.
    */
  def onRelease: (Panes, Option[String]) = {
    val text = drag
      .flatMap(d => get(d.pane).map(p => p.doc.textOf(d.selection)))
      .filter(_.nonEmpty)
    (copy(drag = None), text)
  }

  /** Where a pane last painted, topmost placement first. */
  private def rectOf(places: Vector[Placement], id: PaneId): Option[Rect] =
    places.findLast(_.pane == id).map(_.rect)

  private def clamp(v: Int, extent: Int): Int =
    math.max(0, math.min(v, math.max(0, extent - 1)))
}

object Panes {

  val empty: Panes = Panes(Vector.empty)

  def of(panes: TextPane*): Panes = Panes(panes.toVector)
}
