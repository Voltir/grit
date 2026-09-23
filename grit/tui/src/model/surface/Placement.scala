package grit.tui.model.surface

/** Names a pane of the UI -- a transcript, a modal -- for hit-testing and selection. */
opaque type PaneId = String

object PaneId {
  def of(s: String): PaneId = s
  extension (p: PaneId) def value: String = p
}

/** Where a pane landed: the pane's identity and the rect it actually painted,
  * already clipped to the grid. Later blits stack after earlier ones, so the map is
  * innermost-last and a backwards walk finds the topmost pane first.
  */
final case class Placement(pane: PaneId, rect: Rect)

/** Hit-testing: the inverse of rendering. */
object Hit {

  /** The topmost pane under `pos`, with `pos` converted to that pane's local
    * coordinates (relative to its top-left), or None if no pane covers it.
    *
    * Takes the placement map rather than the surface, because an app that lays its panes
    * out in `update` has the map before it has a frame -- and hit-testing against the
    * layout the frame was painted from is the same thing as hit-testing the frame.
    */
  def paneAt(panes: Vector[Placement], pos: Pos): Option[(PaneId, Pos)] = {
    var i = panes.length - 1
    while (i >= 0) {
      val p = panes(i)
      if (p.rect.contains(pos))
        return Some((p.pane, Pos(pos.row - p.rect.top, pos.col - p.rect.left)))
      i -= 1
    }
    None
  }

  /** The topmost pane of a painted surface under `pos`. */
  def paneAt(s: Surface, pos: Pos): Option[(PaneId, Pos)] = paneAt(s.panes, pos)
}

/** Where the last painted frame put every named thing.
  *
  * The inverse of rendering, handed to `onInput` so binding can ask *where* something
  * is instead of the app maintaining a parallel map of rects in its own state and
  * remembering to refresh it. Lookups are innermost-first, matching [[Hit.paneAt]]:
  * `blit` appends, so the last placement for a name is the topmost one.
  */
final case class Placements(all: Vector[Placement]) {

  /** Where `pane` was painted, or `None` if it was not on the last frame. */
  def apply(pane: PaneId): Option[Rect] = all.findLast(_.pane == pane).map(_.rect)

  /** Whether `pane` was painted at all. */
  def contains(pane: PaneId): Boolean = apply(pane).isDefined

  /** The innermost pane under `pos`, and the position within it. */
  def at(pos: Pos): Option[(PaneId, Pos)] = Hit.paneAt(all, pos)

  /** True before anything has been painted -- the state the very first input arrives
    * in, since the runtime delivers a synthetic `Resize` before the first frame.
    */
  def isEmpty: Boolean = all.isEmpty

  def nonEmpty: Boolean = all.nonEmpty
}

object Placements {

  /** Nothing painted yet. */
  val empty: Placements = Placements(Vector.empty)
}
