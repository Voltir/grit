package grit.tui.components.layout

import grit.tui.components.{Passive, View}
import grit.tui.model.surface.{PaneId, Pos, Rect, Size, Surface}

/** Where every region of an area is, once resolved, keyed by the [[PaneId]] each region
  * is found under. Lookups are total: a region the layout starved to nothing is still in
  * the map, as an empty rect.
  */
final case class Placed(rects: Map[PaneId, Rect], starved: Set[PaneId]) {

  def apply(pane: PaneId): Rect = rects(pane)

  /** This, plus `inner` resolved inside the rect already held for `pane`.
    *
    * [[Stack.views]] nests -- a `Split` composed into a `Stack`'s body region is one
    * `View` -- but resolving did not, so an app that both painted a nested layout and
    * needed its rects described the same tree twice, in two vocabularies, and kept them
    * in step by hand. This is the second description deleted: `chrome.resolve(size)`
    * `.nest(BodyPane, bodySplit)` resolves exactly what `chrome.views(..., bodySplit
    * .views(...), ...)` paints, and `PlacedTests` asserts that equality rather than
    * trusting it.
    *
    * A `pane` this does not hold is not an error: nothing was painted there either, so
    * there is nothing to nest inside and the answer is unchanged.
    */
  def nest(pane: PaneId, inner: Stack): Placed = merged(rects.get(pane).map(inner.resolveIn))

  /** As [[nest]], for a horizontal layout. */
  def nest(pane: PaneId, inner: Split): Placed = merged(rects.get(pane).map(inner.resolveIn))

  /** As [[nest]], for a layout that already holds its children -- the form a
    * [[Region.Fit]] needs, since what it resolves to is a question only a child can
    * answer.
    */
  def nest(pane: PaneId, inner: Regions): Placed = merged(rects.get(pane).map(inner.placedIn))

  private def merged(inner: Option[Placed]): Placed = inner match {
    case None => this
    case Some(p) => Placed(rects ++ p.rects, starved ++ p.starved)
  }
}

/** Regions stacked down the screen, each spanning the full width of its area. A region
  * is named by the [[PaneId]] it is laid out under -- the same name it is found by in
  * `onInput`, so there is no second id to keep in step.
  */
final case class Stack(regions: Vector[(PaneId, Region)]) {

  /** Resolve against a terminal size; the area is the whole screen. */
  def resolve(size: Size): Placed = resolveIn(Rect(0, 0, size.rows, size.cols))

  /** Resolve inside `area`; the rects come back in absolute coordinates, ready to
    * paint or hit-test.
    */
  def resolveIn(area: Rect): Placed = {
    Layout.childless(regions)
    Layout.resolve(regions, Vector.empty, area, vertical = true)
  }

  /** This layout with a view in each region, in declaration order: a [[View]] that
    * resolves the regions and paints each child into its own.
    *
    * Typed as [[Regions]] rather than [[View]] because it is the only thing that can
    * resolve a [[Region.Fit]] -- see [[Regions.placed]].
    */
  def views(children: View*): Regions = Regions(regions, children.toVector, vertical = true)
}

object Stack {
  def of(regions: (PaneId, Region)*): Stack = Stack(regions.toVector)
}

/** Regions laid side by side, each spanning the full height of its area. */
final case class Split(regions: Vector[(PaneId, Region)]) {

  /** Resolve against a terminal size; the area is the whole screen. */
  def resolve(size: Size): Placed = resolveIn(Rect(0, 0, size.rows, size.cols))

  /** Resolve inside `area` -- the body row of a [[Stack]], usually. */
  def resolveIn(area: Rect): Placed = {
    Layout.childless(regions)
    Layout.resolve(regions, Vector.empty, area, vertical = false)
  }

  /** This layout with a view in each region, in declaration order. See [[Stack.views]]. */
  def views(children: View*): Regions = Regions(regions, children.toVector, vertical = false)
}

object Split {
  def of(regions: (PaneId, Region)*): Split = Split(regions.toVector)
}

/** A resolved layout with a view in each region -- what [[Stack.views]] and
  * [[Split.views]] produce.
  *
  * Each child is painted under its region's name, so the surface that comes back says
  * where every region landed, in absolute coordinates, through however many levels of
  * nesting: `blit` translates a patch's own placements as it carries them up. That is
  * what lets an app name a region once and hit-test it later without keeping a parallel
  * map of rects in its own state.
  *
  * It is [[Passive]] on purpose. Its children speak different routing vocabularies --
  * that difference is deliberate and recorded -- so a container has no business
  * unifying them. It reports where they landed; the app dispatches.
  */
final case class Regions(
    regions: Vector[(PaneId, Region)],
    children: Vector[View],
    vertical: Boolean
) extends Passive {

  require(
    regions.length == children.length,
    s"a layout of ${regions.length} regions needs ${regions.length} views, got ${children.length}"
  )

  /** All of it: a layout fills what it is given, and reports what it could not serve
    * through `Placed.starved` rather than shrinking.
    */
  def measure(avail: Size): Size = avail

  /** Where the regions land at `size` -- resolved the way [[render]] resolves them,
    * children and all.
    *
    * [[Stack.resolve]] answers the same question without the children, and for a
    * layout of [[Region.Fixed]] and [[Region.Flex]] the two agree by construction.
    * A [[Region.Fit]] is what makes the difference matter: an `onResize` that wants
    * the rects of a content-sized layout has to ask the thing that holds the content.
    */
  def placed(size: Size): Placed = placedIn(Rect(0, 0, size.rows, size.cols))

  /** As [[placed]], inside an area already resolved by an enclosing layout. */
  def placedIn(area: Rect): Placed = Layout.resolve(regions, children, area, vertical)

  def render(size: Size): Surface = {
    val placed = placedIn(Rect(0, 0, size.rows, size.cols))
    regions.indices.foldLeft(Surface.blank(size)) { (s, i) =>
      val (pane, _) = regions(i)
      val r = placed(pane)
      s.blit(children(i).render(r.size), Pos(r.top, r.left), pane)
    }
  }
}

private object Layout {

  /** Refuse a layout that cannot answer for itself. A [[Region.Fit]] resolves to what
    * its child measures, so resolving without the children could only guess -- and the
    * guess would disagree with what [[Regions.render]] paints, silently, which is the
    * one failure `CompositionTests` exists to catch.
    */
  def childless(regions: Vector[(PaneId, Region)]): Unit = {
    var i = 0
    while (i < regions.length) {
      regions(i)(1) match {
        case Region.Fit(_, _) =>
          throw new IllegalArgumentException(
            s"region '${regions(i)(0).value}' is a Fit: it is sized by its child, so " +
              "resolve it through Regions.placed rather than without the children"
          )
        case _ => ()
      }
      i += 1
    }
  }

  /** Resolve `regions` along one axis of `area` ([[Stacking.shares]]), naming each rect
    * by its region and reporting every region served less than it demanded.
    *
    * `children` is empty for the childless paths ([[Stack.resolve]] and friends), which
    * [[childless]] has already established hold no [[Region.Fit]].
    */
  def resolve(
      regions: Vector[(PaneId, Region)],
      children: Vector[View],
      area: Rect,
      vertical: Boolean
  ): Placed = {
    val shares = Stacking.shares(
      regions.map(_(1)),
      area,
      vertical,
      (i, avail) => if i < children.length then children(i).measure(avail) else Size(0, 0)
    )
    val rects = Map.newBuilder[PaneId, Rect]
    val starved = Set.newBuilder[PaneId]
    var i = 0
    while (i < regions.length) {
      val name = regions(i)(0)
      rects += (name -> shares(i).rect)
      if shares(i).starved then starved += name
      i += 1
    }
    Placed(rects.result(), starved.result())
  }
}
