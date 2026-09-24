package grit.tui.components.layout

import grit.tui.model.surface.{Rect, Size}

/** What one region demands along the layout axis.
  *
  * A demand that cannot be met is never stolen back from other regions and never
  * paints outside the parent: regions are served in declaration order and clamped in
  * place, and the shortfall is *reported* (`Placed.starved`) so the app can react --
  * collapse the header, show a too-small notice -- rather than the layout guessing.
  */
enum Region {

  /** Exactly `n` cells along the axis. */
  case Fixed(n: Int)

  /** Whatever remains after every fixed region, at least `min` when that much exists.
    *
    * Since [[Fit]], `min` is no longer only a report: a fit region will not grow into
    * the space a flex declared it needs. It is still not stolen back from a [[Fixed]]
    * demand -- declaration order still wins there -- so this widened the word rather
    * than replacing it.
    */
  case Flex(min: Int)

  /** As much as the child asks for through `View.measure`, never below `min`, never
    * more than `upTo` of the axis, and never so much that a [[Flex]] sibling drops
    * below its own minimum -- whichever bites first. `min` wins over both caps, and a
    * region that cannot be given even that is reported starved, as a [[Fixed]] is.
    *
    * The region declares the *policy* and the child supplies the *demand*: that is why
    * there are two parameters and not three. A prompt that grows with its draft is
    * `Fit(min = 3, upTo = 0.5)` over an `Editor`, and the editor says nothing about how
    * tall it is allowed to get.
    *
    * A layout holding one of these cannot be resolved without its children --
    * [[Stack.resolve]] refuses, and [[Regions.placed]] is the way in.
    *
    * There is no fixed point to converge on: a `Fit` in a [[Stack]] varies the height
    * of a region whose width is the full area either way, so what the child measures
    * does not depend on what the measurement produced. A [[Split]] is the same
    * argument transposed, and one region cannot be fit on both axes at once.
    */
  case Fit(min: Int, upTo: Double)
}

/** One region's rect, and whether it was served less than it demanded. */
final case class Share(rect: Rect, starved: Boolean)

/** The stack arithmetic, positional: one implementation behind both the named layouts
  * ([[Stack]], [[Split]], [[Regions]]) and the view tree's boxes (`grit.tui.node`).
  */
object Stacking {

  /** `demands` laid along one axis of `area` (down it when `vertical`), in order. Fixed
    * demands are served first, then fit demands out of one shared budget, then flexes
    * share what remains evenly with the first ones absorbing the rounding. A demand
    * that cannot be met is clamped in place and reported, never stolen back from a
    * sibling. `measure(i, avail)` is what the `i`th child asks of `avail`; only a
    * [[Region.Fit]] is asked.
    */
  def shares(
      demands: Vector[Region],
      area: Rect,
      vertical: Boolean,
      measure: (Int, Size) => Size
  ): Vector[Share] = {
    val total = if vertical then area.rows else area.cols
    val cross = if vertical then area.cols else area.rows

    // What the other two kinds have already spoken for: a fit grows into neither.
    var fixedDemand = 0
    var flexCount = 0
    var flexReserve = 0
    var i = 0
    while (i < demands.length) {
      demands(i) match {
        case Region.Fixed(n) => fixedDemand += math.max(0, n)
        case Region.Flex(min) => flexCount += 1; flexReserve += math.max(0, min)
        case Region.Fit(_, _) => ()
      }
      i += 1
    }

    // Each fit, in order, out of one shared budget. `min` wins over both caps, so a fit
    // is never smaller than what it declared it needs -- and what that costs a flex
    // sibling is reported below, the way a starved fixed region is.
    val fits = new Array[Int](demands.length)
    val offered = math.max(0, total - fixedDemand)
    var fitDemand = 0
    i = 0
    while (i < demands.length) {
      demands(i) match {
        case Region.Fit(min, upTo) =>
          val avail = offered - fitDemand
          val m = measure(i, if vertical then Size(avail, cross) else Size(cross, avail))
          val want = if vertical then m.rows else m.cols
          val cap = math.floor(total.toDouble * upTo).toInt
          val budget = math.max(0, total - fixedDemand - flexReserve - fitDemand)
          val got = math.max(math.max(0, min), math.min(want, math.min(cap, budget)))
          fits(i) = got
          fitDemand += got
        case _ => ()
      }
      i += 1
    }

    val remaining = math.max(0, total - fixedDemand - fitDemand)
    val share = if flexCount == 0 then 0 else remaining / flexCount
    val extras = if flexCount == 0 then 0 else remaining % flexCount

    val out = Vector.newBuilder[Share]
    var cursor = 0
    var left = total
    var flexSeen = 0
    i = 0
    while (i < demands.length) {
      val (got, want) = demands(i) match {
        case Region.Fixed(n) => (math.min(math.max(0, n), left), math.max(0, n))
        case Region.Fit(_, _) => (math.min(fits(i), left), fits(i))
        case Region.Flex(min) =>
          val n2 = math.min(left, share + (if flexSeen < extras then 1 else 0))
          flexSeen += 1
          (n2, min)
      }
      left -= got
      val rect =
        if vertical then Rect(area.top + cursor, area.left, got, cross)
        else Rect(area.top, area.left + cursor, cross, got)
      out += Share(rect, got < want)
      cursor += got
      i += 1
    }
    out.result()
  }
}
