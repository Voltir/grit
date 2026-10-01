package grit.tui.components.layout

import grit.tui.model.surface.{Rect, Size}

/** What one child of a box demands along the box's axis.
  *
  * A demand that cannot be met is never stolen back from a sibling and never paints
  * outside the parent: children are served in order and clamped in place.
  */
enum Region {

  /** Exactly `n` cells along the axis. */
  case Fixed(n: Int)

  /** Whatever remains after every fixed and fit child, shared evenly with the other
    * flexes. A fit child will not grow into the `min` a flex declared it needs.
    */
  case Flex(min: Int)

  /** As much as the child asks for through its measure, never below `min`, never more
    * than `upTo` of the axis, and never so much that a [[Flex]] sibling drops below its
    * own minimum -- whichever bites first. `min` wins over both caps.
    *
    * The region declares the *policy* and the child supplies the *demand*: a prompt that
    * grows with its draft is `Fit(min = 3, upTo = 0.5)` over an `Editor`, and the editor
    * says nothing about how tall it is allowed to get. There is no fixed point to
    * converge on: a fit varies the extent along the axis, and the child measures against
    * the full cross extent either way.
    */
  case Fit(min: Int, upTo: Double)
}

/** The stack arithmetic: where each child of a box goes. */
object Stacking {

  /** `demands` laid along one axis of `area` (down it when `vertical`), in order. Fixed
    * demands are served first, then fit demands out of one shared budget, then flexes
    * share what remains evenly with the first ones absorbing the rounding.
    * `measure(i, avail)` is what the `i`th child asks of `avail`; only a [[Region.Fit]]
    * is asked.
    */
  def rects(
      demands: Vector[Region],
      area: Rect,
      vertical: Boolean,
      measure: (Int, Size) => Size
  ): Vector[Rect] = {
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
    // is never smaller than what it declared it needs.
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

    val out = Vector.newBuilder[Rect]
    var cursor = 0
    var left = total
    var flexSeen = 0
    i = 0
    while (i < demands.length) {
      val got = demands(i) match {
        case Region.Fixed(n) => math.min(math.max(0, n), left)
        case Region.Fit(_, _) => math.min(fits(i), left)
        case Region.Flex(_) =>
          val n2 = math.min(left, share + (if flexSeen < extras then 1 else 0))
          flexSeen += 1
          n2
      }
      left -= got
      out += (if vertical then Rect(area.top + cursor, area.left, got, cross)
              else Rect(area.top, area.left + cursor, cross, got))
      cursor += got
      i += 1
    }
    out.result()
  }
}
