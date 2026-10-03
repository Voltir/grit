package grit.eval.harness.stats

import utest.*

/** A proportion's interval: Wilson's on the effective number of items, none under ten
  * clusters. The pinned values were worked outside the code under test (z = 1.96).
  */
object ProportionTests extends TestSuite {

  private def r(x: Double): Double = math.round(x * 1e6) / 1e6

  /** `hits` of `n` items, one a cluster, the hits first. */
  private def apart(hits: Int, n: Int): Vector[(String, Boolean)] =
    Vector.tabulate(n)(i => s"c$i" -> (i < hits))

  private def bounds(p: Proportion): Option[(Double, Double)] = p.interval match {
    case Proportion.Interval.Wilson(low, high, _) => Some((low, high))
    case Proportion.Interval.TooFewClusters => None
  }

  val tests = Tests {
    test("an interval never leaves [0, 1], a rate of none and of all included") {
      // Wald's over 11 of 12 reaches 1.100.
      val each = Vector(0, 1, 6, 11, 12).map(h => h -> bounds(Proportion.of(apart(h, 12))))
      each.filterNot { (h, b) =>
        b.exists((low, high) => 0 <= low && low <= h / 12.0 && h / 12.0 <= high && high <= 1)
      } ==> Vector.empty
      // All 12 hit: the interval still has width below 1.
      bounds(Proportion.of(apart(12, 12))).map((low, high) => (r(low), high)) ==> Some(
        (0.757499, 1.0)
      )
    }

    test("under ten clusters a proportion has no interval, 6 of 6 included") {
      Proportion.of(apart(6, 6)) ==> Proportion(6, 6, 6, Proportion.Interval.TooFewClusters)
      // Twelve items, nine clusters.
      val nine = apart(5, 9) ++ Vector.fill(3)("c0" -> true)
      Proportion.of(nine).interval ==> Proportion.Interval.TooFewClusters
    }

    test("Wilson's interval on n over the design effect of clustering") {
      // 7 of 10, one item a cluster: the design effect is 10/9, so 9 effective items.
      Proportion.of(apart(7, 10)).interval match {
        case Proportion.Interval.Wilson(low, high, effective) =>
          (r(low), r(high), r(effective)) ==> (0.382484, 0.897855, 9.0)
        case other => throw new java.lang.AssertionError(other.toString)
      }
      // 7 of 12 over 10 clusters, cluster c0 holding three hits: the design effect is 1.507937.
      val clustered = Vector.fill(2)("c0" -> true) ++ apart(5, 10)
      Proportion.of(clustered).interval match {
        case Proportion.Interval.Wilson(low, high, effective) =>
          (r(low), r(high), r(effective)) ==> (0.273592, 0.838812, 7.957895)
        case other => throw new java.lang.AssertionError(other.toString)
      }
    }
  }
}
