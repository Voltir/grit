package grit.eval.harness.score

import grit.core.id.ConversationId

import utest.*

/** A rate's interval: Wilson's on the effective number of turns, none under ten threads. The
  * pinned values were worked outside the code under test (z = 1.96).
  */
object RateTests extends TestSuite {

  private def r(x: Double): Double = math.round(x * 1e6) / 1e6

  /** `hits` of `n` turns, one a thread, the hits first. */
  private def apart(hits: Int, n: Int): Vector[(ConversationId, Boolean)] =
    Vector.tabulate(n)(i => ConversationId(s"c$i") -> (i < hits))

  private def bounds(rate: Rate): Option[(Double, Double)] = rate.interval match {
    case Rate.Interval.Wilson(low, high, _) => Some((low, high))
    case Rate.Interval.TooFewThreads => None
  }

  val tests = Tests {
    test("an interval never leaves [0, 1], a rate of none and of all included") {
      // Wald's over 11 of 12 reaches 1.100.
      val each = Vector(0, 1, 6, 11, 12).map(h => h -> bounds(Rate.of(apart(h, 12))))
      each.filterNot { (h, b) =>
        b.exists((low, high) => 0 <= low && low <= h / 12.0 && h / 12.0 <= high && high <= 1)
      } ==> Vector.empty
      // All 12 hit: the interval still has width below 1.
      bounds(Rate.of(apart(12, 12))).map((low, high) => (r(low), high)) ==> Some((0.757499, 1.0))
    }

    test("under ten threads a rate has no interval, 6 of 6 included") {
      Rate.of(apart(6, 6)) ==> Rate(6, 6, 6, Rate.Interval.TooFewThreads)
      // Twelve turns, nine threads.
      val nine = apart(5, 9) ++ Vector.fill(3)(ConversationId("c0") -> true)
      Rate.of(nine).interval ==> Rate.Interval.TooFewThreads
    }

    test("Wilson's interval on n over the design effect of clustering by thread") {
      // 7 of 10, one turn a thread: the design effect is 10/9, so 9 effective turns.
      Rate.of(apart(7, 10)).interval match {
        case Rate.Interval.Wilson(low, high, effective) =>
          (r(low), r(high), r(effective)) ==> (0.382484, 0.897855, 9.0)
        case other => throw new java.lang.AssertionError(other.toString)
      }
      // 7 of 12 over 10 threads, thread c0 holding three hits: the design effect is 1.507937.
      val clustered = Vector.fill(2)(ConversationId("c0") -> true) ++ apart(5, 10)
      Rate.of(clustered).interval match {
        case Rate.Interval.Wilson(low, high, effective) =>
          (r(low), r(high), r(effective)) ==> (0.273592, 0.838812, 7.957895)
        case other => throw new java.lang.AssertionError(other.toString)
      }
    }
  }
}
