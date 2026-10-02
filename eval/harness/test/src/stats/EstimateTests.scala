package grit.eval.harness.stats

import utest.*

/** A clustered mean and its error, against values worked by hand. */
object EstimateTests extends TestSuite {

  private def near(actual: Double, expected: Double, within: Double = 1e-4): Unit =
    assert(math.abs(actual - expected) < within)

  val tests = Tests {
    test("three clusters: CR1's standard error, its t interval on 2 df, and its MDE") {
      // Cluster a: 1, 0; b: 1; c: 0, 0, 1. n = 6, mean 3/6 = 0.5.
      // Residuals a: .5, −.5 (sum 0); b: .5 (sum .5); c: −.5, −.5, .5 (sum −.5).
      // V = g/(g−1) · Σ S² / n² = 3/2 · (0 + .25 + .25) / 36 = 0.0208333; se = 0.1443376.
      // (Unclustered, s/√n would be √(0.3/6) = 0.2236.)
      // t.975 on 2 df = 4.3027: interval 0.5 ± 0.6210450.
      // t.80 on 2 df = 1.0607: MDE (4.3027 + 1.0607) · 0.1443376 = 0.7741493.
      val e = Estimate.clustered(
        Vector("a" -> 1.0, "a" -> 0.0, "b" -> 1.0, "c" -> 0.0, "c" -> 0.0, "c" -> 1.0)
      )
      e.map(x => (x.n, x.g)) ==> Some((6, 3))
      e.foreach { x =>
        near(x.mean, 0.5)
        near(x.se, 0.1443376, 1e-6)
        near(x.interval._1, -0.1210450)
        near(x.interval._2, 1.1210450)
        near(x.mde, 0.7741493)
      }
    }

    test("unbalanced clusters: each item weighs alike, not each cluster") {
      // x: .2, .4, .6, .8; y: 1.0. n = 5, mean 3.0/5 = 0.6 (the clusters' means' mean is 0.75).
      // Residuals x: −.4, −.2, 0, .2 (sum −.4); y: .4. V = 2/1 · (.16 + .16) / 25 = .0256.
      val e = Estimate.clustered(
        Vector("x" -> 0.2, "x" -> 0.4, "x" -> 0.6, "x" -> 0.8, "y" -> 1.0)
      )
      e.map(x => (x.n, x.g)) ==> Some((5, 2))
      e.foreach { x =>
        near(x.mean, 0.6, 1e-9)
        near(x.se, 0.16, 1e-9)
      }
    }

    test("one cluster, or none, has no standard error") {
      Estimate.clustered(Vector("a" -> 1.0, "a" -> 0.0, "a" -> 1.0)) ==> None
      Estimate.clustered(Vector.empty[(String, Double)]) ==> None
    }

    test("paired: b less a over the items both hold, clustered") {
      // Items 1..4 in both, 5 only in b. Differences: 1: .1, 2: .3 (cluster p); 3: −.1,
      // 4: .1 (cluster q). n = 4, mean .1; residuals p: 0, .2 (sum .2); q: −.2, 0 (sum −.2).
      // V = 2 · (.04 + .04) / 16 = .01; se = .1.
      val a = Map(1 -> 0.5, 2 -> 0.2, 3 -> 0.6, 4 -> 0.0)
      val b = Map(1 -> 0.6, 2 -> 0.5, 3 -> 0.5, 4 -> 0.1, 5 -> 1.0)
      val e = Estimate.paired(a, b, i => if (i <= 2) "p" else "q")
      e.map(x => (x.n, x.g)) ==> Some((4, 2))
      e.foreach { x =>
        near(x.mean, 0.1, 1e-9)
        near(x.se, 0.1, 1e-9)
      }
    }

    test("the t table: 2.228 at 10 df, as printed tables give it, and the normal past 120") {
      near(Student.t975(10), 2.228, 5e-4)
      near(Student.t80(1), 1.3764)
      near(Student.t975(121), 1.96)
    }
  }
}
