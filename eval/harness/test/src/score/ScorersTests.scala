package grit.eval.harness.score

import grit.core.triage.Kind
import grit.eval.harness.label.{Context, Labelled, Labels, Place}
import grit.eval.harness.log.{Outcome, Suite}

import utest.*
import Fixtures.*

/** The scorers over a run's answers and a person's labels, against values worked by hand. */
object ScorersTests extends TestSuite {

  private def near(actual: Double, expected: Double, within: Double = 1e-6): Unit =
    assert(math.abs(actual - expected) < within)

  private def r(x: Double): Double = math.round(x * 1e9) / 1e9

  /** Cases `n` with probability `p` and label `yes`, each its own exchange and unenrolled. */
  private def judged(each: (Int, Double, Boolean)*): Vector[Judged] =
    each.toVector.map((n, p, yes) => Judged(caseOf(n, n), p, yes))

  val tests = Tests {
    test("a case's repeats are averaged, a choice normalised first, and their spread kept") {
      val (a, b) = (id("C1/1"), id("C1/2"))
      val rows = Vector(
        row(Suite.Triage, a, 0, triage(Vector(2, 2, 0, 0, 0), 0.25, 0.5, 0.75)),
        row(Suite.Triage, a, 1, triage(Vector(1, 0, 0, 0, 1), 0.5, 0.5, 0.5)),
        row(Suite.Triage, a, 2, Outcome.Skipped),
        row(Suite.Stitch, b, 0, stitch(Vector(3, 1))),
        row(Suite.Stitch, b, 1, stitch(Vector(1, 3)))
      )
      // a's kinds normalised: (.5, .5, 0, 0, 0) and (.5, 0, 0, 0, .5); mean (.5, .25, 0, 0,
      // .25), spread (0, .5, 0, 0, .5). Waiting .25 and .5: mean .375, spread .25. The
      // skipped repeat counts for nothing. b's: (.75, .25) and (.25, .75).
      def kinds(v: Double*) = Kind.values.toVector.zip(v).toMap
      Answers.of(rows) ==> Answers(
        Map(
          a -> Averaged(
            Triage(kinds(0.5, 0.25, 0, 0, 0.25), 0.375, 0.5, 0.625),
            Triage(kinds(0, 0.5, 0, 0, 0.5), 0.25, 0, 0.25),
            2
          )
        ),
        Map(b -> Averaged(Placing(Vector(0.5, 0.5)), Placing(Vector(0.5, 0.5)), 2))
      )
    }

    test("a tag's Brier, clustered by exchange, and by author with each unenrolled one alone") {
      // a (exchange 1, X) .8 yes: .04; b (1, none) .3 no: .09; c (3, X) .6 no: .36;
      // d (3, none) .5 yes: .25. n = 4, mean .74/4 = .185.
      // By exchange: {a, b} .13 − .37 = −.24, {c, d} .24. V = 2 · .1152 / 16; se .12.
      // By author: {a, c} .40 − .37 = .03, {b} −.095, {d} .065. Σ S² = .01415.
      // V = 3/2 · .01415 / 16 = .00132656; se .0364220, on 3 clusters.
      val scored = Brier.tag(
        Vector(
          Judged(caseOf(1, 1, Some("X")), 0.8, true),
          Judged(caseOf(2, 1), 0.3, false),
          Judged(caseOf(3, 3, Some("X")), 0.6, false),
          Judged(caseOf(4, 3), 0.5, true)
        )
      )
      val c = Clustered.of(scored)
      c.n ==> 4
      c.exchange.map(e => (r(e.mean), r(e.se), e.g)) ==> Some((0.185, 0.12, 2))
      c.author.map(e => (e.g, math.round(e.se * 1e6))) ==> Some((3, 36422L))
    }

    test("a kind's Brier sums every kind's squared error, not the labelled kind's alone") {
      // {question .7, answer .2, chatter .1} labelled question: .09 + .04 + .01 = .14.
      // {question 1} labelled answer: 1 + 1 = 2.
      val ps = Map(Kind.Question -> 0.7, Kind.Answer -> 0.2, Kind.Chatter -> 0.1)
      Brier
        .kind(
          Vector(
            (caseOf(1, 1), ps, Kind.Question),
            (caseOf(2, 2), Map(Kind.Question -> 1.0), Kind.Answer)
          )
        )
        .map(x => r(x._2)) ==> Vector(0.14, 2.0)
    }

    test("a place is scored at the position of the exchange labelled, beginning anew last") {
      val (r1, r2, other) = (id("C1/91"), id("C1/92"), id("C1/99"))
      val offered = (n: Int) => caseOf(n, n, slots = Some(Vector(r1, r2)))
      val cases = Vector(offered(1), offered(2), offered(3), offered(4))
      val answers = Answers(
        Map.empty,
        Vector(1, 2, 3)
          .map(n => id(s"C1/$n") -> Placing(Vector(0.5, 0.3, 0.2)))
          .toMap
          .updated(id("C1/4"), Placing(Vector(0.5, 0.5)))
          .map((k, p) => k -> Averaged(p, p, 1))
      )
      val labels = Labels(
        Map(
          id("C1/1") -> Labelled.Blank.copy(place = Some(Place.Follows(r2))),
          id("C1/2") -> Labelled.Blank.copy(place = Some(Place.Begins)),
          // Not offered: left out.
          id("C1/3") -> Labelled.Blank.copy(place = Some(Place.Follows(other))),
          // Its placing offers one exchange, the corpus two: left out.
          id("C1/4") -> Labelled.Blank.copy(place = Some(Place.Begins))
        ),
        Set("v1")
      )
      val scoring = Scoring(cases, answers, labels)
      scoring.place.map((c, _, at) => c.id.written -> at) ==> Vector("C1/1" -> 1, "C1/2" -> 2)
      scoring.unplaced ==> 2
      // At 1: .25 + .49 + .04 = .78; at 2 (beginning anew): .25 + .09 + .64 = .98.
      Brier.place(scoring.place).map(x => r(x._2)) ==> Vector(0.78, 0.98)
    }

    test("a score splits by context: ok and short alone, the unlabelled only in all") {
      val cases = Vector(caseOf(1, 1), caseOf(2, 2), caseOf(3, 3))
      val labels = Labels(
        Map(
          cases(0).id -> Labelled.Blank.copy(context = Some(Context.Ok)),
          cases(1).id -> Labelled.Blank.copy(context = Some(Context.Short))
        ),
        Set("v1")
      )
      val s = Split.of(cases.map(_ -> 1.0), labels)
      (s.all.n, s.ok.n, s.short.n) ==> (3, 1, 1)
    }

    test("reliability: five bins, the last taking 1, a bin of one exchange giving no rate") {
      // [0, .2): .1 no (exchange 1), .15 yes (2): predicted .125, rate .5 on 2 clusters.
      // [.4, .6): .4 yes, .55 yes, both exchange 1: predicted .475, rate 1 on one cluster.
      // [.8, 1]: .9 yes (1), 1.0 no (2): predicted .95, rate .5.
      val bins = Reliability.of(
        Vector(
          Judged(caseOf(1, 1), 0.1, false),
          Judged(caseOf(2, 2), 0.15, true),
          Judged(caseOf(3, 1), 0.4, true),
          Judged(caseOf(4, 1), 0.55, true),
          Judged(caseOf(5, 1), 0.9, true),
          Judged(caseOf(6, 2), 1.0, false)
        )
      )
      bins.map(b => (b.n, b.predicted.map(r), b.rate.exchange.map(e => r(e.mean)))) ==> Vector(
        (2, Some(0.125), Some(0.5)),
        (0, None, None),
        (2, Some(0.475), None),
        (0, None, None),
        (2, Some(0.95), Some(0.5))
      )
    }

    test("the sweep decides yes at or above each threshold, and marks the shipped one") {
      // Yes at .3, .6, .9; no at .1, .5. At .5: TPR 2/3, TNR 1/2 (.5 is decided yes).
      val points = Sweep.of(
        judged((1, 0.3, true), (2, 0.6, true), (3, 0.9, true), (4, 0.1, false), (5, 0.5, false)),
        Some(0.5)
      )
      def at(t: Double) = points
        .find(p => math.abs(p.threshold - t) < 1e-9)
        .map(p =>
          (p.tpr.exchange.map(e => r(e.mean)), p.tnr.exchange.map(e => r(e.mean)), p.shipped)
        )
      at(0.05) ==> Some((Some(1.0), Some(0.0), false))
      at(0.5) ==> Some((Some(r(2.0 / 3)), Some(0.5), true))
      at(0.95) ==> Some((Some(0.0), Some(1.0), false))
      points.count(_.shipped) ==> 1
    }

    test(
      "expected cost: the cheapest threshold (lowest on a tie), and whether shipped is within error"
    ) {
      // Ratio 1, the sweep's cases: a miss and a false alarm each cost 1. Cost per case is .4
      // at .05–.1 (both noes alarm), .2 at .15–.3 (only .5 alarms), .4 at .35–.5 (.3 missed,
      // .5 alarms), .2 at .55–.6, then more. Cheapest: .15. Shipped .5: .4. Paired, shipped
      // less best per case: 1 for the .3 yes, else 0: mean .2, se .2 on 5 clusters; t on 4 df
      // 2.7764, so the interval reaches below 0: within error.
      val five =
        judged((1, 0.3, true), (2, 0.6, true), (3, 0.9, true), (4, 0.1, false), (5, 0.5, false))
      val one = Cost.curve(five, 0.5).find(_.ratio == 1.0)
      one.map(c =>
        (
          r(c.best),
          c.atBest.exchange.map(e => r(e.mean)),
          c.atShipped.exchange.map(e => r(e.mean)),
          c.within
        )
      ) ==>
        Some((0.15, Some(0.2), Some(0.4), Some(true)))
      // Ratio 16, ten yeses at .3 and a no at .1, each its own exchange: cheapest .15 (nothing
      // missed or alarmed, cost 0); shipped .5 misses all ten, 160/11 = 14.545 per case.
      // Differences 16 ×10 and 0: se 1.4545, t on 10 df 2.228: the interval is above 0.
      val eleven = judged((0 until 10).map(n => (n + 10, 0.3, true)) :+ (30, 0.1, false)*)
      val sixteen = Cost.curve(eleven, 0.5).find(_.ratio == 16.0)
      sixteen.map(c => (r(c.best), c.atBest.exchange.map(e => r(e.mean)), c.within)) ==>
        Some((0.15, Some(0.0), Some(false)))
      sixteen.flatMap(_.atShipped.exchange).foreach(e => near(e.mean, 160.0 / 11))
      assert(Cost.curve(Vector.empty, 0.5).isEmpty)
    }
  }

}
