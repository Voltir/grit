package grit.eval.harness.score

import grit.core.id.WorkflowId
import grit.core.prompt.Layer
import grit.core.review.{Reason, Verdict}
import grit.eval.harness.corpus.{Drafted, Ended, Part}
import grit.eval.harness.stats.Proportion
import grit.turn.TurnOffer

import utest.*

/** A corpus's recorded turns read structurally, over five hand-built turns whose every
  * aggregate is worked by hand in the comments.
  */
object StructureTests extends TestSuite {

  import TurnFixtures.*

  private def r(x: Double): Double = math.round(x * 1e9) / 1e9
  private def q(q: Quantiles): (Int, Double, Double, Double) = (q.n, r(q.mean), r(q.p50), r(q.p90))

  private val all = Structure.of(turns, Slice.All, verdicts)

  val tests = Tests {
    test("quantiles: the mean, and the median and 90th percentile by nearest rank") {
      // Sorted 0 0 0 1 2: the 3rd smallest is the median, the 5th the 90th percentile.
      Quantiles.of(Vector(2.0, 0.0, 1.0, 0.0, 0.0)).map(q) ==> Some((5, 0.6, 0.0, 2.0))
      Quantiles.of(Vector(5.0, 1.0)).map(q) ==> Some((2, 3.0, 1.0, 5.0))
      Quantiles.of(Vector.empty) ==> None
    }

    test("the pass rate is over the turns that replied, counted by thread") {
      // Replied: t1 passed (A), t2 not (A), t4 not (C).
      all.passed ==> Proportion(1, 3, 2, Proportion.Interval.TooFewClusters)
    }

    test("the post and hold rates are over the drafts, counted by thread") {
      // Both drafts are thread A's: one posted, one held below the bar.
      all.online.map(o => (o.posted, o.held)) ==> Some(
        (
          Proportion(1, 2, 1, Proportion.Interval.TooFewClusters),
          Proportion(1, 2, 1, Proportion.Interval.TooFewClusters)
        )
      )
    }

    test("each slice holds its root's or its place's turns") {
      Slice.every.map(s => s -> Structure.of(turns, s, verdicts).n) ==> Vector(
        Slice.All -> 5,
        Slice.Rooted(TurnOffer.Root.Addressed) -> 3,
        Slice.Rooted(TurnOffer.Root.Heard) -> 2,
        Slice.At(Where.Slack) -> 3,
        Slice.At(Where.Tui) -> 1,
        Slice.At(Where.Task) -> 1
      )
    }

    test("the turns that did not reply, by step and kind or by status, in the order met") {
      all.unreplied.toVector ==> Vector(
        Unreplied.Failed("assemble", Ended.Why.Assembly) -> 1,
        Unreplied.Unfinished("PENDING") -> 1
      )
    }

    test("rounds per turn, 0 for none, and the turns with a loop") {
      // 2, 1, 0, 0, 0.
      (all.rounds.map(q), all.loops) ==> (Some((5, 0.6, 0.0, 2.0)), 2)
    }

    test("tools offered against called: each by name, the topic tool and unnamed calls apart") {
      // Offers: 2, 2, 1 tools; schema 100, 100, 40.
      (all.offered.map(q), all.schema.map(q)) ==>
        (Some((3, r(5.0 / 3), 2.0, 2.0)), Some((3, 80.0, 100.0, 100.0)))
      all.tools.toVector ==> Vector(
        read -> ToolUse(3, Calls(2, 1, Settlings(1, 1, 0, 0, 0))),
        search -> ToolUse(2, Calls(0, 0, Settlings(0, 0, 0, 0, 0)))
      )
      all.topic ==> Calls(1, 1, Settlings(1, 0, 0, 0, 0))
      all.unnamed ==> Calls(1, 1, Settlings(0, 0, 1, 0, 0))
    }

    test("prompt tokens by layer, 0 in an offer without the layer") {
      all.prompt.view.mapValues(q).toVector ==> Vector(
        Layer.Base -> (3, 500.0, 500.0, 500.0),
        // 50, 50, 0.
        Layer.Edge -> (3, r(100.0 / 3), 50.0, 50.0)
      )
    }

    test("window tokens by part kind, 0 in a window without the kind, and the whole") {
      // Windows: t1 open 30 record 10 gaps 2 own 8 (50); t2 open 20 closed 40 (60); t4
      // recent 60 recalled 15 own 4 (79).
      all.window.map(w =>
        (w.parts.view.mapValues(q).toVector, q(w.gaps), q(w.own), q(w.total))
      ) ==> Some(
        (
          Vector(
            Part.Kind.Open -> (3, r(50.0 / 3), 20.0, 30.0),
            Part.Kind.Closed -> (3, r(40.0 / 3), 0.0, 40.0),
            Part.Kind.Record -> (3, r(10.0 / 3), 0.0, 10.0),
            Part.Kind.Recent -> (3, 20.0, 0.0, 60.0),
            Part.Kind.Recalled -> (3, 5.0, 0.0, 15.0)
          ),
          (3, r(2.0 / 3), 0.0, 2.0),
          (3, 4.0, 4.0, 8.0),
          (3, 63.0, 60.0, 79.0)
        )
      )
    }

    test("the estimate against the ledger's input of the first call to the main model") {
      // First main calls: t1's round 0 (100 est, 80 in), t2's reply (50, 0), t4's (200, 100);
      // the ratio over inputs above 0: 1.25, 2.
      all.estimate.map(e => (q(e.estimated), q(e.actual), e.ratio.map(q))) ==> Some(
        (
          (3, r(350.0 / 3), 100.0, 200.0),
          (3, 60.0, 80.0, 100.0),
          Some((2, 1.625, 1.25, 2.0))
        )
      )
    }

    test("cost per turn by what it paid for, 0 in a turn without it, and unpriced rows counted") {
      all.cost.view.mapValues(q).toVector ==> Vector(
        Paid.Query -> (5, 0.0002, 0.0, 0.001),
        Paid.Rounds -> (5, 0.0004, 0.0, 0.002),
        // 0.003, 0.002, 0, 0.004, 0.
        Paid.Reply -> (5, 0.0018, 0.002, 0.004),
        Paid.Judge -> (5, 0.0, 0.0, 0.0),
        Paid.Summary -> (5, 0.0001, 0.0, 0.0005)
      )
      // 0.006, 0.002, 0, 0.0045, 0.
      (all.total.map(q), all.unpriced) ==> (Some((5, 0.0025, 0.002, 0.006)), 1)
    }

    test("speech outcomes, the post and hold rates, and the judge's scores") {
      all.online.map(o =>
        (o.n, o.outcomes.toVector, o.held.hits, o.grounded.map(q), o.worth.map(q))
      ) ==> Some(
        (
          2,
          Vector(Drafted.Kind.Below -> 1, Drafted.Kind.Posted -> 1),
          1,
          Some((2, 0.6, 0.4, 0.8)),
          Some((2, 0.65, 0.6, 0.7))
        )
      )
      Structure.of(turns, Slice.Rooted(TurnOffer.Root.Addressed), verdicts).online ==> None
    }

    test("verdicts joined by case, by why picked and the verdict; those no turn answers counted") {
      all.verdicts.toVector ==> Vector(
        (Reason.ShadowOnly, Verdict.Interruption) -> 1,
        (Reason.Both, Verdict.Welcome) -> 1
      )
      Structure.unjoined(turns, verdicts) ==> 1
    }

    test("used parts over the turns with a supported reply; none where nothing replied") {
      // t2: 0.5, 0.1; t4: 0.2, 0.0. Used at 0.3: one part, one turn; tops 0.5, 0.2.
      all.used.map(u => (u.turns, u.parts, u.used, u.using, q(u.top))) ==>
        Some((2, 4, 1, 1, (2, 0.35, 0.2, 0.5)))
      Vector(Slice.At(Where.Tui), Slice.At(Where.Task))
        .map(Structure.of(turns, _, verdicts).used) ==> Vector(None, None)
    }

    test("turns by cost, most first, a tie by workflow") {
      Structure.byCost(turns).map((t, c) => WorkflowId.value(t.workflow) -> c) ==> Vector(
        "w1" -> BigDecimal("0.006"),
        "w4" -> BigDecimal("0.0045"),
        "w2" -> BigDecimal("0.002"),
        "w3" -> BigDecimal(0),
        "w5" -> BigDecimal(0)
      )
    }
  }
}
