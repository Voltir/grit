package grit.eval.harness.score

import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.stitch.Offered
import grit.core.triage.Kind
import grit.eval.harness.corpus.{Case, Offering, Placement, SeenCheck, Stitched}
import grit.eval.harness.log.{Footer, Log, Outcome, Suite}

import utest.*
import Fixtures.*

/** What needs no label: a run against live, its repeats' spread, and what it consumed. */
object AgreementTests extends TestSuite {

  private def r(x: Double): Double = math.round(x * 1e9) / 1e9

  private def kinds(top: Kind, p: Double): Map[Kind, Double] =
    Kind.values.toVector.map(k => k -> (if (k == top) p else (1 - p) / 4)).toMap

  // Case 1 live: question, durable .6, helps .4; case 2: answer, durable .3, helps .7; case 3
  // failed live.
  private val one = caseOf(1, 1, live = weighed(Kind.Question, 0.9, 0.2, 0.6, 0.4))
  private val two = caseOf(2, 2, live = weighed(Kind.Answer, 0.8, 0.5, 0.3, 0.7))
  private val three = caseOf(3, 3)
  private val answers = Answers(
    Map(
      one.id -> Averaged(
        Triage(kinds(Kind.Question, 0.7), 0.3, 0.4, 0.4),
        Triage(Kind.values.toVector.map(_ -> 0.1).toMap, 0.05, 0.2, 0.0),
        2
      ),
      two.id -> Averaged(
        Triage(kinds(Kind.Question, 0.6), 0.5, 0.5, 0.8),
        Triage(Map.empty, 0, 0, 0),
        1
      ),
      three.id -> Averaged(Triage(kinds(Kind.Chatter, 1), 0, 0, 0), Triage(Map.empty, 0, 0, 0), 1)
    ),
    Map.empty
  )
  private val cases: Vector[Case] = Vector(one, two, three)

  val tests = Tests {
    test("against live: the likeliest kind, each tag's distance, and decisions at a threshold") {
      Agreement.kind(cases, answers).map(_._2) ==> Vector(true, false)
      Agreement.tag(Tag.Durable, cases, answers).map(x => r(x._2)) ==> Vector(0.2, 0.2)
      // Durable at .5: case 1 run .4 no, live .6 yes; case 2 run .5 yes (at it), live .3 no.
      Agreement.decided(Tag.Durable, 0.5, cases, answers).map(_._2) ==> Vector(false, false)
      // Helps at .7: case 1 .4 and .4, no both; case 2 .8 and .7 (at it), yes both.
      Agreement.decided(Tag.Helps, 0.7, cases, answers).map(_._2) ==> Vector(true, true)
    }

    test("against live, a place: the largest distance over exchanges with a live probability") {
      def placed(n: Int, seen: SeenCheck) = caseOf(n, n).copy(stitch =
        Some(
          Stitched(
            Placement.Begins(Probability.Zero),
            Vector(
              Offering(id("C1/91"), Offered.Recent(1), Some(Probability.clamped(0.6))),
              Offering(id("C1/92"), Offered.Recent(2), None)
            ),
            None,
            Vector.empty,
            seen,
            0
          )
        )
      )
      val (matched, differs, short) =
        (placed(4, SeenCheck.Match), placed(5, SeenCheck.Unbuilt), placed(6, SeenCheck.Match))
      val p = (ps: Vector[Double]) => Averaged(Placing(ps), Placing(ps), 1)
      val stitched = Answers(
        Map.empty,
        Map(
          matched.id -> p(Vector(0.5, 0.3, 0.2)),
          differs.id -> p(Vector(0.5, 0.3, 0.2)),
          short.id -> p(Vector(0.5, 0.5))
        )
      )
      // .5 against .6; the second has no live probability. 5's seen check differs, and 6
      // offers one exchange fewer: both left out.
      Agreement
        .place(Vector(matched, differs, short), stitched)
        .map((c, v) => c.id.written -> r(v)) ==>
        Vector("C1/4" -> 0.1)
    }

    test("repeat spread: only cases answered twice or more, a kind's largest") {
      Repeated
        .spread(Target.Tagged(Tag.Durable), cases, answers)
        .map(x => x._1.id.written -> x._2) ==>
        Vector("C1/1" -> 0.2)
      Repeated.spread(Target.Kinds, cases, answers).map(_._2) ==> Vector(0.1)
    }

    test("spending: the footer's spend, rows by outcome, tokens and latency by nearest rank") {
      val used = Usage(Tokens(5), Tokens(2), Tokens.Zero, None)
      val rows = Vector(30, 10, 20, 40).zipWithIndex.map((ms, i) =>
        row(Suite.Triage, id(s"C1/$i"), 0, triage(Vector(1, 0, 0, 0, 0), 0, 0, 0), ms)
          .copy(usage = used)
      ) ++ Vector(
        row(Suite.Triage, id("C1/8"), 0, Outcome.Skipped, 0),
        row(Suite.Triage, id("C1/9"), 0, Outcome.Failed(grit.eval.harness.corpus.Failure.Other), 99)
      )
      // Answered latencies 10, 20, 30, 40: the median's rank ⌈.5·4⌉ = 2, p90's ⌈.9·4⌉ = 4.
      Spending.of(Log(header(), rows, None)) ==>
        Spending(None, 6, Footer(BigDecimal(0), 4, 0, 1, 1), 20, 8, Some(Latency(20, 40, 40)))
    }
  }
}
