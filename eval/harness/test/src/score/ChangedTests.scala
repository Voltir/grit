package grit.eval.harness.score

import grit.core.message.{Tokens, Usage}
import grit.core.store.Focus
import grit.eval.harness.capture.Case
import grit.eval.harness.log.{Outcome, Suite, Weights}
import grit.eval.harness.stats.Estimate

import utest.*
import Fixtures.*

/** Which cases two runs asked differently, how large their calls were, and what Jev's repeat
  * noise alone reads as a difference. Every id here is synthetic.
  */
object ChangedTests extends TestSuite {

  private val answer: Outcome[Vector[Weights]] = triage(Vector(1, 0, 0, 0, 0), 0.5, 0.5, 0.5)
  private val cases: Vector[Case] = Vector.tabulate(4)(n => caseOf(n + 1, n + 1))
  private def at(n: Int) = id(s"C1/$n")

  val tests = Tests {
    test("a case is changed when the runs asked its triage by different requests, and only then") {
      val a = Vector(
        row(Suite.Triage, at(1), 0, answer, request = "same"),
        row(Suite.Triage, at(2), 0, answer, request = "shipped"),
        row(Suite.Triage, at(3), 0, answer, request = "only in A"),
        row(Suite.Triage, at(4), 0, answer, request = "same"),
        row(Suite.Stitch, at(4), 0, stitch(Vector(1, 0)), request = "stitch A")
      )
      val b = Vector(
        row(Suite.Triage, at(1), 0, answer, request = "same"),
        row(Suite.Triage, at(2), 0, answer, request = "with a pool"),
        row(Suite.Triage, at(4), 0, answer, request = "same"),
        row(Suite.Stitch, at(4), 0, stitch(Vector(1, 0)), request = "stitch B")
      )
      Changed.of(cases, a, b).map(_.id) ==> Vector(at(2))
    }

    test("a case's focus is what its triage rows record; none when they record none or disagree") {
      val rows = Vector(
        row(Suite.Triage, at(1), 0, answer, focus = Some(Focus.Open)),
        row(Suite.Triage, at(1), 1, answer, focus = Some(Focus.Open)),
        row(Suite.Triage, at(2), 0, answer, focus = Some(Focus.Focused)),
        row(Suite.Triage, at(3), 0, answer),
        row(Suite.Triage, at(4), 0, answer, focus = Some(Focus.Open)),
        row(Suite.Triage, at(4), 1, answer, focus = Some(Focus.Focused))
      )
      Changed.focus(rows) ==> Map(at(1) -> Focus.Open, at(2) -> Focus.Focused)
    }

    test("size: each answered triage call's input tokens and cost, mean and p90 by nearest rank") {
      def used(tokens: Long, cost: Option[String]) =
        Usage(Tokens(tokens), Tokens(1), Tokens.Zero, cost.map(BigDecimal(_)))
      val rows = Vector.tabulate(10)(i =>
        row(
          Suite.Triage,
          at(i),
          0,
          answer,
          usage = used(100L * (i + 1), Option.when(i < 5)(s"0.0000${i + 1}"))
        )
      ) ++ Vector(
        row(Suite.Triage, at(11), 0, Outcome.Skipped, usage = used(9_999, Some("1"))),
        row(Suite.Stitch, at(12), 0, stitch(Vector(1, 0)), usage = used(9_999, Some("1")))
      )
      // Tokens 100 … 1000: mean 550, p90 the 9th of 10, 900. Costs .00001 … .00005 on five:
      // mean .00003, p90 the 5th of 5.
      Size.of(rows) ==> Size(10, Some(PerCall(550, 900)), Some(PerCall(0.00003, 0.00005)))
      Size.of(Vector.empty) ==> Size(0, None, None)
    }

    test(
      "apart: each case's measure on the second repeat less the first; implied is its MDE / √R"
    ) {
      val rows = Vector(1 -> 0.25, 2 -> 0.5, 3 -> 0.75, 4 -> 0.5).flatMap((n, h) =>
        Vector(
          row(Suite.Triage, at(n), 0, triage(Vector(1, 0, 0, 0, 0), 0.5, 0.5, h)),
          row(Suite.Triage, at(n), 1, triage(Vector(1, 0, 0, 0, 0), 0.5, 0.5, 0.5)),
          row(Suite.Triage, at(n), 2, triage(Vector(1, 0, 0, 0, 0), 0.5, 0.5, 0.0))
        )
      )
      def helps(a: Answers) =
        cases.flatMap(c => a.triage.get(c.id).map(x => c -> Tag.of(Tag.Helps, x.mean)))
      val apart = Apart.of(rows, helps)
      // .5 − .25, .5 − .5, .5 − .75, .5 − .5: the third repeat is never read.
      val expected = Estimate.clustered(Vector(1 -> 0.25, 2 -> 0.0, 3 -> -0.25, 4 -> 0.0))
      (apart.n, apart.exchange) ==> (4, expected)
      expected.map(e => Apart.implied(e, 4)) ==> expected.map(_.mde / 2)
    }
  }
}
