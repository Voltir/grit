package grit.eval.harness.report

import grit.core.triage.Kind
import grit.eval.harness.corpus.Case
import grit.eval.harness.label.{Context, Labelled, Labels}
import grit.eval.harness.log.{Log, Suite}
import grit.eval.harness.score.Fixtures.*

import utest.*

/** The reports on one small run. Every id here is synthetic. */
object ReportTests extends TestSuite {

  private val cases: Vector[Case] = Vector(
    caseOf(1, 1, live = weighed(Kind.Question, 0.9, 0.2, 0.6, 0.4)),
    caseOf(2, 2, live = weighed(Kind.Answer, 0.8, 0.5, 0.3, 0.7))
  )

  private def run(
      name: String,
      durable: Double,
      labels: Labels,
      variant: String = "live"
  ): Scored =
    Scored(
      name,
      Log(
        header(variant),
        cases.map(c =>
          row(Suite.Triage, c.id, 0, triage(Vector(1, 0, 0, 0, 0), 0.5, durable, 0.5))
        ),
        None
      ),
      cases,
      labels
    )

  private def lines(report: String): Vector[String] = report.linesIterator.toVector

  val tests = Tests {
    test("with no case of the corpus labelled, only what needs no label is reported") {
      // A label of a case outside the corpus labels none of it.
      val elsewhere =
        Labels(Map(id("C9/1") -> Labelled.Blank.copy(durable = Some(true))), Set("v1"))
      val report = lines(Report.score(run("a.jsonl", 0.7, elsewhere)))
      assert(report.contains("unlabelled: 0 of 2 cases labelled"))
      report.filter(_.startsWith("## ")) ==>
        Vector("## Spend and latency", "## Repeat spread", "## Against live")
    }

    test("once a case is labelled, its scores follow, all contexts and each alone") {
      val labels = Labels(
        Map(cases(0).id -> Labelled.Blank.copy(durable = Some(true), context = Some(Context.Ok))),
        Set("v1")
      )
      val report = lines(Report.score(run("a.jsonl", 0.7, labels)))
      assert(report.contains("labelled: 1 of 2 cases, guides v1"))
      report.filter(_.startsWith("## ")).drop(3) ==>
        Vector(
          "## Brier score",
          "## Tags, all contexts",
          "## Tags, context ok",
          "## Tags, context short"
        )
    }

    test(
      "a log of live triage's kept tags is said to hold only the likeliest kind's probability, scored or compared"
    ) {
      val note = Report.KeptNote
      val kept = run("live-20261002.jsonl", 0.7, Labels.Empty, grit.eval.harness.pull.Pull.Kept)
      val other = run("b.jsonl", 0.7, Labels.Empty)
      (
        lines(Report.score(kept)).count(_ == note),
        lines(Report.compare(kept, other)).count(_ == note),
        lines(Report.compare(other, other)).count(_ == note),
        lines(Report.score(other)).count(_ == note)
      ) ==> (1, 1, 0, 0)
    }

    test("a comparison lists by id the cases each question's decision moved") {
      // Durable .3 in A, .7 in B, for both cases: decided no, then yes, at .5; unlabelled.
      val report =
        lines(Report.compare(run("a.jsonl", 0.3, Labels.Empty), run("b.jsonl", 0.7, Labels.Empty)))
      val at = report.indexOf("### durable at 0.500 (shipped)")
      report.slice(at + 2, at + 6) ==> Vector(
        "fixed (0): —",
        "broken (0): —",
        "moved (2): C1/1 C1/2",
        "unchanged: 0"
      )
    }
  }
}
