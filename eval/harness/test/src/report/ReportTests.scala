package grit.eval.harness.report

import grit.core.message.{Tokens, Usage}
import grit.core.store.Focus
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

    test(
      "a comparison under a replica leaves out the cases that moved on, and lists them with each question's tolerance"
    ) {
      val (a, b) = (run("a.jsonl", 0.7, Labels.Empty), run("b.jsonl", 0.2, Labels.Empty))
      val noise = grit.eval.harness.score.Noise(0.02, 0.01, 0.01, 0.01)
      val first = cases.headOption.map(_.id).toVector
      val report =
        lines(Report.compare(a, b, movedOn = Some(grit.eval.harness.score.Moved(noise, first))))
      (
        report.filter(_.startsWith("answered by both")),
        report.dropWhile(_ != "## Moved on").slice(1, 6),
        lines(Report.compare(a, b)).count(_ == "## Moved on")
      ) ==> (
        Vector("answered by both: triage 1, stitch 0"),
        Vector(
          "",
          "A replica's answer more than 10 × Jev's repeat spread from live's: kind 0.200, waiting 0.100, durable 0.100, helps 0.100.",
          "",
          "moved on, left out of this comparison: 1",
          s"- ${first.map(_.written).mkString}"
        ),
        0
      )
    }

    test(
      "a comparison counts the cases whose triage inputs changed by context and focus, and sizes each run's calls"
    ) {
      // Case 1's request differs, said at its open focus and labelled short; case 2's does not.
      def used(tokens: Long) =
        Usage(Tokens(tokens), Tokens(1), Tokens.Zero, Some(BigDecimal("0.00004")))
      def changedRun(name: String, first: String, tokens: Long) = Scored(
        name,
        Log(
          header(repeats = 2),
          Vector(0, 1).flatMap(r =>
            Vector(
              row(
                Suite.Triage,
                cases(0).id,
                r,
                triage(Vector(1, 0, 0, 0, 0), 0.5, 0.5, 0.5),
                request = first,
                usage = used(tokens),
                focus = Some(Focus.Open)
              ),
              row(
                Suite.Triage,
                cases(1).id,
                r,
                triage(Vector(1, 0, 0, 0, 0), 0.5, 0.5, 0.5),
                usage = used(tokens),
                focus = Some(Focus.Focused)
              )
            )
          ),
          None
        ),
        cases,
        Labels(Map(cases(0).id -> Labelled.Blank.copy(context = Some(Context.Short))), Set("v1"))
      )
      val report =
        lines(Report.compare(changedRun("a.jsonl", "r", 700), changedRun("b.jsonl", "x", 900)))
      val at = report.indexOf("## Inputs changed")
      report.slice(at + 2, at + 10) ==> Vector(
        "triage asked differently: 1 of the 2 cases both runs asked",
        "",
        "| context | focused | open | focus unknown | all |",
        "|---|---|---|---|---|",
        "| ok | 0 | 0 | 0 | 0 |",
        "| short | 0 | 1 | 0 | 1 |",
        "| no context | 0 | 0 | 0 | 0 |",
        "| all | 0 | 1 | 0 | 1 |"
      )
      val size = report.indexOf("### Size of triage's calls")
      report.slice(size + 2, size + 6) ==> Vector(
        "| run | calls answered | input tokens: mean | p90 | cost, USD: mean | p90 |",
        "|---|---|---|---|---|---|",
        "| A | 4 | 700 | 700 | 0.0000400 | 0.0000400 |",
        "| B | 4 | 900 | 900 | 0.0000400 | 0.0000400 |"
      )
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
