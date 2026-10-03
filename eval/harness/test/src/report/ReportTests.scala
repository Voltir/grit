package grit.eval.harness.report

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.{QuestionName, ShadowName}
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.review.Reason
import grit.core.store.Focus
import grit.core.triage.{Bound, Gate, Kind, Reading}
import grit.eval.harness.corpus.Case
import grit.eval.harness.label.{Context, Labelled, Labels}
import grit.eval.harness.log.{Log, Row, Suite}
import grit.eval.harness.score.Fixtures.*
import grit.eval.harness.score.{Cells, Column, Drafts, Judgement, TurnFixtures}
import grit.lifecycle.triage.TriageQuestions

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
    test("a turns report gives the pass rate first, then each section, then a line a turn") {
      val report = lines(Report.turns("20261004", TurnFixtures.turns, TurnFixtures.verdicts))
      report.filter(_.startsWith("## ")) ==> Vector(
        "## Pass rate",
        "## Not replied",
        "## Rounds",
        "## Tools",
        "## Tokens",
        "## Estimate against the ledger",
        "## Cost per turn",
        "## Prompt cache",
        "## Speech",
        "## Verdicts",
        "## Used parts",
        "## Turns by cost"
      )
      // The replies are in two threads, the heard ones in thread A alone: too few for either.
      report
        .dropWhile(_ != "## Pass rate")
        .takeWhile(_ != "## Not replied")
        .filter(l => l.startsWith("| all ") || l.startsWith("| heard ")) ==>
        Vector(
          "| all | 3 | 1 | 0.333 (1/3 in 2 threads) too few threads for an interval |",
          "| heard | 2 | 1 | 0.500 (1/2 in 1 thread) too few threads for an interval |"
        )
    }

    test("a turns report counts topic and unnamed calls apart from the tools offered") {
      val report = lines(Report.turns("20261004", TurnFixtures.turns, TurnFixtures.verdicts))
      val all = report.dropWhile(_ != "### Tools: all").takeWhile(_ != "### Tools: addressed")
      all.filter(_.startsWith("| ")).drop(1) ==> Vector(
        "| read | 3 | 2 | 1 | 1, 1, 0, 0, 0 |",
        "| search | 2 | 0 | 0 | 0, 0, 0, 0, 0 |",
        "| topic | — | 1 | 1 | 1, 0, 0, 0, 0 |",
        "| unnamed | — | 1 | 1 | 0, 0, 1, 0, 0 |"
      )
    }

    test("a turns report states its used threshold, and recall undefined where nothing replied") {
      val report = lines(Report.turns("20261004", TurnFixtures.turns, TurnFixtures.verdicts))
      assert(report.exists(_.startsWith("A part is used at support 0.3 or over")))
      assert(report.exists(_.contains("A lexical heuristic until labels validate it.")))
      report.filter(l => l.startsWith("| tui |") && l.contains("undefined")) ==>
        Vector("| tui | 0 | — | — | — | undefined: no reply said anything |")
    }

    test("a turns report gives the cache's hit rate of tokens, the first call apart from later") {
      val report = lines(Report.turns("20261004", TurnFixtures.turns, TurnFixtures.verdicts))
      val few = "too few threads for an interval"
      report
        .dropWhile(_ != "## Prompt cache")
        .takeWhile(_ != "### Prompt cache by role")
        .filter(_.startsWith("| all ")) ==> Vector(
        s"| all | 372 | 90 | 0.242 (90/372 in 2 threads) $few | 0.111 (20/180 in 2 threads) $few | " +
          s"0.600 (60/100 in 1 thread) $few |"
      )
    }

    test("a turns report's lines run most costly first, each with its top parts by support") {
      val report = lines(Report.turns("20261004", TurnFixtures.turns, TurnFixtures.verdicts))
      report.dropWhile(_ != "## Turns by cost").drop(6) ==> Vector(
        "| w1 | slack | heard | open | passed | 2 | topic 1, read 2 | 50 | 100 / 80 | 6 mills ($0.006) | — |",
        "| w4 | slack | addressed | open | replied 90 | 0 | — | 79 | 200 / 100 | 4.5 mills ($0.0045) | recent 0.20, recalled 0.00 |",
        "| w2 | slack | heard | open | replied 40 | 1 | unnamed 1 | 60 | 50 / 0 | 2 mills ($0.002) | open 0.50, closed 0.10 |",
        "| w3 | tui | addressed | open | failed at assemble (assembly) | 0 | — | — | — | 0 mills ($0) | — |",
        "| w5 | task | addressed | open | unfinished (PENDING) | 0 | — | — | — | 0 mills ($0) | — |"
      )
    }

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
        "| run | calls answered | input tokens: mean | p90 | cost: mean | p90 |",
        "|---|---|---|---|---|---|",
        "| A | 4 | 700 | 700 | 0.04 mills | 0.04 mills |",
        "| B | 4 | 900 | 900 | 0.04 mills | 0.04 mills |"
      )
    }

    test(
      "a rate prints Wilson's interval by exchange and by author, `[—]` under ten clusters, and no MDE"
    ) {
      // Twelve cases live a question, each its own exchange; the run's likeliest kind agrees on
      // eleven: Wilson's interval on 11 effective cases (Wald's on t reaches 1.100).
      val twelve =
        (1 to 12).toVector.map(n => caseOf(n, n, live = weighed(Kind.Question, 0.9, 0.2, 0.6, 0.4)))
      val agreeing = Scored(
        "a.jsonl",
        Log(
          header(),
          twelve.map(c =>
            row(
              Suite.Triage,
              c.id,
              0,
              triage(
                if (c == twelve(0)) Vector(0, 1, 0, 0, 0) else Vector(1, 0, 0, 0, 0),
                0.5,
                0.5,
                0.5
              )
            )
          ),
          None
        ),
        twelve,
        Labels.Empty
      )
      def agrees(report: String) = lines(report).filter(_.startsWith("| kind: likeliest"))
      agrees(Report.score(agreeing)) ==>
        Vector(
          "| kind: likeliest agrees | 12 | 0.917 [0.632, 0.986] g=12 | [0.632, 0.986] g=12 | — |"
        )
      // Two cases, two exchanges: no interval.
      agrees(Report.score(run("a.jsonl", 0.7, Labels.Empty))) ==>
        Vector("| kind: likeliest agrees | 2 | 0.500 [—] g=2 | [—] g=2 | — |")
      agrees(
        Report.compare(run("a.jsonl", 0.3, Labels.Empty), run("b.jsonl", 0.7, Labels.Empty))
      ) ==>
        Vector("| kind: likeliest the same | 2 | 1.000 [—] g=2 | [—] g=2 | — |")
    }

    test(
      "a gate is written with every one of some parts joined by ∧, any one by ∨, each part joining two or more in parentheses"
    ) {
      def q(n: String) = QuestionName.read(n).fold(sys.error, identity)
      def at(name: String, p: Double) =
        Gate.Holds(Bound.AtLeast(Reading.Yes(q(name)), Probability.clamped(p)))
      val below = Gate.Holds(Bound.Below(Reading.Key(q("gap"), "asks"), Probability.clamped(0.5)))
      Vector(
        Report.gate(Gate.all(at("a", 0.5), Gate.either(at("b", 0.2), at("c", 0.2)), below)),
        Report.gate(Gate.either(Gate.all(at("a", 0.5), at("b", 0.5)), Gate.Open)),
        Report.gate(Gate.Open)
      ) ==> Vector(
        "a ≥ 0.500 ∧ (b ≥ 0.200 ∨ c ≥ 0.200) ∧ gap.asks < 0.500",
        "(a ≥ 0.500 ∧ b ≥ 0.500) ∨ everything",
        "everything"
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

    test(
      "a drafts report names each side's log, set and gate, counts each cell by focus and over all, names live's draft apart from its speech decision, and lists no case"
    ) {
      def q(n: String) = QuestionName.read(n).fold(sys.error, identity)
      val (c1, c2, c3) = (id("C1/1"), id("C1/2"), id("C1/3"))
      val found = Drafts(
        Map(
          Some(Focus.Open) -> Cells(Vector(c1), Vector.empty, Vector(c2), Vector.empty),
          None -> Cells(Vector.empty, Vector(c3), Vector.empty, Vector.empty)
        ),
        2,
        Vector(Column("gap.asks", 0.5, 0.25, 3)),
        Some(Cells(Vector(c1), Vector(c2), Vector.empty, Vector(c3)))
      )
      val setLog = Log(
        header("shadow-v2").copy(questions = Some(Vector(q("gap"), q("open")))),
        Vector.empty[Row[VectorMap[QuestionName, Answer]]],
        None
      )
      val liveLog = Log(
        header("kept").copy(questions = Some(Vector(q("kind"), q("helps")))),
        Vector.empty[Row[VectorMap[QuestionName, Answer]]],
        None
      )
      val report = Report.drafts(
        Report.Side("live-20261003.jsonl", liveLog, "v1", TriageQuestions.V1.speak),
        Report.Side("shadow-v2.jsonl", setLog, "v2", TriageQuestions.V2.speak),
        found,
        None
      )
      lines(report).filter(l =>
        l.startsWith("|") || l.startsWith("A, ") || l.startsWith("B") || l.startsWith(
          "answered"
        ) || l.startsWith("agree")
      ) ==> Vector(
        "A, live: live-20261003.jsonl, variant kept, model m, set v1, questions kind, helps",
        "B: shadow-v2.jsonl, variant shadow-v2, model m, set v2, questions gap, open",
        "answered by both: 5; undecided: 2 (a question a gate reads unanswered)",
        "A, live's draft by v1's gate: kind=chatter < 0.500 ∧ helps ≥ 0.500. Triage's answers alone: not live's speech decision, which also checks the address, freshness, who was asked, the thread and the rate limits.",
        "B, v2's draft: gap.asks ≥ 0.500 ∧ open ≥ 0.500 ∧ to < 0.500 ∧ anchor < 0.500.",
        "| focus | both | live only | v2 only | neither | agree |",
        "|---|---|---|---|---|---|",
        "| open | 1 | 0 | 1 | 0 | 1 of 2 |",
        "| focus unknown | 0 | 1 | 0 | 0 | 0 of 1 |",
        "| all | 1 | 1 | 1 | 0 | 1 of 3 |",
        "| question | mean | sd | cases |",
        "|---|---|---|---|",
        "| gap.asks | 0.500 | 0.250 | 3 |",
        "A, live's durable ≥ 0.500; B, v2's durable ≥ 0.500 (both at Earning.DurableAt), over the cases both answered.",
        "| | v2 yes | v2 no |",
        "|---|---|---|",
        "| live yes | 1 | 1 |",
        "| live no | 0 | 1 |",
        "agree: 2 of 3"
      )
      assert(!Vector(c1, c2, c3).exists(c => report.contains(c.written)))
    }
    test(
      "a drafts report counts each reason's verdicts against both drafts and each side's to, and says plainly when there are none"
    ) {
      val v1 = ShadowName.of("triage-v1").fold(sys.error, identity)
      val setLog = Log(
        header("shadow-triage-v1"),
        Vector.empty[Row[VectorMap[QuestionName, Answer]]],
        None
      )
      val liveLog = Log(header("kept"), Vector.empty[Row[VectorMap[QuestionName, Answer]]], None)
      def report(verdicts: Option[Judgement]) = lines(
        Report.drafts(
          Report.Side("live.jsonl", liveLog, "v2", TriageQuestions.V2.speak),
          Report.Side("shadow-triage-v1.jsonl", setLog, "v1", TriageQuestions.V1.speak),
          Drafts(Map.empty, 0, Vector.empty, None),
          verdicts
        )
      ).dropWhile(_ != "## Against verdicts")
      val judged = Judgement(
        Vector(
          Judgement.Of(v1, Reason.ShadowOnly, 3, 1, 2, Some(Judgement.Matched(3, 3)), None),
          Judgement.Of(v1, Reason.Neither, 2, 2, 2, Some(Judgement.Matched(1, 2)), None)
        ),
        1
      )
      (
        report(Some(judged)).filter(l => l.startsWith("|") || l.contains("left out")),
        report(None).drop(2).take(1),
        report(Some(Judgement(Vector.empty, 0))).drop(2).take(1)
      ) ==> (
        Vector(
          "| shadow | picked | verdicts | live's draft = speak | v1's draft = speak | live's to ≥ 0.500 = to a person | v1's to ≥ 0.500 = to a person |",
          "|---|---|---|---|---|---|---|",
          "| triage-v1 | shadow-only | 3 | 1 | 2 | 3 of 3 | — |",
          "| triage-v1 | neither | 2 | 2 | 2 | 1 of 2 | — |",
          "verdicts on a case either gate could not decide, left out: 1"
        ),
        Vector("No verdicts given (`--verdicts`, a file `pull` writes)."),
        Vector("No verdicts: none standing in the file given.")
      )
    }
  }
}
