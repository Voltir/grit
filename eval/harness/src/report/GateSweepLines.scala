package grit.eval.harness.report

import java.time.Instant

import grit.eval.harness.score.GateSweep

/** [[Report.bounds]]'s lines. */
private[report] object GateSweepLines {

  def of(
      capture: String,
      since: Instant,
      cases: Vector[GateSweep.Case],
      verdicts: Int,
      points: Vector[GateSweep.Point]
  ): String =
    (Vector(
      s"# bounds: $capture",
      "",
      s"cases tagged since $since: ${cases.size}, in ${cases.map(_.heard.room).distinct.size} " +
        s"rooms; verdicts standing on them: $verdicts",
      "",
      "Each case decided by the shipped speech decision as it was said, over an empty ledger " +
        "and no cap: no rate, cap or staleness holds one. A case decided before cues reads " +
        "every fact 0.",
      "",
      "| point | drafted | welcome among drafted | interruptions drafted | welcome held |",
      "|---|---|---|---|---|"
    ) ++ points.map(p =>
      s"| ${p.name} | ${RecipeLines.rate(p.drafted, "room")} | " +
        s"${RecipeLines.rate(p.welcome, "room")} | ${p.interrupting} | ${p.welcomeHeld} |"
    )).mkString("", "\n", "\n")
}
