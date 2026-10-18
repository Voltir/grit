package grit.eval.harness.report

import grit.eval.harness.score.{RoomPlacing, RoomScore}

/** One run of the stitch lab over a room, as [[Report.lab]] shows it: its `label`, what each
  * opening made, its score, the `calls` it asked (`cached` of them answered from the cache)
  * and what it `spent`.
  */
final case class LabRun(
    label: String,
    placings: Vector[RoomPlacing],
    score: RoomScore,
    calls: Int,
    cached: Int,
    spent: BigDecimal
)
