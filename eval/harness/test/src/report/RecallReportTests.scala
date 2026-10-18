package grit.eval.harness.report

import grit.core.id.{DocumentVersion, WorkflowId}
import grit.core.place.Service
import grit.eval.harness.reply.{Refound, KeptAt}

import utest.*

/** A recall replay as its report prints it: ids, versions and numbers alone. */
object RecallReportTests extends TestSuite {
  import Refound.{Followed, Found, Rank}

  private def version(n: Long): DocumentVersion =
    DocumentVersion.of(n).getOrElse(throw new java.lang.AssertionError("version"))
  private val github =
    KeptAt.Service(
      Service.of("github").fold(why => throw new java.lang.AssertionError(why), identity)
    )

  // w1 followed version 1, ranked 4th by its own search and 3rd now, and version 9, forgotten;
  // w2 followed version 3, which its own search never ranked, missed now; w4 versions 4 and 5,
  // now at 7 and 20; w5 versions 6 and 7, now at 50 and missed; w3 was not replayed. Of the six
  // kept, each cut holds one more than the last, and the turns' own searches ranked five.
  private val report = Report.recall(
    "20261018",
    Vector(
      WorkflowId("w1:0") -> Right(
        Vector(
          Followed(version(1), Found.Kept(github, Some(4), Rank.At(3))),
          Followed(version(9), Found.Forgotten)
        )
      ),
      WorkflowId("w2:0") -> Right(
        Vector(Followed(version(3), Found.Kept(github, None, Rank.Missed)))
      ),
      WorkflowId("w4:0") -> Right(
        Vector(
          Followed(version(4), Found.Kept(github, Some(1), Rank.At(7))),
          Followed(version(5), Found.Kept(github, Some(6), Rank.At(20)))
        )
      ),
      WorkflowId("w5:0") -> Right(
        Vector(
          Followed(version(6), Found.Kept(github, Some(9), Rank.At(50))),
          Followed(version(7), Found.Kept(github, Some(12), Rank.Missed))
        )
      ),
      WorkflowId("w3:0") -> Left("the search of w3:0 does not read: no results")
    )
  )

  val tests = Tests {
    test(
      "a recall report gives each cut's share by source as a point estimate under ten turns, a line a turn, and the turns not replayed"
    ) {
      val lines = report.linesIterator.toVector
      assert(lines.contains("turns replayed: 4; not replayed: 1"))
      def of(n: Int) = f"${n / 6.0}%.3f ($n/6 in 4 turns) point estimate"
      assert(lines.contains("| source | ≤ 5 | ≤ 10 | ≤ 30 | ranked | own search |"))
      assert(lines.contains(s"| github | ${of(1)} | ${of(2)} | ${of(3)} | ${of(4)} | ${of(5)} |"))
      assert(lines.contains("gone: 0; forgotten: 1"))
      assert(lines.contains("| w1:0 | 1 github at 4 → at 3, 9 forgotten |"))
      assert(lines.contains("| w2:0 | 3 github unranked → missed |"))
      assert(lines.contains("- w3:0: the search of w3:0 does not read: no results"))
    }
  }
}
