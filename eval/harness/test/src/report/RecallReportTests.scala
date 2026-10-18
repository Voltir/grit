package grit.eval.harness.report

import grit.core.id.{DocumentVersion, WorkflowId}
import grit.core.place.Service
import grit.eval.harness.reply.{Refound, Source}

import utest.*

/** A recall replay as its report prints it: ids, versions and numbers alone. */
object RecallReportTests extends TestSuite {
  import Refound.{Followed, Found, Rank}

  private def version(n: Long): DocumentVersion =
    DocumentVersion.of(n).getOrElse(throw new java.lang.AssertionError("version"))
  private val github =
    Source.Service(
      Service.of("github").fold(why => throw new java.lang.AssertionError(why), identity)
    )

  // w1 followed version 1, ranked 4th by its own search and 2nd now, and version 9, forgotten;
  // w2 followed version 3, which its own search never ranked, missed now; w3 was not replayed.
  private val report = Report.recall(
    "20261018",
    Vector(
      WorkflowId("w1:0") -> Right(
        Vector(
          Followed(version(1), Found.Kept(github, Some(4), Rank.At(2))),
          Followed(version(9), Found.Forgotten)
        )
      ),
      WorkflowId("w2:0") -> Right(
        Vector(Followed(version(3), Found.Kept(github, None, Rank.Missed)))
      ),
      WorkflowId("w3:0") -> Left("the search of w3:0 does not read: no results")
    )
  )

  val tests = Tests {
    test(
      "a recall report gives each cut's share by source as a point estimate under ten turns, a line a turn, and the turns not replayed"
    ) {
      val lines = report.linesIterator.toVector
      assert(lines.contains("turns replayed: 2; not replayed: 1"))
      // github: version 1 at 2 and version 3 missed, over two turns; one of them ranked by its
      // own search.
      val half = "0.500 (1/2 in 2 turns) point estimate"
      assert(lines.contains(s"| github | $half | $half | $half | $half | $half |"))
      assert(lines.contains("gone: 0; forgotten: 1"))
      assert(lines.contains("| w1:0 | 1 github at 4 → at 2, 9 forgotten |"))
      assert(lines.contains("| w2:0 | 3 github unranked → missed |"))
      assert(lines.contains("- w3:0: the search of w3:0 does not read: no results"))
    }
  }
}
