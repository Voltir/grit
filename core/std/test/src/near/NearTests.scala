package grit.std.near

import utest.*

object NearTests extends TestSuite {
  val tests = Tests {
    test(
      "nearest puts strings extending the asked one first, then by edit distance, at most the most asked, none past a third of its length"
    ) {
      val among = Vector(
        "plan-c5-runs-turns",
        "plan-c5b",
        "plan-c4",
        "plan-c7",
        "plan-c5",
        "note-c5",
        "unrelated"
      )
      // plan-c5b extends it by one, plan-c5-runs-turns by eleven; plan-c4 and plan-c7 are one
      // edit away and tie, in string order, so the cut at three leaves plan-c7.
      val nearest = Near.nearest("plan-c5", among, 3)
      // plan-c47 is two edits from plan-c5, within 7 / 3 = 2; note-c5 is four.
      val within = Near.nearest("plan-c5", Vector("note-c5", "plan-c47"), 3)
      (nearest, within, Near.nearest("z", Vector("a", "b"), 3)) ==> (
        Vector("plan-c5b", "plan-c5-runs-turns", "plan-c4"),
        Vector("plan-c47"),
        Vector.empty
      )
    }
  }
}
