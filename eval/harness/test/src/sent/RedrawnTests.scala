package grit.eval.harness.sent

import grit.core.id.TurnSeq
import grit.core.message.Tokens
import grit.core.provider.ModelRequest

import utest.*

/** A boundary turn's first call as recorded beside its window drawn afresh ([[Redrawn.table]]):
  * each part, the room left and what summaries would cost, read off a table row.
  */
object RedrawnTests extends TestSuite {

  private def call(task: Long, earlier: Long, digest: Long): FirstCall =
    FirstCall(
      Tokens(16_000),
      Tokens(1_000),
      Tokens(task),
      Tokens(earlier),
      Tokens(digest),
      Tokens(300)
    )

  val tests = Tests {
    test(
      "a redrawn boundary's parts are printed beside the recorded ones, with the room each leaves"
    ) {
      val redrawn = Redrawn(
        turn = TurnSeq(10),
        shown = Vector(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L).map(TurnSeq(_)),
        drawn = Vector(TurnSeq(9)),
        was = call(7_300, 27_000, 400),
        now = call(7_300, 3_150, 420),
        last = Some(Tokens(70_500)),
        limit = Some(Tokens(78_100)),
        summaries = Tokens(2_140),
        reread = 3,
        calls = 8,
        request = ModelRequest("", Vector.empty)
      )
      Redrawn.table(Vector(redrawn)) ==> Vector(
        "| turn | earlier turns shown | first call | task | earlier turns | digest | room | room at limit | summaries appended | calls re-reading a file the digest lists |",
        "|---|---|---|---|---|---|---|---|---|---|",
        "| 10 | 1–9 → 9 | 52.0k → 28.2k | 7.3k → 7.3k | 27.0k → 3.2k | 0.4k → 0.4k | 18.5k → 42.3k | 26.1k → 49.9k | 2.1k | 3 of 8 |"
      )
    }
  }
}
