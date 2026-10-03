package grit.core.id

import java.time.Instant

import utest.*

object StitchRefTests extends TestSuite {

  private val turn = TurnRef(ConversationId("0199-abc"), TurnSeq(0))

  val tests = Tests {
    test("a placement's workflow id is stitch:said-at:conversation:turn, and parses back") {
      // Said to the microsecond, as Postgres keeps an entry's time; the placement keeps the
      // millisecond, so the one parsed back from its id is equal to it.
      val stitch = StitchRef(turn, Instant.parse("2026-09-20T12:00:00.123456Z"))
      // A pin of a recorded name: DBOS keeps every placement under this id.
      WorkflowId.value(stitch.workflowId) ==> "stitch:1789905600123:0199-abc:0"
      StitchRef.fromWorkflowId(stitch.workflowId) ==> Some(stitch)
    }

    test("placements' ids sort as their messages were said, across a change in digits") {
      val said =
        Vector(Instant.ofEpochMilli(999_999_999_999L), Instant.ofEpochMilli(1_000_000_000_000L))
      val ids = said.map(at => WorkflowId.value(StitchRef(turn, at).workflowId))
      ids.sorted ==> ids
      ids.headOption ==> Some("stitch:0999999999999:0199-abc:0")
    }

    test("a message said before 1970 is placed as said at its start") {
      StitchRef(turn, Instant.EPOCH.minusSeconds(5)).at ==> Instant.EPOCH
    }

    test("an id that is not a placement's parses to None, a triage's among them") {
      val not = List(
        "0199-abc:0",
        "stitch:1:c",
        "stitch:x:c:0",
        "stitch:-1:c:0",
        "stitch:1:c:-1",
        "stitch:1::0",
        "triage:c:2:7"
      )
      not.map(s => StitchRef.fromWorkflowId(WorkflowId(s))) ==> not.map(_ => None)
    }
  }
}
