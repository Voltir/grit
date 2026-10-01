package grit.core.id

import java.time.Instant

import utest.*

object CloseRefTests extends TestSuite {

  private def period(n: Long): PeriodSeq =
    PeriodSeq.of(n).getOrElse(throw new java.lang.AssertionError(n))

  val tests = Tests {
    test("a close attempt's workflow id is close:conversation:period:last:due, and parses back") {
      // Due to the microsecond, as Postgres keeps an entry's time; the attempt keeps the
      // millisecond, so the one parsed back from its id is equal to it.
      val due = Instant.parse("2026-09-20T12:00:00.123456Z")
      val attempt = CloseRef(PeriodRef(ConversationId("0199-abc"), period(2)), TurnSeq(7), due)
      // A pin of a recorded name: DBOS keeps every close workflow under this id, and the
      // purge finds a period's attempts by its prefix.
      WorkflowId.value(attempt.workflowId) ==> "close:0199-abc:2:7:1789905600123"
      CloseRef.fromWorkflowId(attempt.workflowId) ==> Some(attempt)
    }

    test("a period's attempts share its prefix, which period 10's do not share with period 1's") {
      val c = ConversationId("c")
      val one = CloseRef.prefix(PeriodRef(c, period(1)))
      Vector(1L, 10L)
        .map(n => CloseRef(PeriodRef(c, period(n)), TurnSeq(3), Instant.EPOCH).workflowId)
        .map(id => WorkflowId.value(id).startsWith(one)) ==> Vector(true, false)
    }

    test("an id that is not a close's parses to None, a turn's among them") {
      val notCloses = List(
        "0199-abc:7",
        "close:c:2:7",
        "close:c:0:7:1",
        "close:c:2:-1:1",
        "close::2:7:1",
        "close:c:2:7:x",
        "shut:c:2:7:1"
      )
      notCloses.map(s => CloseRef.fromWorkflowId(WorkflowId(s))) ==> notCloses.map(_ => None)
    }

    test("period numbers start at 1") {
      PeriodSeq.of(0) ==> None
      PeriodSeq.of(1).map(PeriodSeq.value) ==> Some(1L)
    }
  }
}
