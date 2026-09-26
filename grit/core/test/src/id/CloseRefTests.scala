package grit.core.id

import utest.*

object CloseRefTests extends TestSuite {

  private def period(n: Long): PeriodSeq =
    PeriodSeq.of(n).getOrElse(throw new java.lang.AssertionError(n))

  val tests = Tests {
    test("a close attempt's workflow id is close:conversation:period:last, and parses back") {
      val attempt = CloseRef(PeriodRef(ConversationId("0199-abc"), period(2)), TurnSeq(7))
      // A pin of a recorded name: DBOS keeps every close workflow under this id, and the
      // purge derives the ids to delete from it.
      WorkflowId.value(attempt.workflowId) ==> "close:0199-abc:2:7"
      CloseRef.fromWorkflowId(attempt.workflowId) ==> Some(attempt)
    }

    test("an id that is not a close's parses to None, a turn's among them") {
      val notCloses =
        List("0199-abc:7", "close:c:2", "close:c:0:7", "close:c:2:-1", "close::2:7", "shut:c:2:7")
      notCloses.map(s => CloseRef.fromWorkflowId(WorkflowId(s))) ==> notCloses.map(_ => None)
    }

    test("period numbers start at 1") {
      PeriodSeq.of(0) ==> None
      PeriodSeq.of(1).map(PeriodSeq.value) ==> Some(1L)
    }
  }
}
