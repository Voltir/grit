package grit.core.id

import java.time.Instant

import utest.*

object SettleRefTests extends TestSuite {

  private def period(n: Long): PeriodSeq =
    PeriodSeq.of(n).getOrElse(throw new java.lang.AssertionError(n))

  val tests = Tests {
    test("a question's workflow id is settle:conversation:period:last:activity, and parses back") {
      val activity = Instant.parse("2026-09-20T12:00:00.123456Z")
      val question =
        SettleRef(PeriodRef(ConversationId("0199-abc"), period(2)), TurnSeq(7), activity)
      // A pin of a recorded name: DBOS keeps every settle workflow under this id, and the
      // purge finds a period's questions by its prefix.
      WorkflowId.value(question.workflowId) ==> "settle:0199-abc:2:7:1789905600123"
      SettleRef.fromWorkflowId(question.workflowId) ==> Some(question)
      SettleRef.prefix(PeriodRef(ConversationId("0199-abc"), period(2))) ==> "settle:0199-abc:2:"
    }

    test("an id that is not a settle's parses to None, a close's among them") {
      val notSettles =
        List("0199-abc:7", "settle:c:2:7", "settle:c:0:7:1", "settle:c:2:-1:1", "close:c:2:7:1")
      notSettles.map(s => SettleRef.fromWorkflowId(WorkflowId(s))) ==> notSettles.map(_ => None)
    }
  }
}
