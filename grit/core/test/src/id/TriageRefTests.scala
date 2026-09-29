package grit.core.id

import utest.*

object TriageRefTests extends TestSuite {

  private def period(n: Long): PeriodSeq =
    PeriodSeq.of(n).getOrElse(throw new java.lang.AssertionError(n))

  val tests = Tests {
    test("a triage's workflow id is triage:conversation:period:turn, and parses back") {
      val triage = TriageRef(PeriodRef(ConversationId("0199-abc"), period(2)), TurnSeq(7))
      // A pin of a recorded name: DBOS keeps every triage workflow under this id, and the
      // purge finds a period's triages by its prefix.
      WorkflowId.value(triage.workflowId) ==> "triage:0199-abc:2:7"
      TriageRef.fromWorkflowId(triage.workflowId) ==> Some(triage)
      TriageRef.prefix(PeriodRef(ConversationId("0199-abc"), period(2))) ==> "triage:0199-abc:2:"
    }

    test("an id that is not a triage's parses to None, a settle's among them") {
      val not =
        List(
          "0199-abc:7",
          "triage:c:2",
          "triage:c:0:7",
          "triage:c:2:-1",
          "triage::2:7",
          "settle:c:2:7:1"
        )
      not.map(s => TriageRef.fromWorkflowId(WorkflowId(s))) ==> not.map(_ => None)
    }
  }
}
