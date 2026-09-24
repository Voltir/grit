package grit.core

import utest.*

object TurnRefTests extends TestSuite {

  val tests = Tests {
    test("a turn's workflow id is conversation:seq, and parses back") {
      val turn = TurnRef(ConversationId("0199-abc"), TurnSeq(3))
      WorkflowId.value(turn.workflowId) ==> "0199-abc:3"
      TurnRef.fromWorkflowId(turn.workflowId) ==> Some(turn)
    }

    test("an id that is not a turn's parses to None") {
      val notTurns = List("proof-1", "c:", ":3", "c:x", "c:-1", "c:1:2", "")
      notTurns.map(s => TurnRef.fromWorkflowId(WorkflowId(s))) ==> notTurns.map(_ => None)
    }
  }
}
