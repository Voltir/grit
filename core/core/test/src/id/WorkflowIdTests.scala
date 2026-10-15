package grit.core.id

import utest.*

object WorkflowIdTests extends TestSuite {

  val tests = Tests {
    test("as reads an id as its kind, and says which kind an id is not, naming the id") {
      val turn = TurnRef(ConversationId("c"), TurnSeq(3))
      val read = WorkflowId.as("turn", TurnRef.fromWorkflowId)
      (read(turn.workflowId), read(WorkflowId("proof-1"))) ==>
        (Right(turn), Left("not a turn: proof-1"))
    }
  }
}
