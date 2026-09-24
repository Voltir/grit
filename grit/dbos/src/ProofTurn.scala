package grit.dbos

import grit.core.{Durable, Entry, EntryStore, Message, Origin, Payload, TurnRef, WorkflowId}

/** A stand-in for the durable turn, registered as `turn` until `grit.turn` has one: one
  * transaction step that reads the message that started it and reports what it saw.
  */
object ProofTurn {

  /** Every proof run shares one conversation. */
  val ProofOrigin: Origin = Origin.Task("proof", "main")

  def body(entries: EntryStore)(workflowId: WorkflowId)(using d: Durable^): String = {
    val id = WorkflowId.value(workflowId)
    TurnRef.fromWorkflowId(workflowId) match {
      case None => s"not a turn id: $id"
      case Some(turn) =>
        // Failure is reported as a value: a thrown step is rethrown on every replay.
        d.transact("read-message") {
          println(s"[turn] $id started")
          // Long enough to see that one conversation's turns do not overlap.
          Thread.sleep(500)
          val seen = entries
            .list(turn.conversationId)
            .map(_.collectFirst {
              case Entry(_, _, turn.turnSeq, _, _, Payload.Message(Message.User(text)), _) => text
            })
          println(s"[turn] $id finished")
          seen match {
            case Right(Some(text)) => s"saw: $text"
            case Right(None) => "saw no message"
            case Left(error) => s"failed: $error"
          }
        }
    }
  }
}
