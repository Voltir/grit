package grit.dbos

import dev.dbos.transact.DBOS
import grit.core.{
  ConversationStore,
  Entry,
  EntryId,
  EntryStore,
  Message,
  Origin,
  Payload,
  StoreError,
  TurnSeq
}

/** The phase-0 proof workflow: one `txStep` that finds or creates the proof
  * conversation and inserts an entry keyed by the workflow id into it.
  *
  * A named class, not a lambda, because DBOS records each workflow under its
  * registered name and class name and refuses to resume or replay an id under
  * a different pair. A lambda's class name changes between builds; this one
  * does not, and it is also what `DBOSClient` names when it enqueues.
  */
final class ProofWorkflow(
    store: Store,
    conversations: ConversationStore,
    entries: EntryStore
) {

  /** The workflow body; DBOS calls it reflectively with no arguments. Its only
    * input is the workflow id, read from DBOS's context.
    */
  def run(): String = {
    val wfId = Option(DBOS.workflowId()).getOrElse("none")
    // The step reports failure as a value rather than throwing: DBOS retries a
    // step that throws.
    store.transact("insert-entry") { tx ?=>
      println(s"[step] insert-entry executing for $wfId")
      val entryId = EntryId("proof-" + wfId)
      val outcome = for {
        conversation <- conversations.findOrCreate(ProofWorkflow.ProofOrigin)
        _ <- entries.insert(
          Entry(
            id = entryId,
            conversationId = conversation.id,
            turnSeq = TurnSeq.First,
            parentId = None,
            seq = 0L,
            payload = Payload.Message(Message.User("txStep proof")),
            createdAt = java.time.Instant.now()
          )
        )
      } yield ()
      val id = EntryId.value(entryId)
      Outcome.render(outcome match {
        case Right(_) => Outcome.Inserted(id)
        case Left(StoreError.DuplicateId(_)) => Outcome.AlreadyPresent(id)
        case Left(StoreError.DatabaseError(cause)) => Outcome.Failed(cause)
      })
    }
  }
}

object ProofWorkflow {

  /** The workflow name DBOS records; stable across builds, like the class. */
  val Name = "proofWorkflow"

  /** Every proof run shares one conversation, so a second run exercises the
    * "find" half of find-or-create.
    */
  val ProofOrigin: Origin = Origin.Task("proof", "main")
}

/** What the insert step reports back to the workflow.
  *
  * A step output crosses DBOS's Jackson boundary and is replayed from a row,
  * so it travels as a string; that encoding is confined here and callers match
  * on the enum.
  */
private[dbos] enum Outcome {
  case Inserted(id: String)
  case AlreadyPresent(id: String)
  case Failed(detail: String)
}

private[dbos] object Outcome {
  def render(o: Outcome): String = o match {
    case Inserted(id) => s"inserted:$id"
    case AlreadyPresent(id) => s"already-present:$id"
    case Failed(detail) => s"failed:$detail"
  }

  def parse(s: String): Option[Outcome] = s.split(":", 2) match {
    case Array("inserted", id) => Some(Inserted(id))
    case Array("already-present", id) => Some(AlreadyPresent(id))
    case Array("failed", detail) => Some(Failed(detail))
    case _ => None
  }
}
