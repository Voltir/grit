package grit.core.store

import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.{Tokens, Usage}

/** An in-memory [[UsageLedger]] for tests, keeping [[StoreContract]]. It ignores the `Tx`. */
final class InMemoryUsageLedger extends UsageLedger {

  @caps.unsafe.untrackedCaptures
  var rows = Vector.empty[(EntryId, WorkflowId, String, Usage, Tokens, TurnRef)]

  def record(
      entry: EntryId,
      turn: TurnRef,
      workflow: WorkflowId,
      model: String,
      usage: Usage,
      estimatedInput: Tokens
  )(using Tx^): Either[StoreError, Unit] =
    if (rows.exists(_._1 == entry)) Left(StoreError.DuplicateId(entry))
    else {
      rows = rows :+ (entry, workflow, model, usage, estimatedInput, turn)
      Right(())
    }

  def of(workflow: WorkflowId)(using Tx^): Either[StoreError, Vector[UsageLedger.Row]] =
    Right(rows.collect {
      case (entry, w, model, usage, estimate, _) if w == workflow =>
        UsageLedger.Row(entry, model, usage, estimate)
    })

  def forget(conversation: ConversationId, from: TurnSeq, to: TurnSeq)(using
      Tx^
  ): Either[StoreError, Unit] = {
    rows = rows.filterNot { row =>
      val t = row._6
      t.conversationId == conversation &&
      TurnSeq.value(t.turnSeq) >= TurnSeq.value(from) && TurnSeq.value(t.turnSeq) <= TurnSeq.value(
        to
      )
    }
    Right(())
  }
}
