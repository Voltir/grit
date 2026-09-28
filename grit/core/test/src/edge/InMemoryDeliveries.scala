package grit.core.edge

import grit.core.id.{ConversationId, TurnRef, TurnSeq, WorkflowId}
import grit.core.store.{StoreError, Tx}

/** In-memory [[Deliveries]] for tests, keeping [[DeliveriesContract]]. It ignores the `Tx`. */
final class InMemoryDeliveries extends Deliveries {

  private final case class Row(pending: Pending, delivered: Boolean)

  @caps.unsafe.untrackedCaptures
  private var rows = Vector.empty[Row]

  private def missing(turn: TurnRef): Either[StoreError, Unit] =
    Left(StoreError.Invalid(s"no delivery awaited for ${WorkflowId.value(turn.workflowId)}"))

  private def change(turn: TurnRef)(f: Row => Row): Either[StoreError, Unit] =
    if (!rows.exists(_.pending.turn == turn)) missing(turn)
    else {
      rows = rows.map(r => if (r.pending.turn == turn) f(r) else r)
      Right(())
    }

  private def part(turn: TurnRef, n: Int, p: Part): Either[StoreError, Unit] =
    change(turn)(r => r.copy(pending = r.pending.copy(parts = r.pending.parts.updated(n, p))))

  def await(turn: TurnRef, to: String)(using Tx^): Either[StoreError, Unit] = {
    if (!rows.exists(_.pending.turn == turn))
      rows = rows :+ Row(Pending(turn, to, Map.empty), false)
    Right(())
  }

  def pending()(using Tx^): Either[StoreError, Vector[Pending]] =
    Right(rows.filterNot(_.delivered).map(_.pending))

  def posting(turn: TurnRef, n: Int)(using Tx^): Either[StoreError, Unit] =
    part(turn, n, Part.Posting)

  def posted(turn: TurnRef, n: Int, id: String)(using Tx^): Either[StoreError, Unit] =
    part(turn, n, Part.Posted(id))

  def delivered(turn: TurnRef)(using Tx^): Either[StoreError, Unit] =
    change(turn)(_.copy(delivered = true))

  /** Deletes the deliveries of `conversation`'s turns `first` to `last`: a purge's. */
  def forget(conversation: ConversationId, first: TurnSeq, last: TurnSeq): Unit =
    rows = rows.filterNot { r =>
      val t = r.pending.turn
      t.conversationId == conversation &&
      TurnSeq.value(t.turnSeq) >= TurnSeq.value(first) &&
      TurnSeq.value(t.turnSeq) <= TurnSeq.value(last)
    }
}
