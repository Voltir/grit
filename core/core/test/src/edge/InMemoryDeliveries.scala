package grit.core.edge

import java.time.Instant

import grit.core.id.{ConversationId, TurnRef, TurnSeq, WorkflowId}
import grit.core.store.{EntryStore, InMemoryEntryStore, InMemoryPrincipals, StoreError, Tx}

/** In-memory [[Deliveries]] for tests, keeping [[DeliveriesContract]]. It ignores the `Tx`,
  * but for a turn's asks, read from `entries` as its clearance reads them, the first's account
  * from `principals`, at [[now]].
  */
final class InMemoryDeliveries(
    entries: EntryStore = new InMemoryEntryStore(),
    principals: InMemoryPrincipals = new InMemoryPrincipals
) extends Deliveries {

  /** When a turn's asks are read: an ask whose `until` is not after it is closed. */
  @caps.unsafe.untrackedCaptures
  var now: Instant = Instant.EPOCH

  private final case class Row(pending: Pending, delivered: Boolean)

  // Replaced whole by the store's own methods, which the test calls on its one thread.
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

  def asks(turn: TurnRef)(using Tx^): Either[StoreError, Vector[OpenAsk]] =
    entries
      .ofTurn(turn)
      .map(kept =>
        OpenAsk.of(kept, kept.headOption.flatMap(first => principals.author(first.id)), now)
      )

  /** Deletes the deliveries of `conversation`'s turns `first` to `last`: a purge's. */
  def forget(conversation: ConversationId, first: TurnSeq, last: TurnSeq): Unit =
    rows = rows.filterNot { r =>
      val t = r.pending.turn
      t.conversationId == conversation &&
      TurnSeq.value(t.turnSeq) >= TurnSeq.value(first) &&
      TurnSeq.value(t.turnSeq) <= TurnSeq.value(last)
    }
}
