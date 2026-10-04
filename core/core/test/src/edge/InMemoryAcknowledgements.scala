package grit.core.edge

import java.time.Instant

import grit.core.id.{ConversationId, TurnRef, TurnSeq}
import grit.core.store.{StoreError, Tx}

/** In-memory [[Acknowledgements]] for tests, keeping [[AcknowledgementsContract]]. It ignores
  * the `Tx`.
  */
final class InMemoryAcknowledgements extends Acknowledgements {

  private final case class Row(ack: Acknowledgement, cleared: Boolean)

  @caps.unsafe.untrackedCaptures
  private var rows = Vector.empty[Row]

  private def change(turn: TurnRef)(f: Row => Row): Either[StoreError, Unit] = {
    rows = rows.map(r => if (r.ack.turn == turn && !r.cleared) f(r) else r)
    Right(())
  }

  def want(turn: TurnRef, to: String, at: Instant)(using Tx^): Either[StoreError, Unit] = {
    if (!rows.exists(_.ack.turn == turn))
      rows = rows :+ Row(Acknowledgement(turn, to, shown = false), cleared = false)
    Right(())
  }

  def standing()(using Tx^): Either[StoreError, Vector[Acknowledgement]] =
    Right(rows.filterNot(_.cleared).map(_.ack))

  def shown(turn: TurnRef, at: Instant)(using Tx^): Either[StoreError, Unit] =
    change(turn)(r => r.copy(ack = r.ack.copy(shown = true)))

  def cleared(turn: TurnRef, at: Instant)(using Tx^): Either[StoreError, Unit] =
    change(turn)(_.copy(cleared = true))

  /** Deletes the acknowledgements of `conversation`'s turns `first` to `last`: a purge's. */
  def forget(conversation: ConversationId, first: TurnSeq, last: TurnSeq): Unit =
    rows = rows.filterNot { r =>
      val t = r.ack.turn
      t.conversationId == conversation &&
      TurnSeq.value(t.turnSeq) >= TurnSeq.value(first) &&
      TurnSeq.value(t.turnSeq) <= TurnSeq.value(last)
    }
}
