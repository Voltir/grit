package grit.dbos.engine

import java.time.Instant

import scala.util.Using

import grit.core.id.{EntryId, SourceId, TriageRef, TurnRef}
import grit.core.identity.Account
import grit.core.inbox.{InboundId, InboxError}
import grit.core.message.Message
import grit.core.speech.{Reach, SpeechStore}
import grit.core.spend.{Budget, Spending}
import grit.core.stitch.Opening
import grit.core.store.{
  ConversationStore,
  Entry,
  EntryStore,
  Origin,
  Payload,
  PeriodStore,
  StoreError,
  Tx
}
import grit.core.visibility.{Integrity, Label}
import grit.dbos.sql.{SqlIdentities, SqlRooms, SqlStatements}

/** How the inbox records what an edge hands it, each in the transaction open: what it writes,
  * and what that starts, never started here. A message is refused once `spending` says the
  * day's spend has reached `budget`'s cap, where the inbox caps it.
  */
private[engine] final class Recorder(
    conversations: ConversationStore,
    entries: EntryStore,
    periods: PeriodStore,
    speech: SpeechStore,
    spending: Spending,
    budget: Budget
) {

  /** `text` recorded as heard in `origin`'s room at `at`, `reach` where grit could answer it,
    * and what that starts: a message recorded before as heard is heard again (its triage
    * starts again, which runs once), one recorded before as said to grit is not.
    */
  def heard(
      origin: Origin,
      source: SourceId,
      text: String,
      by: Account,
      at: Instant,
      reach: Reach
  )(using Tx^): Either[StoreError, Starts.Heard] =
    roomOf(origin)
      .flatMap(label =>
        recorded(origin, source, Payload.Heard(text), by, label, None, at, capped = false)
      )
      .flatMap {
        case Left(_) => Right(Starts.Heard(None, None))
        case Right(turn) =>
          for {
            entry <- entries.get(InboundId.of(turn.conversationId, source))
            period <- periods.of(turn)
            triage <- entry.map(_.payload) match {
              // A message recorded as a turn before is not heard, and not triaged.
              case Some(Payload.Heard(_)) =>
                speech
                  .heard(turn, Reach.heardIn(origin.room, reach))
                  .map(_ => period.map(p => TriageRef(p.ref, turn.turnSeq)))
              case _ => Right(None)
            }
            opening <- openingOf(turn)
          } yield Starts.Heard(opening, triage)
      }

  /** `message` recorded as said to grit at `at`, and what that starts; refused over the day's
    * cap when it is new, or as [[recorded]] refuses it.
    */
  def said(
      origin: Origin,
      source: SourceId,
      message: Message.User,
      by: Account,
      at: Instant
  )(using Tx^): Either[StoreError, Either[InboxError, Starts.Said]] =
    conversations
      .find(origin)
      .flatMap {
        // No conversation yet: the message is new, and is refused over the cap before its
        // conversation is created.
        case None =>
          overCapAt(at).flatMap {
            case Some(refused) => Right(Left(refused))
            case None =>
              roomOf(origin).flatMap(label =>
                recorded(
                  origin,
                  source,
                  Payload.Message(message),
                  by,
                  label,
                  None,
                  at,
                  capped = true
                )
              )
          }
        case Some(_) =>
          roomOf(origin).flatMap(label =>
            recorded(
              origin,
              source,
              Payload.Message(message),
              by,
              label,
              None,
              at,
              capped = true
            )
          )
      }
      .flatMap {
        case Left(refused) => Right(Left(refused))
        case Right(turn) => openingOf(turn).map(opening => Right(Starts.Said(opening, turn)))
      }

  /** `turn`'s message as an [[Opening]] of its conversation, read in the transaction open;
    * `None` when it is not one.
    */
  def openingOf(turn: TurnRef)(using Tx^): Either[StoreError, Option[Opening]] =
    for {
      conversation <- conversations.get(turn.conversationId)
      all <- entries.list(turn.conversationId)
    } yield conversation.flatMap(Opening.of(_, all, turn))

  /** Why the day's spend at `now` refuses a new message; `None` when it does not. */
  def overCapAt(now: Instant)(using Tx^): Either[StoreError, Option[InboxError.OverCap]] =
    budget.cap match {
      case None => Right(None)
      case Some(cap) =>
        val day = budget.today(now)
        spending
          .on(day)
          .map(spent => Option.when(!budget.admits(spent))(InboxError.OverCap(spent, cap, day)))
    }

  /** The label a conversation an edge's message begins from `origin` is created at. */
  def roomOf(origin: Origin)(using Tx^): Either[StoreError, Label] =
    SqlRooms.label(origin)

  /** `payload` recorded as the first entry of a new turn, in the transaction open, dated `at`
    * as the period it opens is, its conversation created at `label` if it is new, believed at
    * `believed`, else at what `by` is believed at ([[integrityOf]]): its existing turn when `source`
    * was recorded before, else, when `capped`, refused over the cap of the day `at` falls on,
    * else a new turn.
    */
  def recorded(
      origin: Origin,
      source: SourceId,
      payload: Payload,
      by: Account,
      label: Label,
      believed: Option[Integrity],
      at: Instant,
      capped: Boolean
  )(using Tx^): Either[StoreError, Either[InboxError, TurnRef]] =
    for {
      conversation <- conversations.findOrCreate(origin, by, label)
      id = InboundId.of(conversation.id, source)
      // Serialises ingest per conversation: a concurrent ingest waits here, then sees
      // this one's entry and its sequence numbers.
      next <- entries.lockNext(conversation.id)
      existing <- entries.get(id)
      refused <- existing.fold(
        sealedOf(origin, conversation.label).flatMap(
          _.fold[Either[StoreError, Option[InboxError]]](
            if (capped) overCapAt(at) else Right(None)
          )(why => Right(Some(why)))
        )
      )(_ => Right(None))
      turn <- (existing, refused) match {
        case (Some(entry), _) => Right(Right(TurnRef(entry.conversationId, entry.turnSeq)))
        case (None, Some(why)) => Right(Left(why))
        case (None, None) =>
          (for {
            _ <- periods.openFor(conversation.id, next.turnSeq, at)
            integrity <- believed.fold(integrityOf(by))(Right(_))
          } yield integrity).flatMap { integrity =>
            entries
              .insert(
                Entry(
                  id,
                  conversation.id,
                  next.turnSeq,
                  None,
                  next.seq,
                  payload,
                  at,
                  integrity
                )
              )
              .flatMap(_ => Recorder.authored(id, by))
              .map(_ => Right(TurnRef(conversation.id, next.turnSeq)))
          }
      }
    } yield turn

  /** Why a new message in `origin`'s conversation, created at `label`, is refused as
    * [[InboxError.Sealed]]: a direct message's thread whose label its person's clearance now
    * does not dominate; `None` for any other.
    */
  private def sealedOf(origin: Origin, label: Label)(using
      Tx^
  ): Either[StoreError, Option[InboxError]] =
    origin match {
      case d @ Origin.Direct(_, _) =>
        SqlRooms
          .label(d)
          .map(now => Option.when(!now.dominates(label))(InboxError.Sealed(d)))
      case Origin.Tui(_, _) | Origin.Slack(_, _, _) | Origin.Task(_, _) => Right(None)
    }

  /** What `by`'s words are believed at as they enter grit, before their room's cap
    * ([[Tx.integrityOf]] of whom it is linked to now); an account never seen is
    * [[Integrity.Outside]], anyone else's.
    */
  private def integrityOf(by: Account)(using Tx^): Either[StoreError, Integrity] =
    SqlIdentities.person(by).map(_.fold(Integrity.Outside)(Tx.integrityOf(_)))
}

private object Recorder {

  /** Records that the inbound entry `id` was written through `by`, kept as a new person's one
    * account when not seen before.
    */
  private def authored(id: EntryId, by: Account)(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    SqlIdentities.enroll(Set(by)).flatMap { _ =>
      SqlStatements.attempt {
        Using.resource(
          conn.prepareStatement("INSERT INTO grit.inbound (entry_id, account) VALUES (?, ?)")
        ) { ps =>
          ps.setString(1, EntryId.value(id))
          ps.setString(2, SqlIdentities.written(by))
          ps.executeUpdate()
          ()
        }
      }
    }
  }
}
