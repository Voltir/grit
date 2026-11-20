package grit.core.speech

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.Cost
import grit.core.place.Place
import grit.core.spend.{Day, Spend}
import grit.core.store.{
  ConversationStore,
  Entry,
  EntryStore,
  InMemoryConversationStore,
  InMemoryUsageLedger,
  Origin,
  Payload,
  StoreError,
  Tx
}

/** An in-memory [[SpeechStore]] for tests, keeping [[SpeechContract]], over the entries and
  * the usage ledger it is given: a posted reply's position is read from `entries`, and a
  * day's speech spend from `ledger`'s rows recorded that day for turns decided on.
  */
final class InMemorySpeechStore(
    entries: EntryStore,
    ledger: InMemoryUsageLedger,
    conversations: ConversationStore = new InMemoryConversationStore()
) extends SpeechStore {

  /** Each turn's reach, with its heard entry, which it goes with (the SQL row cascades). */
  // Replaced whole by the store's own methods, which the test calls on its one thread.
  @caps.unsafe.untrackedCaptures
  private var reaches = Map.empty[TurnRef, (EntryId, Reach)]

  /** Each decision, in the order it was kept, with when, and its outcome once settled. */
  @caps.unsafe.untrackedCaptures
  var decisions = Vector.empty[(Heard, Decision, Instant)]

  /** Each occasion's decision, in the order it was kept: its turn, room, decision and when. */
  @caps.unsafe.untrackedCaptures
  var occasions = Vector.empty[(TurnRef, Place, Decision, Instant)]

  /** Every decision kept, a heard message's or an occasion's, and whether it is an occasion's. */
  private def kept: Vector[(TurnRef, Place, Decision, Instant, Boolean)] =
    decisions.map((h, d, at) => (h.turn, h.room, d, at, false)) ++
      occasions.map((t, room, d, at) => (t, room, d, at, true))

  /** Each settled turn's outcome, its draft's excerpt and its reply's position. */
  // As `reaches`: replaced whole by the store's own methods, on the test's one thread.
  @caps.unsafe.untrackedCaptures
  var outcomes = Map.empty[TurnRef, (Outcome, Option[String], Option[EntrySeq])]

  def heard(turn: TurnRef, reach: Reach)(using Tx^): Either[StoreError, Unit] =
    entries.list(turn.conversationId).flatMap { all =>
      all.filter(_.turnSeq == turn.turnSeq).minByOption(_.seq) match {
        case Some(first @ Entry(_, _, _, _, _, Payload.Heard(_), _, _)) =>
          if (!reaches.contains(turn)) reaches = reaches.updated(turn, (first.id, reach))
          Right(())
        case _ => Left(StoreError.Invalid(s"${turn.workflowId} is not a heard message's turn"))
      }
    }

  def reach(turn: TurnRef)(using Tx^): Either[StoreError, Option[Reach]] =
    reaches.get(turn) match {
      case None => Right(None)
      case Some((heard, kept)) => entries.get(heard).map(_.map(_ => kept))
    }

  def addressIn(room: Place, since: Instant)(using
      Tx^
  ): Either[StoreError, Option[(Origin, String)]] =
    reaches.toVector
      .foldLeft[Either[StoreError, Vector[(Entry, Origin, String)]]](Right(Vector.empty)) {
        case (found, (turn, (heard, reach))) =>
          found.flatMap(sofar =>
            reach.replyTo.flatMap(_.room).fold(Right(sofar)) { to =>
              for {
                entry <- entries.get(heard)
                conversation <- conversations.get(turn.conversationId)
              } yield (for {
                e <- entry.filter(!_.createdAt.isBefore(since))
                c <- conversation.filter(_.origin.room == room)
              } yield (e, c.origin, to)).fold(sofar)(sofar :+ _)
            }
          )
      }
      .map(
        _.maxByOption((e, _, _) => (e.createdAt, EntrySeq.value(e.seq)))
          .flatMap((_, origin, to) => origin.topLevel.map((_, to)))
      )

  def spoken(since: Instant)(using Tx^): Either[StoreError, Vector[Spoken]] =
    Right(
      kept
        .collect {
          case (_, room, Decision.Drafting(turn, venue), at, occasion) if !at.isBefore(since) =>
            val stage = outcomes.get(turn) match {
              case None => Stage.Drafting
              case Some((_, _, Some(seq))) => Stage.Posted(seq)
              case Some((_, _, None)) => Stage.Settled
            }
            val thread = reaches.get(turn).flatMap(_._2.replyTo).flatMap(_.within)
            Spoken(turn, room, at, stage, venue, thread, occasion)
        }
        .sortBy(s => (s.at, WorkflowId.value(s.turn.workflowId)))
    )

  def spentOn(day: Day)(using Tx^): Either[StoreError, Spend] = {
    val drafting = kept.collect { case (_, _, Decision.Drafting(t, _), _, _) => t.workflowId }.toSet
    Right(
      ledger.rows
        .filter((entry, workflow, _, _, _, _) =>
          drafting.contains(workflow) &&
            ledger.recordedOn(entry).exists(at => !at.isBefore(day.from) && at.isBefore(day.until))
        )
        .foldLeft(Spend.Zero)((s, row) => s + Spend(1, Cost.of(row._4)))
    )
  }

  def decided(heard: Heard, decision: Decision, at: Instant)(using
      Tx^
  ): Either[StoreError, Boolean] =
    if (decisions.exists(_._1.turn == heard.turn)) Right(false)
    else {
      decisions = decisions :+ ((heard, decision, at))
      Right(true)
    }

  def occasioned(turn: TurnRef, room: Place, decision: Decision, at: Instant)(using
      Tx^
  ): Either[StoreError, Boolean] = decision match {
    case Decision.Answering(_, _) => Left(StoreError.Invalid("an occasion is never answered"))
    case Decision.Drafting(_, _) | Decision.Held(_) =>
      if (kept.exists(_._1 == turn)) Right(false)
      else {
        occasions = occasions :+ ((turn, room, decision, at))
        Right(true)
      }
  }

  def answering(turn: TurnRef)(using Tx^): Either[StoreError, Boolean] =
    Right(decisions.exists {
      case (_, Decision.Answering(t, _), _) => t == turn
      case _ => false
    })

  def venue(turn: TurnRef)(using Tx^): Either[StoreError, Venue] =
    Right(
      kept
        .collectFirst { case (_, _, Decision.Drafting(t, v), _, _) if t == turn => v }
        .getOrElse(Venue.Thread)
    )

  def toThread(turn: TurnRef)(using Tx^): Either[StoreError, Unit] = {
    decisions = decisions.map {
      case (h, Decision.Drafting(t, _), d) if t == turn =>
        (h, Decision.Drafting(t, Venue.Thread), d)
      case d => d
    }
    occasions = occasions.map {
      case (t, room, Decision.Drafting(_, _), at) if t == turn =>
        (t, room, Decision.Drafting(t, Venue.Thread), at)
      case o => o
    }
    Right(())
  }

  /** Nothing to hold: a test's transactions never run together. */
  def lockRoom(turn: TurnRef)(using Tx^): Either[StoreError, Unit] = Right(())

  def drafted(turn: TurnRef, outcome: Outcome, draft: Option[String], at: Instant)(using
      Tx^
  ): Either[StoreError, Boolean] =
    if (!kept.exists(d => started(d._3).contains(turn)))
      Left(StoreError.Invalid(s"${turn.workflowId} was never decided on"))
    else if (outcomes.contains(turn)) Right(false)
    else {
      val posted: Either[StoreError, Option[EntrySeq]] = outcome match {
        case Outcome.Posted(_) =>
          entries
            .get(turn.replyId)
            .flatMap(_.map(_.seq).toRight(StoreError.Invalid(s"${turn.workflowId} has no reply")))
            .map(Some(_))
        case _ => Right(None)
      }
      posted.map { seq =>
        outcomes = outcomes.updated(turn, (outcome, draft.map(_.take(SpeechStore.Excerpt)), seq))
        true
      }
    }

  def forget(conversation: ConversationId, from: TurnSeq, to: TurnSeq)(using
      Tx^
  ): Either[StoreError, Unit] = {
    def on(turn: TurnRef): Boolean =
      turn.conversationId == conversation &&
        TurnSeq.value(turn.turnSeq) >= TurnSeq.value(from) &&
        TurnSeq.value(turn.turnSeq) <= TurnSeq.value(to)
    decisions = decisions.filterNot(d => on(d._1.turn))
    occasions = occasions.filterNot(o => on(o._1))
    outcomes = outcomes.filterNot((t, _) => on(t))
    Right(())
  }

  /** The turn `d` started, drafting or answering. */
  private def started(d: Decision): Option[TurnRef] = d match {
    case Decision.Drafting(t, _) => Some(t)
    case Decision.Answering(t, _) => Some(t)
    case Decision.Held(_) => None
  }
}
