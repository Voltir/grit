package grit.core.speech

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.Cost
import grit.core.spend.{Day, Spend}
import grit.core.store.{Entry, EntryStore, InMemoryUsageLedger, Payload, StoreError, Tx}

/** An in-memory [[SpeechStore]] for tests, keeping [[SpeechContract]], over the entries and
  * the usage ledger it is given: a posted reply's position is read from `entries`, and a
  * day's speech spend from `ledger`'s rows recorded that day for turns decided on.
  */
final class InMemorySpeechStore(entries: EntryStore, ledger: InMemoryUsageLedger)
    extends SpeechStore {

  /** Each turn's reach, with its heard entry, which it goes with (the SQL row cascades). */
  @caps.unsafe.untrackedCaptures
  private var reaches = Map.empty[TurnRef, (EntryId, Reach)]

  /** Each decision, in the order it was kept, with when, and its outcome once settled. */
  @caps.unsafe.untrackedCaptures
  var decisions = Vector.empty[(Heard, Decision, Instant)]

  @caps.unsafe.untrackedCaptures
  var outcomes = Map.empty[TurnRef, (Outcome, Option[String], Option[Long])]

  def heard(turn: TurnRef, reach: Reach)(using Tx^): Either[StoreError, Unit] =
    entries.list(turn.conversationId).flatMap { all =>
      all.filter(_.turnSeq == turn.turnSeq).minByOption(_.seq) match {
        case Some(first @ Entry(_, _, _, _, _, Payload.Heard(_), _)) =>
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

  def spoken(since: Instant)(using Tx^): Either[StoreError, Vector[Spoken]] =
    Right(
      decisions
        .collect {
          case (h, Decision.Drafting(turn), at) if !at.isBefore(since) =>
            val stage = outcomes.get(turn) match {
              case None => Stage.Drafting
              case Some((_, _, Some(seq))) => Stage.Posted(seq)
              case Some((_, _, None)) => Stage.Settled
            }
            Spoken(turn, h.room, at, stage)
        }
        .sortBy(s => (s.at, WorkflowId.value(s.turn.workflowId)))
    )

  def spentOn(day: Day)(using Tx^): Either[StoreError, Spend] = {
    val drafting = decisions.collect { case (_, Decision.Drafting(t), _) => t.workflowId }.toSet
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

  def drafted(turn: TurnRef, outcome: Outcome, draft: Option[String], at: Instant)(using
      Tx^
  ): Either[StoreError, Boolean] =
    if (!decisions.exists(d => d._2 == Decision.Drafting(turn)))
      Left(StoreError.Invalid(s"${turn.workflowId} was never decided on"))
    else if (outcomes.contains(turn)) Right(false)
    else {
      val posted: Either[StoreError, Option[Long]] = outcome match {
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
    outcomes = outcomes.filterNot((t, _) => on(t))
    Right(())
  }
}
