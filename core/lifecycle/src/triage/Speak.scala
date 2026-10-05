package grit.lifecycle.triage

import java.time.Instant

import grit.core.id.{EntryId, TriageRef, TurnRef, WorkflowId}
import grit.core.speech.{Decision, Heard, Ledger, Reach, Silence, Speaking, Speech}
import grit.core.spend.Budget
import grit.core.store.{StoreError, Tx}
import grit.core.triage.Tags

/** The triage's `consider` step: whether grit drafts a reply to the heard message it tagged
  * (ADR 0022).
  */
private[triage] object Speak {

  /** What `speaking` decides about the heard message `entry`, `triage`'s, tagged `tags`, at
    * `now`, against the speech ledger and the day's spend under `budget`
    * ([[Speech.decide]]), kept in `records`' speech store; a message it answers as said to
    * grit ([[Decision.Answering]]) has its reply awaited and its mark wanted at its reply
    * address in the same transaction, as an edge does for a message said to grit. Under `Off`
    * a message triage did not read as put to grit ([[Tags.directed]]) is held without reading
    * or keeping anything. Why not, when the store fails or the message is gone.
    */
  def consider(
      records: TriageRecords,
      speaking: Speaking,
      budget: Budget,
      triage: TriageRef,
      entry: EntryId,
      tags: Tags,
      now: Instant
  )(using Tx^): Either[String, Decision] =
    speaking match {
      case Speaking.Off if !Tags.directed(tags) => Right(Decision.Held(Silence.Off))
      case Speaking.Off => decide(records, speaking, None, budget, triage, entry, tags, now)
      case Speaking.Shadow(limits) =>
        decide(records, speaking, Some(limits), budget, triage, entry, tags, now)
      case Speaking.Within(limits) =>
        decide(records, speaking, Some(limits), budget, triage, entry, tags, now)
    }

  private def decide(
      records: TriageRecords,
      speaking: Speaking,
      limits: Option[grit.core.speech.Limits],
      budget: Budget,
      triage: TriageRef,
      entry: EntryId,
      tags: Tags,
      now: Instant
  )(using Tx^): Either[String, Decision] = {
    val turn = TurnRef(triage.period.conversationId, triage.turn)
    val day = budget.today(now)
    (for {
      found <- records.entries.get(entry)
      conversation <- records.conversations.get(turn.conversationId)
      reach <- records.speech.reach(turn)
      spoken <- limits.fold[Either[StoreError, Vector[grit.core.speech.Spoken]]](
        Right(Vector.empty)
      )(l => records.speech.spoken(l.from(now)))
      speech <- records.speech.spentOn(day)
      all <- records.spending.on(day)
      decided <- (found, conversation) match {
        case (Some(e), Some(c)) =>
          val heard =
            Heard(turn, e.seq, c.origin.room, e.createdAt, reach.getOrElse(Reach.Nowhere), tags)
          val decision = Speech.decide(speaking, heard, Ledger(spoken, speech, all), budget, now)
          for {
            kept <- records.speech.decided(heard, decision, now)
            _ <- decision match {
              case Decision.Answering(_, to) if kept =>
                records.deliveries
                  .await(turn, to)
                  .flatMap(_ => records.acknowledgements.want(turn, to, now))
              case _ => Right(())
            }
          } yield Some(decision)
        case _ => Right(None)
      }
    } yield decided).left
      .map(describe)
      .flatMap(
        _.toRight(s"${WorkflowId.value(turn.workflowId)}: the message or its conversation is gone")
      )
  }

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
