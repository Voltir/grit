package grit.lifecycle.triage

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.core.id.{ConversationId, EntryId, TriageRef, TurnRef, WorkflowId}
import grit.core.period.{Judgement, PeriodState}
import grit.core.recipe.Records
import grit.core.speech.{Decision, Heard, Ledger, Reach, Silence, Speaking, Speech}
import grit.core.spend.Budget
import grit.core.stitch.{Along, Placed}
import grit.core.store.{Conversation, Entry, PeriodStore, StoreError, Tx}
import grit.core.triage.{Cues, Tags}

/** The triage's `consider` step: whether grit drafts a reply to the heard message it tagged
  * (ADR 0022).
  */
private[triage] object Speak {

  /** What `speaking` decides about the heard message `entry`, `triage`'s, tagged `tags`, at
    * `now`, against the speech ledger and the day's spend under `budget`
    * ([[Speech.decide]]), kept in `records`' speech store with the cues read around it to
    * `now`, its strand read from `horizon` before its thread began ([[Cues]]); a message it
    * answers as said to grit ([[Decision.Answering]]) has its reply awaited and its mark
    * wanted at its reply address in the same transaction, as an edge does for a message said to
    * grit. Under `Off` a message triage did not read as put to grit ([[Tags.directed]]) is held
    * without reading or keeping anything. Why not, when the store fails or the message is gone.
    */
  def consider(
      records: TriageRecords,
      speaking: Speaking,
      budget: Budget,
      horizon: FiniteDuration,
      triage: TriageRef,
      entry: EntryId,
      tags: Tags,
      now: Instant
  )(using Tx^): Either[String, Decision] =
    speaking match {
      case Speaking.Off if !Tags.directed(tags) => Right(Decision.Held(Silence.Off))
      case Speaking.Off =>
        decide(records, speaking, None, budget, horizon, triage, entry, tags, now)
      case Speaking.Shadow(limits) =>
        decide(records, speaking, Some(limits), budget, horizon, triage, entry, tags, now)
      case Speaking.Within(limits) =>
        decide(records, speaking, Some(limits), budget, horizon, triage, entry, tags, now)
    }

  private def decide(
      records: TriageRecords,
      speaking: Speaking,
      limits: Option[grit.core.speech.Limits],
      budget: Budget,
      horizon: FiniteDuration,
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
          val reached = reach.getOrElse(Reach.Nowhere)
          for {
            cues <- cuesOf(records, horizon, e, c, reached, now)
            heard = Heard.of(turn, e, c.origin, reached, tags, cues)
            decision = Speech.decide(speaking, heard, Ledger(spoken, speech, all), budget, now)
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

  /** What code reads around `heard`, of `conversation`, whose reach is `reach`, until `now`
    * ([[Cues]]); its strand read from `horizon` before its thread began.
    */
  private def cuesOf(
      records: TriageRecords,
      horizon: FiniteDuration,
      heard: Entry,
      conversation: Conversation,
      reach: Reach,
      now: Instant
  )(using Tx^): Either[StoreError, Cues] = {
    val own = conversation.id
    for {
      all <- records.entries.list(own)
      read <- Along.read(
        records.stitches,
        records.conversations,
        own,
        all,
        horizon,
        now
      )
      links <- records.stitches.links(Vector(own))
      root = links.find(_.conversation == own).fold(own)(_.root)
      opening = all.minByOption(_.seq).exists(_.id == heard.id)
      later = all.filter(e => e.seq > heard.seq && e.createdAt.isBefore(now))
      // What its room said since, in other conversations: where an opening of its author's may be.
      room <- records.stitches.spokenIn(conversation.origin.room, heard.createdAt, now)
      elsewhere = room.collect {
        case s if s.conversation != own && s.entry.createdAt.isAfter(heard.createdAt) => s.entry
      }
      speakers <- records.principals.speakers((heard +: (later ++ elsewhere)).map(_.id))
      by = speakers.of(heard.id)
      theirs = (e: Entry) => by.isDefined && speakers.of(e.id) == by
      followers <- elsewhere
        .filter(theirs)
        .foldLeft[Either[StoreError, Vector[Entry]]](Right(Vector.empty))((acc, e) =>
          acc.flatMap(done =>
            records.stitches.placed(e.id).map {
              case Some(Placed.Follows(r, _, _, _, _)) if r == root => done :+ e
              case _ => done
            }
          )
        )
      settled <-
        if (!opening) verdictOn(records.periods, heard)
        else if (root != own) rootVerdict(records.periods, root, heard.createdAt)
        else Right(None)
      hits <- Records.near(
        records.documents,
        heard,
        all.filter(_.seq < heard.seq),
        Cues.Searched,
        heard.createdAt
      )
    } yield Cues(
      named = reach.asked.nonEmpty,
      spoken = Speech.spoken(heard, all, read.strand.shown).map(_.by),
      continued = (later.filter(theirs) ++ followers).minByOption(_.createdAt).map(_.id),
      settled = settled,
      records = hits.headOption.map((h, _) => Cues.Match(h.document.plugin, h.score, hits.size))
    )
  }

  /** The latest settle verdict on the open period `heard`, a reply, is in, made about its turns
    * before it; `None` when there is none, or the period is not open.
    */
  private def verdictOn(periods: PeriodStore, heard: Entry)(using
      Tx^
  ): Either[StoreError, Option[Judgement]] =
    periods.of(TurnRef(heard.conversationId, heard.turnSeq)).flatMap {
      case Some(period) if period.state == PeriodState.Open =>
        periods
          .activity(period.ref)
          .map(_.flatMap(_.verdict).filter(_.last < heard.turnSeq).map(_.judgement))
      case _ => Right(None)
    }

  /** The latest settle verdict on `root`'s open period, made before `said`; `None` when there
    * is none, or no period of it is open.
    */
  private def rootVerdict(periods: PeriodStore, root: ConversationId, said: Instant)(using
      Tx^
  ): Either[StoreError, Option[Judgement]] =
    periods
      .all(root)
      .flatMap(
        _.find(_.state == PeriodState.Open).fold(Right(None))(open =>
          periods
            .activity(open.ref)
            .map(_.flatMap(_.verdict).filter(_.at.isBefore(said)).map(_.judgement))
        )
      )

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
