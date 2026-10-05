package grit.core.review

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.{ConversationId, EntryId, PrincipalId, QuestionName, ShadowName, TurnRef}
import grit.core.inbox.InMemoryInbox
import grit.core.speech.{Decision, InMemorySpeechStore}
import grit.core.store.{ConversationStore, Entry, EntryStore, Payload, StoreError, Tx}
import grit.core.triage.{
  InMemoryTriageShadows,
  InMemoryTriageStore,
  ShadowAnswers,
  Shadowed,
  TriageShadows
}

/** An in-memory [[ReviewStore]] for tests, keeping [[ReviewContract]], over the stores it is
  * given: a candidate is a heard message in `entries`, decided on in `speech`, answered in
  * `shadows`; a review is kept while `conversations` holds its conversation (the SQL row
  * cascades from it). It ignores the `Tx`.
  */
final class InMemoryReviews(
    entries: EntryStore,
    conversations: ConversationStore,
    speech: InMemorySpeechStore,
    shadows: TriageShadows
) extends ReviewStore {
  import InMemoryReviews.Row

  @caps.unsafe.untrackedCaptures
  private var rows = Map.empty[EntryId, Row]

  def unposted()(using Tx^): Either[StoreError, Vector[Prompt]] =
    kept.flatMap { all =>
      all
        .collect { case (entry, row @ Row(_, _, _, Considered.Picked(reason), None, _)) =>
          (entry, row, reason)
        }
        .sortBy((entry, row, _) => (row.at, EntryId.value(entry)))
        .foldLeft[Either[StoreError, Vector[Prompt]]](Right(Vector.empty)) {
          case (acc, (entry, row, reason)) =>
            for {
              done <- acc
              heard <- entries.get(entry)
              conversation <- conversations.get(row.conversation)
              answers <- heard.fold(Right(None))(answersOf(_, row.shadow))
            } yield (for {
              h <- heard
              c <- conversation
              live <- settled(h)
              a <- answers
            } yield Prompt(entry, c.origin, row.shadow, reason, live, a)).fold(done)(done :+ _)
        }
    }

  def posted(entry: EntryId, address: String, at: Instant)(using
      Tx^
  ): Either[StoreError, Boolean] =
    kept.map { all =>
      val taken = all.exists((_, r) => r.posted.exists(_._1 == address))
      all.find(_._1 == entry) match {
        case Some((_, row @ Row(_, _, _, Considered.Picked(_), None, _))) if !taken =>
          rows = rows.updated(entry, row.copy(posted = Some((address, at))))
          true
        case _ => false
      }
    }

  def reacted(address: String, rater: PrincipalId, verdict: Verdict, at: Instant)(using
      Tx^
  ): Either[StoreError, Boolean] =
    postedAt(address).map {
      case Some((entry, row)) =>
        rows = rows.updated(entry, row.copy(label = Some(Label(verdict, rater, at))))
        true
      case None => false
    }

  def unreacted(address: String, rater: PrincipalId, verdict: Verdict)(using
      Tx^
  ): Either[StoreError, Boolean] =
    postedAt(address).map {
      case Some((entry, row)) if row.label.exists(l => l.verdict == verdict && l.rater == rater) =>
        rows = rows.updated(entry, row.copy(label = None))
        true
      case _ => false
    }

  def candidates(shadow: ShadowName, since: Instant, limit: Int)(using
      Tx^
  ): Either[StoreError, Vector[Candidate]] =
    speech.decisions
      .foldLeft[Either[StoreError, Vector[Candidate]]](Right(Vector.empty)) {
        case (acc, (heard, _, _)) =>
          for {
            done <- acc
            first <- entries.ofTurn(heard.turn).map(_.minByOption(_.seq))
            answers <- first.fold(Right(None))(answersOf(_, shadow))
          } yield (for {
            e <- first.filter(e => isHeard(e) && !e.createdAt.isBefore(since))
            if !rows.contains(e.id)
            live <- settled(e)
            a <- answers
          } yield Candidate(e.id, e.conversationId, e.createdAt, live, shadow, a)).fold(done)(
            done :+ _
          )
      }
      .map(_.sortBy(c => (c.said, EntryId.value(c.entry))).take(limit))

  def considered(candidate: Candidate, as: Considered, at: Instant)(using
      Tx^
  ): Either[StoreError, Boolean] =
    if (rows.contains(candidate.entry)) Right(false)
    else {
      rows = rows.updated(
        candidate.entry,
        Row(candidate.conversation, candidate.shadow, at, as, None, None)
      )
      Right(true)
    }

  def reviewed(since: Instant)(using Tx^): Either[StoreError, Vector[Reviewed]] =
    kept.map(
      _.filter((_, r) => !r.at.isBefore(since))
        .sortBy((entry, r) => (r.at, EntryId.value(entry)))
        .map((entry, r) => Reviewed(entry, r.conversation, r.shadow, r.at, r.as, r.label))
    )

  /** The rows whose conversation is still kept. */
  private def kept(using Tx^): Either[StoreError, Vector[(EntryId, Row)]] =
    rows.toVector.foldLeft[Either[StoreError, Vector[(EntryId, Row)]]](Right(Vector.empty)) {
      (acc, kv) =>
        acc.flatMap(done =>
          conversations.get(kv._2.conversation).map(c => c.fold(done)(_ => done :+ kv))
        )
    }

  /** The kept row whose prompt was posted at `address`. */
  private def postedAt(address: String)(using Tx^): Either[StoreError, Option[(EntryId, Row)]] =
    kept.map(_.find((_, r) => r.posted.exists(_._1 == address)))

  private def isHeard(e: Entry): Boolean = e.payload match {
    case Payload.Heard(_) => true
    case _ => false
  }

  /** Live's settled decision on the heard message `e`; `None` while undecided or drafting. */
  private def settled(e: Entry): Option[Settled] = {
    val turn = TurnRef(e.conversationId, e.turnSeq)
    speech.decisions.find(_._1.turn == turn).flatMap {
      case (_, Decision.Held(why), _) => Some(Settled.Held(why))
      case (_, Decision.Drafting(t), _) => speech.outcomes.get(t).map(o => Settled.Drafted(o._1))
      case (_, Decision.Answering(t, _), _) =>
        speech.outcomes.get(t).map(o => Settled.Drafted(o._1))
    }
  }

  /** What `shadow` answered of `e` as a set; `None` when it did not, or failed. */
  private def answersOf(e: Entry, shadow: ShadowName)(using
      Tx^
  ): Either[StoreError, Option[VectorMap[QuestionName, Answer]]] =
    shadows
      .of(shadow, Vector(e.id))
      .map(_.get(e.id).collect { case Shadowed.Answered(_, ShadowAnswers.Named(a), _, _, _, _) =>
        a
      })
}

object InMemoryReviews {

  /** Reviews over `inbox`'s entries, conversations and speech, the shadows' answers kept in
    * `shadows`.
    */
  def over(inbox: InMemoryInbox, shadows: TriageShadows): InMemoryReviews =
    new InMemoryReviews(inbox.entries, inbox.conversations, inbox.speech, shadows)

  /** Reviews over `inbox`'s stores, with shadows' answers of their own, which none records. */
  def over(inbox: InMemoryInbox): InMemoryReviews =
    over(
      inbox,
      new InMemoryTriageShadows(
        inbox.entries,
        new InMemoryTriageStore(inbox.entries, inbox.periods)
      )
    )

  /** A considered message's review: its prompt's address and when it was posted, and its
    * label.
    */
  private final case class Row(
      conversation: ConversationId,
      shadow: ShadowName,
      at: Instant,
      as: Considered,
      posted: Option[(String, Instant)],
      label: Option[Label]
  )
}
