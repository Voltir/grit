package grit.core.store

import java.time.Instant

import grit.core.id.{CloseRef, ConversationId, EntryId, PeriodRef, PeriodSeq, TurnRef, TurnSeq}
import grit.core.period.{
  Activity,
  CloseOrdinal,
  CloseReason,
  Closing,
  LifecycleSettings,
  Period,
  PeriodState,
  Verdict
}

/** An in-memory [[PeriodStore]] for tests, keeping [[PeriodContract]], over the entries of
  * `entries`; `origin` is each conversation's origin, which Postgres keeps in its own row.
  * It ignores the `Tx`: nothing is rolled back, and nothing is locked.
  */
final class InMemoryPeriodStore(
    entries: InMemoryEntryStore,
    origin: ConversationId -> Origin = c => Origin.Task("conversation", ConversationId.value(c)),
    edges: grit.core.edge.InMemoryEdges = new grit.core.edge.InMemoryEdges,
    deliveries: grit.core.edge.InMemoryDeliveries = new grit.core.edge.InMemoryDeliveries
) extends PeriodStore {

  // Only ever replaced by a new immutable vector, as the store's table would be.
  @caps.unsafe.untrackedCaptures
  private var periods = Vector.empty[Period]

  private def mine(c: ConversationId): Vector[Period] =
    periods.filter(_.ref.conversationId == c).sortBy(p => PeriodSeq.value(p.ref.seq))

  // Only ever replaced by the next ordinal, as the store's sequence would be.
  @caps.unsafe.untrackedCaptures
  private var lastOrdinal = CloseOrdinal.Start

  private def replace(p: Period): Unit =
    periods = periods.map(q => if (q.ref == p.ref) p else q)

  private def isOpen(p: Period): Boolean = p.state match {
    case PeriodState.Open => true
    case PeriodState.Closed(_, _, _, _, _, _) => false
  }

  private def closedOf(p: Period): Option[PeriodState.Closed] = p.state match {
    case c: PeriodState.Closed => Some(c)
    case PeriodState.Open => None
  }

  private def entriesOf(c: ConversationId)(using Tx^): Vector[Entry] =
    entries.list(c).getOrElse(Vector.empty)

  def openFor(conversation: ConversationId, turn: TurnSeq, at: Instant)(using
      Tx^
  ): Either[StoreError, Period] = {
    val had = mine(conversation)
    had.find(isOpen) match {
      case Some(p) => Right(p)
      case None =>
        val seq = had.lastOption.fold(PeriodSeq.First)(_.ref.seq.next)
        val p = Period(PeriodRef(conversation, seq), turn, at, PeriodState.Open)
        periods = periods :+ p
        Right(p)
    }
  }

  def get(period: PeriodRef)(using Tx^): Either[StoreError, Option[Period]] =
    Right(periods.find(_.ref == period))

  def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Period]] =
    Right(
      mine(turn.conversationId)
        .filter(p => TurnSeq.value(p.first) <= TurnSeq.value(turn.turnSeq))
        .lastOption
        .filter(p => closedOf(p).forall(c => TurnSeq.value(turn.turnSeq) <= TurnSeq.value(c.last)))
    )

  // Only ever replaced by a new immutable vector, as the store's table would be.
  @caps.unsafe.untrackedCaptures
  private var verdicts = Vector.empty[(PeriodRef, Verdict)]

  def judged(period: PeriodRef, verdict: Verdict)(using Tx^): Either[StoreError, Boolean] =
    periods.find(p => p.ref == period && isOpen(p)).map(activityOf) match {
      case Some(a) if a.last == verdict.last =>
        verdicts = verdicts :+ (period -> verdict)
        Right(true)
      case _ => Right(false)
    }

  private def activityOf(p: Period)(using Tx^): Activity = {
    val own =
      entriesOf(p.ref.conversationId).filter(e =>
        TurnSeq.value(e.turnSeq) >= TurnSeq.value(p.first)
      )
    // A draft is not activity (Payload.Draft).
    val active = own.filter(e =>
      e.payload match {
        case Payload.Draft(_) => false
        case _ => true
      }
    )
    val newest = (active.map(_.createdAt) :+ p.openedAt).maxBy(_.toEpochMilli)
    val last = own.map(_.turnSeq).maxByOption(TurnSeq.value).getOrElse(p.first)
    val judged = verdicts.collect { case (ref, v) if ref == p.ref => v }
    // As the SQL store orders them: by time to the microsecond, then the one judged last.
    val latest = judged.zipWithIndex
      .maxByOption((v, i) => (v.at.getEpochSecond, v.at.getNano / 1000, i))
      .map(_._1)
    Activity(p.ref, newest, last, latest, judged.size)
  }

  def open()(using Tx^): Either[StoreError, Vector[Activity]] =
    Right(periods.filter(isOpen).map(activityOf))

  def activity(period: PeriodRef)(using Tx^): Either[StoreError, Option[Activity]] =
    Right(periods.find(p => p.ref == period && isOpen(p)).map(activityOf))

  def seal(attempt: CloseRef, reason: CloseReason, closing: Closing, at: Instant)(using
      Tx^
  ): Either[StoreError, Sealed] =
    periods.find(p => p.ref == attempt.period && isOpen(p)) match {
      case None => Right(Sealed.Abandoned)
      case Some(p) =>
        entries.lockNext(p.ref.conversationId).flatMap { next =>
          if (next.turnSeq != attempt.last.next) Right(Sealed.Abandoned)
          else {
            val id = p.ref.closingId
            entries
              .insert(
                Entry(
                  id,
                  p.ref.conversationId,
                  attempt.last,
                  None,
                  next.seq,
                  Payload.Closed(p.ref.seq, reason, closing),
                  at
                )
              )
              .map { _ =>
                // Never taken again, as the SQL store's sequence: a dropped period's stays used.
                val o = lastOrdinal.next
                lastOrdinal = o
                replace(p.copy(state = PeriodState.Closed(attempt.last, at, reason, id, o, None)))
                Sealed.Closed(id)
              }
          }
        }
    }

  def closingBefore(turn: TurnRef)(using Tx^): Either[StoreError, Option[ClosingEntry]] = {
    val id = mine(turn.conversationId)
      .flatMap(p =>
        closedOf(p).filter(c => TurnSeq.value(c.last) < TurnSeq.value(turn.turnSeq)).map(_.closing)
      )
      .lastOption
    id.flatMap(id => entriesOf(turn.conversationId).find(_.id == id)) match {
      case None => Right(None)
      case Some(e) =>
        ClosingEntry
          .of(e)
          .map(Some(_))
          .toRight(StoreError.Invalid(s"${EntryId.value(e.id)} is not a closing entry"))
    }
  }

  def openElsewhere(conversation: ConversationId)(using
      Tx^
  ): Either[StoreError, Vector[OpenPeriod]] =
    Right(
      periods
        .filter(p => isOpen(p) && p.ref.conversationId != conversation)
        .sortBy(p => (p.openedAt.toEpochMilli, ConversationId.value(p.ref.conversationId)))
        .map(p => OpenPeriod(p.ref.conversationId, origin(p.ref.conversationId).place, p.first))
    )

  def closedElsewhere(conversation: ConversationId)(using
      Tx^
  ): Either[StoreError, Vector[ClosedElsewhere]] =
    Right(
      periods
        .filter(_.ref.conversationId != conversation)
        .flatMap(p => closedOf(p).filter(_.reason != CloseReason.Unearned).map(p -> _))
        .groupBy(_._1.ref.conversationId)
        .values
        .flatMap(_.maxByOption((p, _) => PeriodSeq.value(p.ref.seq)))
        .toVector
        .sortBy((_, c) => CloseOrdinal.value(c.order))
        .map((p, c) =>
          ClosedElsewhere(p.ref.conversationId, origin(p.ref.conversationId).place, c.closing)
        )
    )

  def closedAfter(after: CloseOrdinal, n: Int)(using
      Tx^
  ): Either[StoreError, Vector[ClosedPeriod]] =
    Right(
      periods
        .flatMap(p => closedOf(p).map(p -> _))
        .filter(_._2.order.isAfter(after))
        .sortBy(pc => CloseOrdinal.value(pc._2.order))
        .flatMap { (p, c) =>
          entriesOf(p.ref.conversationId).find(_.id == c.closing).map(_.payload).collect {
            case Payload.Closed(_, _, closing) =>
              ClosedPeriod(p.ref, origin(p.ref.conversationId), c.reason, closing, c.at, c.order)
          }
        }
        .take(n max 0)
    )

  def verdictsOn(period: PeriodRef): Int = verdicts.count(_._1 == period)

  def all(conversation: ConversationId)(using Tx^): Either[StoreError, Vector[Period]] =
    Right(mine(conversation))

  def drop(period: PeriodRef)(using Tx^): Either[StoreError, Boolean] =
    Right(periods.find(_.ref == period).flatMap(p => closedOf(p).map(p -> _)) match {
      case Some((p, c)) if c.purged.nonEmpty =>
        entries.remove(_.id == c.closing)
        periods = periods.filterNot(_.ref == period)
        true
      case _ => false
    })

  def purge(period: PeriodRef, at: Instant)(using Tx^): Either[StoreError, Unit] = {
    periods.find(_.ref == period).foreach { p =>
      closedOf(p).filter(_.purged.isEmpty).foreach { c =>
        entries.remove(e =>
          e.conversationId == period.conversationId && e.id != c.closing &&
            TurnSeq.value(e.turnSeq) >= TurnSeq.value(p.first) &&
            TurnSeq.value(e.turnSeq) <= TurnSeq.value(c.last)
        )
        verdicts = verdicts.filterNot(_._1 == period)
        edges.forget(period.conversationId, p.first, c.last)
        deliveries.forget(period.conversationId, p.first, c.last)
        replace(p.copy(state = c.copy(purged = Some(at))))
      }
    }
    Right(())
  }
}

/** An in-memory [[LifecycleStore]] for tests, keeping [[PeriodContract]]. */
final class InMemoryLifecycleStore extends LifecycleStore {

  // Only ever replaced by a new immutable value, as the store's row would be.
  @caps.unsafe.untrackedCaptures
  private var stored = Option.empty[LifecycleSettings]

  def current()(using Tx^): Either[StoreError, LifecycleSettings] =
    Right(stored.getOrElse(LifecycleSettings.Default))

  def set(settings: LifecycleSettings)(using Tx^): Either[StoreError, Unit] = {
    stored = Some(settings)
    Right(())
  }
}
