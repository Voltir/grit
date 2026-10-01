package grit.core.period

import java.time.Instant

import grit.core.id.{EntryId, PeriodRef, TurnSeq}

/** A period's place in the order periods closed in, across every conversation. */
opaque type CloseOrdinal = Long

object CloseOrdinal {

  /** Before every closed period: a plugin that has posted none is here. */
  val Start: CloseOrdinal = 0L

  /** The ordinal `value`; `None` below [[Start]]. */
  def of(value: Long): Option[CloseOrdinal] = Option.when(value >= Start)(value)

  def value(o: CloseOrdinal): Long = o

  extension (o: CloseOrdinal) {
    def next: CloseOrdinal = o + 1
    def isAfter(other: CloseOrdinal): Boolean = o > other
  }
}

/** A period: its conversation and number, its first turn, when it opened, and its state. */
final case class Period(ref: PeriodRef, first: TurnSeq, openedAt: Instant, state: PeriodState)

enum PeriodState {

  /** Taking turns. */
  case Open

  /** Sealed `at` for `reason`, its turns running to `last`, `closing` its closing entry;
    * `purged`: when its raw entries were deleted, if they have been.
    */
  case Closed(
      last: TurnSeq,
      at: Instant,
      reason: CloseReason,
      closing: EntryId,
      order: CloseOrdinal,
      purged: Option[Instant]
  )
}
