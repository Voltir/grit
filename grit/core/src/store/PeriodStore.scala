package grit.core.store

import java.time.Instant

import grit.core.id.{CloseRef, ConversationId, EntryId, PeriodRef, TurnRef, TurnSeq}
import grit.core.period.{
  Activity,
  Balance,
  CloseOrdinal,
  CloseReason,
  Closing,
  Period,
  Purgeable,
  Verdict
}

/** A conversation's periods: which is open, closing one, and deleting what a closed one no
  * longer needs. Each writing method takes the conversation's lock itself
  * ([[EntryStore.lockNext]]).
  */
trait PeriodStore {

  /** The period for `conversation`'s turn `turn`: its open one, or, when none is open, a new
    * one starting at `turn`, opened `at`, numbered after its newest.
    */
  def openFor(conversation: ConversationId, turn: TurnSeq, at: Instant)(using
      Tx^
  ): Either[StoreError, Period]

  /** The period `period` names, or `None` when it has not opened. */
  def get(period: PeriodRef)(using Tx^): Either[StoreError, Option[Period]]

  /** The period `turn` is in; `None` for a turn in no period, such as one recorded before
    * its conversation had any.
    */
  def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Period]]

  /** Records what the classifier made of `period` once it went quiet; a verdict whose last
    * turn is no longer the period's newest, or about a period not open, is ignored. Whether
    * it was recorded.
    */
  def judged(period: PeriodRef, verdict: Verdict)(using Tx^): Either[StoreError, Boolean]

  /** Every open period, as its deadline sees it. */
  def open()(using Tx^): Either[StoreError, Vector[Activity]]

  /** `period` as its deadline sees it; `None` when it is not open. */
  def activity(period: PeriodRef)(using Tx^): Either[StoreError, Option[Activity]]

  /** Closes `attempt.period` at `at` for `reason`, with `closing` recorded as its closing
    * entry after everything in the conversation, if it is open and its newest turn is still
    * `attempt.last`; `Abandoned` otherwise, with nothing written. It takes the next
    * [[CloseOrdinal]], and no seal commits before one that took an earlier ordinal.
    */
  def seal(attempt: CloseRef, reason: CloseReason, closing: Closing, at: Instant)(using
      Tx^
  ): Either[StoreError, Sealed]

  /** The closing entry of the newest period of `turn`'s conversation that closed before
    * `turn`'s period opened; `None` before its first close.
    */
  def closingBefore(turn: TurnRef)(using Tx^): Either[StoreError, Option[ClosingEntry]]

  /** Closed periods after `after` in close order, at most `n`. */
  def closedAfter(after: CloseOrdinal, n: Int)(using Tx^): Either[StoreError, Vector[ClosedPeriod]]

  /** Periods closed before `cutoff` whose raw entries are still kept. */
  def expired(cutoff: Instant)(using Tx^): Either[StoreError, Vector[Purgeable]]

  /** Deletes `period`'s raw entries (every entry of its turns but its closing entry) and
    * records it purged at `at`; a period already purged, or not closed, is left as it is.
    * Its workflows are the caller's to delete, first ([[Purgeable.turns]],
    * [[Purgeable.attempts]]).
    */
  def purge(period: PeriodRef, at: Instant)(using Tx^): Either[StoreError, Unit]

  /** Where `turn`'s window opens: the first turn of its period ([[TurnSeq.First]] for a turn
    * in none), and the closing it opens from.
    */
  final def opening(turn: TurnRef)(using Tx^): Either[StoreError, Opening] =
    for {
      period <- of(turn)
      before <- closingBefore(turn)
    } yield Opening(period.fold(TurnSeq.First)(_.first), before)
}

/** What a turn's window, or a close, starts from: the first turn of its period, and the
  * closing entry of the period before it, if any.
  */
final case class Opening(first: TurnSeq, closing: Option[ClosingEntry]) {

  /** The balance it opens with; empty before the conversation's first close. */
  def balance: Balance = closing.fold(Balance.empty)(_.closing.balance)
}

/** A closing entry, and the closing its payload holds. */
final case class ClosingEntry private (entry: Entry, closing: Closing)

object ClosingEntry {

  /** `entry` as a closing entry; `None` when its payload is not [[Payload.Closed]]. */
  def of(entry: Entry): Option[ClosingEntry] = entry.payload match {
    case Payload.Closed(_, _, closing) => Some(new ClosingEntry(entry, closing))
    case _ => None
  }
}

/** What a seal came to. */
enum Sealed {

  /** The period closed, `entry` its closing entry. */
  case Closed(entry: EntryId)

  /** Nothing was written: the period was not open, or a turn came in after the attempt. */
  case Abandoned
}

/** A closed period as a plugin sees it: never its raw entries. */
final case class ClosedPeriod(
    ref: PeriodRef,
    origin: Origin,
    reason: CloseReason,
    closing: Closing,
    at: Instant,
    order: CloseOrdinal
)
