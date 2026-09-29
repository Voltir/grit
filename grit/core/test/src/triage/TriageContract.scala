package grit.core.triage

import java.time.Instant

import grit.core.id.{CloseRef, ConversationId, EntryId, TurnRef}
import grit.core.message.{Tokens, Usage}
import grit.core.period.{CloseReason, Probability, TestClosings}
import grit.core.store.{Entry, EntryStore, Payload, PeriodStore, StoreError, Tx}

import utest.*

/** The contract every [[TriageStore]] keeps, run against the in-memory fake in core and the
  * SQL store in grit.dbos. Tests share the store's database, so each names its own
  * conversation.
  */
abstract class TriageContract extends TestSuite {

  /** The entry store the tagged entries are in. */
  protected def entries: EntryStore

  /** The periods of those entries, whose purge deletes them. */
  protected def periods: PeriodStore

  /** The triage store under test, over [[entries]]. */
  protected def triage: TriageStore

  protected def transaction[A](body: (Tx^) ?=> A): A

  /** A conversation entries may be written to, the same one for the same `name`. */
  protected def conversation(name: String): ConversationId

  private val At = Instant.parse("2026-09-28T10:00:00Z")

  private def right[A](result: Either[StoreError, A]): A =
    result.fold(e => throw new java.lang.AssertionError(s"store failed: $e"), identity)

  private def p(x: Double): Probability =
    Probability.of(x).getOrElse(throw new java.lang.AssertionError(x))

  private val weighed = Tags.Weighed(
    Kind.Decision,
    p(0.75),
    p(0.125),
    p(0.875),
    p(0.25),
    "jev-1.13.0",
    Usage(Tokens(812), Tokens(40), Tokens.Zero, Some(BigDecimal("0.000034104")))
  )

  /** A heard message as `c`'s next turn, its period opened for it as the inbox does. */
  private def hear(c: ConversationId, text: String): Entry = transaction {
    val next = right(entries.lockNext(c))
    right(periods.openFor(c, next.turnSeq, At))
    val e = Entry(
      EntryId(s"${ConversationId.value(c)}:heard:${next.seq}"),
      c,
      next.turnSeq,
      None,
      next.seq,
      Payload.Heard(text),
      At
    )
    right(entries.insert(e))
    e
  }

  val tests = Tests {
    test(
      "tags are kept once, weighed or unanswered; a second record is false and keeps the first"
    ) {
      val c = conversation("triage-once")
      val a = hear(c, "standup moves to 10:00")
      val b = hear(c, "lunch?")
      transaction {
        for {
          first <- triage.record(a.id, weighed, At)
          again <- triage.record(a.id, Tags.Unanswered("late"), At)
          other <- triage.record(b.id, Tags.Unanswered("unavailable: timeout"), At)
        } yield (first, again, other)
      } ==> Right((true, false, true))
      transaction(triage.of(Vector(a.id, b.id, EntryId("never")))) ==>
        Right(Map(a.id -> weighed, b.id -> Tags.Unanswered("unavailable: timeout")))
    }

    test("tags go with their entry: a purged entry has none, and none is recorded for it") {
      val c = conversation("triage-purged")
      val a = hear(c, "standup moves to 10:00")
      transaction(triage.record(a.id, weighed, At)) ==> Right(true)
      val period = right(transaction(periods.of(TurnRef(c, a.turnSeq))))
        .map(_.ref)
        .getOrElse(throw new java.lang.AssertionError("no period"))
      transaction {
        for {
          _ <- periods.seal(
            CloseRef(period, a.turnSeq, At),
            CloseReason.Lapsed,
            TestClosings.prose("heard", None),
            At
          )
          _ <- periods.purge(period, At)
        } yield ()
      }
      transaction(triage.of(Vector(a.id))) ==> Right(Map.empty)
      transaction(triage.record(a.id, weighed, At)) ==> Right(false)
      transaction(triage.of(Vector(a.id))) ==> Right(Map.empty)
    }
  }
}
