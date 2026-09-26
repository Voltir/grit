package grit.core.store

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.id.{CloseRef, ConversationId, EntryId, PeriodRef, PeriodSeq, TurnRef, TurnSeq}
import grit.core.message.Message
import grit.core.period.{
  Activity,
  CloseOrdinal,
  CloseReason,
  Closing,
  Judgement,
  LifecycleSettings,
  Period,
  PeriodState,
  Probability,
  Purgeable,
  TestClosings,
  Verdict,
  Windows
}
import grit.core.place.{Locality, Scope, Weight}

import utest.*

/** The contract every [[PeriodStore]] and [[LifecycleStore]] keeps, run against the
  * in-memory fakes in core and the SQL stores in grit.dbos. The fakes stand in for the SQL
  * stores in every other module's tests, so whatever those tests rely on belongs here.
  *
  * Tests share the stores' database, so each names its own conversations, and filters what
  * spans every conversation to its own.
  */
abstract class PeriodContract extends TestSuite {

  /** The entry store the periods' entries are in. */
  protected def entries: EntryStore

  /** The period store under test, over [[entries]]. */
  protected def periods: PeriodStore

  /** The settings store under test. It holds one set for the whole database, so only one
    * test uses it.
    */
  protected def lifecycle: LifecycleStore

  /** Runs `body` in one transaction, committed when it returns. */
  protected def transaction[A](body: (Tx^) ?=> A): A

  /** A conversation entries may be written to, the same one for the same `name`. */
  protected def conversation(name: String): ConversationId

  /** The origin of [[conversation]]`(name)`. */
  protected def origin(name: String): Origin

  /** How many verdicts are kept on `period`, read past the store's own methods: no method
    * reads a closed period's.
    */
  protected def verdictsOn(period: PeriodRef): Int

  private val Start = Instant.parse("2026-09-20T10:00:00Z")

  /** `minute` minutes into the tests' day. */
  private def at(minute: Long): Instant = Start.plusSeconds(minute * 60)

  private def right[A](result: Either[StoreError, A]): A =
    result.fold(e => throw new java.lang.AssertionError(s"store failed: $e"), identity)

  private def p(x: Double): Probability =
    Probability.of(x).getOrElse(throw new java.lang.AssertionError(x))

  /** How every seal here closes its period. */
  private val Resolved = CloseReason.Resolved(p(0.86))

  private def first(n: Long): PeriodSeq =
    PeriodSeq.of(n).getOrElse(throw new java.lang.AssertionError(n))

  private def closing(text: String): Closing = TestClosings.prose(text, Some(s"$text: done"))

  /** A user message starting `c`'s next turn at `minute`, its period opened for it as the
    * inbox does; the turn.
    */
  private def say(c: ConversationId, minute: Long): TurnRef = transaction {
    val next = right(entries.lockNext(c))
    right(periods.openFor(c, next.turnSeq, at(minute)))
    val id = EntryId(s"${ConversationId.value(c)}:say:${next.seq}")
    right(
      entries.insert(
        Entry(id, c, next.turnSeq, None, next.seq, Payload.Message(Message.User("hi")), at(minute))
      )
    )
    TurnRef(c, next.turnSeq)
  }

  /** A user message added to `turn` at `minute`: activity with no new turn. */
  private def more(turn: TurnRef, minute: Long): Unit = transaction {
    val next = right(entries.lockNext(turn.conversationId))
    val id = EntryId(s"${ConversationId.value(turn.conversationId)}:more:${next.seq}")
    right(
      entries.insert(
        Entry(
          id,
          turn.conversationId,
          turn.turnSeq,
          None,
          next.seq,
          Payload.Message(Message.User("more")),
          at(minute)
        )
      )
    )
  }

  private def seal(period: PeriodRef, last: TurnRef, minute: Long, text: String): Sealed =
    transaction(
      right(
        periods.seal(
          CloseRef(period, last.turnSeq, at(minute)),
          Resolved,
          closing(text),
          at(minute)
        )
      )
    )

  private def ids(listed: Vector[Entry]): Vector[String] = listed.map(e => EntryId.value(e.id))

  /** `state` with its ordinal left out, which depends on every close before it. */
  private def closed(
      state: PeriodState
  ): Option[(TurnSeq, Instant, CloseReason, EntryId, Option[Instant])] =
    state match {
      case PeriodState.Closed(last, when, reason, entry, _, purged) =>
        Some((last, when, reason, entry, purged))
      case PeriodState.Open => None
    }

  private def ordinal(period: PeriodRef): CloseOrdinal =
    transaction(right(periods.get(period))).map(_.state) match {
      case Some(PeriodState.Closed(_, _, _, _, o, _)) => o
      case other => throw new java.lang.AssertionError(s"not closed: $other")
    }

  val tests = Tests {

    test("a conversation's first turn opens period 1 at that turn, and later turns stay in it") {
      val c = conversation("first-period")
      say(c, 0)
      say(c, 1)
      val p1 = PeriodRef(c, PeriodSeq.First)
      transaction(periods.get(p1)) ==>
        Right(Some(Period(p1, TurnSeq(0), at(0), PeriodState.Open)))
      transaction(periods.of(TurnRef(c, TurnSeq(1)))) ==>
        Right(Some(Period(p1, TurnSeq(0), at(0), PeriodState.Open)))
      transaction(periods.get(PeriodRef(c, first(2)))) ==> Right(None)
    }

    test("a seal closes the period, its closing entry after everything in the conversation") {
      val c = conversation("seal")
      val t0 = say(c, 0)
      more(t0, 3)
      val p1 = PeriodRef(c, PeriodSeq.First)
      seal(p1, t0, 30, "sealed") ==> Sealed.Closed(p1.closingId)
      transaction(periods.get(p1)).map(_.map(p => closed(p.state))) ==>
        Right(Some(Some((TurnSeq(0), at(30), Resolved, p1.closingId, None))))
      transaction(entries.get(p1.closingId)) ==> Right(
        Some(
          Entry(
            p1.closingId,
            c,
            TurnSeq(0),
            None,
            2,
            Payload.Closed(PeriodSeq.First, Resolved, closing("sealed")),
            at(30)
          )
        )
      )
    }

    test("the turn after a close opens the next period, at that turn") {
      val c = conversation("next-period")
      val t0 = say(c, 0)
      seal(PeriodRef(c, PeriodSeq.First), t0, 30, "one")
      val t1 = say(c, 40)
      val p2 = PeriodRef(c, first(2))
      transaction(periods.of(t1)) ==>
        Right(Some(Period(p2, TurnSeq(1), at(40), PeriodState.Open)))
      transaction(periods.of(t0)).map(_.map(_.ref)) ==> Right(Some(PeriodRef(c, PeriodSeq.First)))
    }

    test("a seal after a turn came in is abandoned, and writes nothing") {
      val c = conversation("abandoned")
      val t0 = say(c, 0)
      say(c, 5)
      val p1 = PeriodRef(c, PeriodSeq.First)
      seal(p1, t0, 30, "late") ==> Sealed.Abandoned
      transaction(periods.get(p1)).map(_.map(_.state)) ==> Right(Some(PeriodState.Open))
      transaction(entries.get(p1.closingId)) ==> Right(None)
    }

    test("a seal of a period already closed is abandoned, and writes nothing") {
      val c = conversation("sealed-twice")
      val t0 = say(c, 0)
      val p1 = PeriodRef(c, PeriodSeq.First)
      seal(p1, t0, 30, "first") ==> Sealed.Closed(p1.closingId)
      val before = transaction(entries.list(c))
      seal(p1, t0, 31, "second") ==> Sealed.Abandoned
      transaction(entries.list(c)) ==> before
      transaction(periods.get(p1)).map(_.map(p => closed(p.state).map(_._2))) ==>
        Right(Some(Some(at(30))))
    }

    test("a verdict about the newest turn is kept; the activity carries the latest and how many") {
      val c = conversation("judged")
      val t0 = say(c, 0)
      val p1 = PeriodRef(c, PeriodSeq.First)
      val early = Verdict(at(60), t0.turnSeq, Judgement.Unanswered("no classifier"))
      val later = Verdict(
        at(120),
        t0.turnSeq,
        Judgement.Weighed(p(0.86), p(0.04), p(0.10), "jev")
      )
      transaction(periods.judged(p1, early)) ==> Right(true)
      transaction(periods.judged(p1, later)) ==> Right(true)
      transaction(periods.activity(p1)).map(_.map(a => (a.verdict, a.asked))) ==>
        Right(Some((Some(later), 2)))
    }

    test("a verdict about a turn no longer the newest, or a period not open, is ignored") {
      val c = conversation("judged-late")
      val t0 = say(c, 0)
      say(c, 5)
      val p1 = PeriodRef(c, PeriodSeq.First)
      val stale = Verdict(at(60), t0.turnSeq, Judgement.Unanswered("no classifier"))
      transaction(periods.judged(p1, stale)) ==> Right(false)
      transaction(periods.activity(p1)).map(_.map(a => (a.verdict, a.asked))) ==>
        Right(Some((None, 0)))
      val d = conversation("judged-closed")
      val d0 = say(d, 0)
      val q1 = PeriodRef(d, PeriodSeq.First)
      seal(q1, d0, 30, "closed")
      transaction(periods.judged(q1, Verdict(at(60), d0.turnSeq, Judgement.Unanswered("x")))) ==>
        Right(false)
    }

    test("an open period's activity is its newest entry's time and turn, or its opening") {
      val a = conversation("activity-a")
      val b = conversation("activity-b")
      val quiet = conversation("activity-quiet")
      val t0 = say(a, 0)
      more(t0, 3)
      val t1 = say(a, 4)
      more(t1, 6)
      seal(PeriodRef(b, PeriodSeq.First), say(b, 0), 10, "b")
      transaction(periods.openFor(quiet, TurnSeq(0), at(2)))
      val mine = Set(a, b, quiet)
      transaction(periods.open()).map(
        _.filter(x => mine.contains(x.period.conversationId))
          .sortBy(x => ConversationId.value(x.period.conversationId))
      ) ==> Right(
        Vector(
          Activity(PeriodRef(a, PeriodSeq.First), at(6), TurnSeq(1), None, 0),
          Activity(PeriodRef(quiet, PeriodSeq.First), at(2), TurnSeq(0), None, 0)
        ).sortBy(x => ConversationId.value(x.period.conversationId))
      )
      transaction(periods.activity(PeriodRef(b, PeriodSeq.First))) ==> Right(None)
    }

    test("the closing before a turn is the newest one closed before its period") {
      val c = conversation("closings")
      for ((n, minute) <- Vector(1L -> 0L, 2L -> 10L, 3L -> 20L)) {
        seal(PeriodRef(c, first(n)), say(c, minute), minute + 5, s"p$n")
      }
      val now = say(c, 30)
      def closing(turn: TurnRef) =
        transaction(periods.closingBefore(turn)).map(_.map(e => EntryId.value(e.entry.id)))
      val closingOf = (n: Long) => EntryId.value(PeriodRef(c, first(n)).closingId)
      closing(now) ==> Right(Some(closingOf(3)))
      closing(TurnRef(c, TurnSeq(1))) ==> Right(Some(closingOf(1)))
      closing(TurnRef(c, TurnSeq(0))) ==> Right(None)
    }

    test(
      "the open periods elsewhere: every other conversation's open one, with its place, oldest first"
    ) {
      val (me, a, b, gone) =
        (
          conversation("near-me"),
          conversation("near-a"),
          conversation("near-b"),
          conversation("near-gone")
        )
      say(me, 0)
      say(b, 10)
      val a0 = say(a, 20)
      say(a, 21)
      seal(PeriodRef(gone, PeriodSeq.First), say(gone, 5), 30, "gone")
      val theirs = Set(a, b, gone)
      transaction(periods.openElsewhere(me)).map(_.filter(o => theirs(o.conversation))) ==> Right(
        Vector(
          OpenPeriod(b, origin("near-b").place, TurnSeq(0)),
          OpenPeriod(a, origin("near-a").place, a0.turnSeq)
        )
      )
      transaction(periods.openElsewhere(a)).map(_.exists(_.conversation == a)) ==> Right(false)
    }

    test("closed periods are listed in close order across conversations, with their origins") {
      val x = conversation("order-x")
      val y = conversation("order-y")
      val x1 = PeriodRef(x, PeriodSeq.First)
      val y1 = PeriodRef(y, PeriodSeq.First)
      val x2 = PeriodRef(x, first(2))
      // Closed in an order neither conversation nor period number gives.
      val (tx1, ty1) = (say(x, 0), say(y, 1))
      seal(y1, ty1, 20, "y1")
      seal(x1, tx1, 21, "x1")
      seal(x2, say(x, 30), 40, "x2")
      val mine = Set(x, y)
      val all = transaction(periods.closedAfter(CloseOrdinal.Start, 10000))
        .map(_.filter(p => mine.contains(p.ref.conversationId)))
      all.map(_.map(p => (p.ref, p.origin, p.reason, p.closing, p.at))) ==> Right(
        Vector(
          (y1, origin("order-y"), Resolved, closing("y1"), at(20)),
          (x1, origin("order-x"), Resolved, closing("x1"), at(21)),
          (x2, origin("order-x"), Resolved, closing("x2"), at(40))
        )
      )
      all.map(_.map(_.order)) ==> Right(Vector(ordinal(y1), ordinal(x1), ordinal(x2)))
      assert(ordinal(x1).isAfter(ordinal(y1)), ordinal(x2).isAfter(ordinal(x1)))
      transaction(periods.closedAfter(ordinal(y1), 1)).map(_.map(_.ref)) ==> Right(Vector(x1))
    }

    test("expired periods are those closed before the cutoff and not yet purged") {
      val c = conversation("expired")
      val t0 = say(c, 0)
      val t1 = say(c, 1)
      val p1 = PeriodRef(c, PeriodSeq.First)
      seal(p1, t1, 30, "old")
      def mine(cutoff: Instant) =
        transaction(periods.expired(cutoff)).map(_.filter(_.period.conversationId == c))
      mine(at(30)) ==> Right(Vector())
      mine(at(31)) ==> Right(Vector(Purgeable(p1, t0.turnSeq, t1.turnSeq)))
      transaction(periods.purge(p1, at(50))) ==> Right(())
      mine(at(60)) ==> Right(Vector())
    }

    test("a purge deletes its period's raw entries, and keeps its closing entry and the rest") {
      val c = conversation("purge")
      val t0 = say(c, 0)
      more(t0, 1)
      val p1 = PeriodRef(c, PeriodSeq.First)
      seal(p1, say(c, 2), 30, "kept")
      say(c, 40)
      transaction(periods.purge(p1, at(100))) ==> Right(())
      val left = Vector(EntryId.value(p1.closingId), s"${ConversationId.value(c)}:say:4")
      transaction(entries.list(c)).map(ids) ==> Right(left)
      def purged = transaction(periods.get(p1)).map(_.map(p => closed(p.state).flatMap(_._5)))
      purged ==> Right(Some(Some(at(100))))
      transaction(periods.purge(p1, at(200))) ==> Right(())
      purged ==> Right(Some(Some(at(100))))
      transaction(entries.list(c)).map(ids) ==> Right(left)
    }

    test("a purge deletes its period's verdicts, and keeps another period's") {
      val c = conversation("purge-verdicts")
      val other = conversation("purge-verdicts-other")
      val t0 = say(c, 0)
      val o0 = say(other, 0)
      val p1 = PeriodRef(c, PeriodSeq.First)
      val q1 = PeriodRef(other, PeriodSeq.First)
      val weighed = Judgement.Weighed(p(0.9), p(0.05), p(0.05), "jev")
      transaction(periods.judged(p1, Verdict(at(60), t0.turnSeq, weighed))) ==> Right(true)
      transaction(periods.judged(q1, Verdict(at(60), o0.turnSeq, weighed))) ==> Right(true)
      seal(p1, t0, 70, "judged")
      transaction(periods.purge(p1, at(100))) ==> Right(())
      (verdictsOn(p1), verdictsOn(q1)) ==> (0, 1)
    }

    test("the lifecycle's settings are the defaults until seeded, seeded once, then set") {
      def settings(
          idle: FiniteDuration,
          balance: Int,
          resolveAt: Double,
          asks: Int,
          locality: Locality
      ) =
        Windows
          .of(idle, 1.day)
          .flatMap(LifecycleSettings.of(_, balance, idle - 1.minute, p(resolveAt), asks, locality))
          .getOrElse(throw new java.lang.AssertionError(idle))
      def locality(scope: String, weight: Double) =
        (for {
          s <- Scope.read(scope)
          w <- Weight.of(weight)
        } yield Locality(s, w)).fold(e => throw new java.lang.AssertionError(e), identity)
      val (seeded, ignored, later) = (
        settings(3.minutes, 300, 0.8, 3, locality("fs:/home/nick/Projects slack:acme", 1.5)),
        settings(5.minutes, 200, 0.9, 2, Locality.Default),
        settings(7.minutes, 100, 0.75, 1, locality("none", 3))
      )
      transaction(lifecycle.current()) ==> Right(LifecycleSettings.Default)
      transaction(lifecycle.seed(seeded)) ==> Right(seeded)
      transaction(lifecycle.seed(ignored)) ==> Right(seeded)
      transaction(lifecycle.current()) ==> Right(seeded)
      transaction(lifecycle.set(later)) ==> Right(())
      transaction(lifecycle.current()) ==> Right(later)
    }
  }
}
