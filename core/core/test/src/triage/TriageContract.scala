package grit.core.triage

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.{
  CloseRef,
  ConversationId,
  EntryId,
  PeriodRef,
  PeriodSeq,
  QuestionName,
  TriageRef,
  TurnRef
}
import grit.core.message.{Tokens, Usage}
import grit.core.period.{CloseReason, TestClosings}
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

  /** A question set's answers: a choice weighing every key, yes/nos, and a source's. */
  private val weighed = Tags.Weighed(
    VectorMap(
      name("gap") -> Answer.Choice(
        "asks",
        Vector(
          Answer.Weight("asks", 0.625),
          Answer.Weight("owes", 0.125),
          Answer.Weight("nothing", 0.25)
        ),
        Answer.confidence(Vector(0.625, 0.125, 0.25))
      ),
      name("open") -> Answer.YesNo(0.75),
      Earning.Durable -> Answer.YesNo(0.875),
      name("source:github") -> Answer.YesNo(0.5)
    ),
    "jev-1.13.0",
    Usage(Tokens(812), Tokens(40), Tokens.Zero, Some(BigDecimal("0.000034104")))
  )

  private def name(text: String): QuestionName =
    QuestionName.read(text).fold(why => throw new java.lang.AssertionError(why), identity)

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

  /** `c`'s period that holds `e`'s turn, sealed after it, so `c`'s next turn opens another. */
  private def seal(c: ConversationId, e: Entry): Unit = {
    val period = right(transaction(periods.of(TurnRef(c, e.turnSeq))))
      .map(_.ref)
      .getOrElse(throw new java.lang.AssertionError("no period"))
    val _ = right(transaction {
      periods.seal(
        CloseRef(period, e.turnSeq, At),
        CloseReason.Lapsed,
        TestClosings.prose("x", None),
        At
      )
    })
  }

  val tests = Tests {
    test("tagged lists what was tagged in the window, oldest first, each with its triage") {
      val c = conversation("triage-tagged")
      // Far from every other test's tags: the stores' database is shared.
      val t0 = Instant.parse("2031-01-01T00:00:00Z")
      val a = hear(c, "standup moves to 10:00")
      seal(c, a)
      val b = hear(c, "lunch?")
      val d = hear(c, "and after")
      transaction {
        for {
          _ <- triage.record(d.id, weighed, t0.plusSeconds(60))
          _ <- triage.record(a.id, Tags.Unanswered("unavailable: timeout"), t0)
          _ <- triage.record(b.id, weighed, t0.plusSeconds(120))
        } yield ()
      } ==> Right(())
      def ref(e: Entry, seq: Long) =
        TriageRef(PeriodRef(c, PeriodSeq.of(seq).getOrElse(PeriodSeq.First)), e.turnSeq)
      transaction(triage.tagged(t0, t0.plusSeconds(121))) ==> Right(
        Vector(
          TriageStore.Tagged(a.id, ref(a, 1), t0, Tags.Unanswered("unavailable: timeout")),
          TriageStore.Tagged(d.id, ref(d, 2), t0.plusSeconds(60), weighed),
          TriageStore.Tagged(b.id, ref(b, 2), t0.plusSeconds(120), weighed)
        )
      )
      // At or after from, before until.
      transaction(triage.tagged(t0.plusSeconds(60), t0.plusSeconds(120))).map(_.map(_.entry)) ==>
        Right(Vector(d.id))
    }

    test(
      "tags are kept once, weighed or unanswered; a second record is false and keeps the first"
    ) {
      // Weighed tags' answers come back in the order asked: a map's equality would not say.
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
      transaction(triage.of(Vector(a.id))).map(_.get(a.id).collect {
        case Tags.Weighed(answers, _, _) => answers.keys.toVector.map(QuestionName.value)
      }) ==> Right(Some(Vector("gap", "open", "durable", "source:github")))
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
