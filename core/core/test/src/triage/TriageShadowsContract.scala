package grit.core.triage

import java.time.Instant

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.*

import grit.core.classify.{Answer, ClassifierError}
import grit.core.id.{
  CloseRef,
  ConversationId,
  EntryId,
  KnowledgeSourceName,
  PeriodRef,
  PeriodSeq,
  QuestionName,
  ShadowName,
  TriageRef,
  TurnRef
}
import grit.core.message.{Tokens, Usage}
import grit.core.period.{CloseReason, TestClosings}
import grit.core.store.{Entry, EntryStore, Payload, PeriodStore, StoreError, Tx}

import utest.*

/** The contract every [[TriageShadows]] keeps, run against the in-memory fake in core and the
  * SQL store in grit.dbos. Tests share the store's database, so each names its own
  * conversation and its own variants.
  */
abstract class TriageShadowsContract extends TestSuite {

  protected def entries: EntryStore

  protected def periods: PeriodStore

  /** Where the heard messages are tagged. */
  protected def triage: TriageStore

  /** The store under test, over [[entries]] and [[triage]]. */
  protected def shadows: TriageShadows

  protected def transaction[A](body: (Tx^) ?=> A): A

  /** A conversation entries may be written to, the same one for the same `name`. */
  protected def conversation(name: String): ConversationId

  private val At = Instant.parse("2026-09-28T10:00:00Z")

  private def right[A](result: Either[StoreError, A]): A =
    result.fold(e => throw new java.lang.AssertionError(s"store failed: $e"), identity)

  private def named(s: String): ShadowName =
    ShadowName.of(s).getOrElse(throw new java.lang.AssertionError(s))

  private def answered(cost: Option[String]): Shadowed =
    answeredAs(
      cost,
      ShadowAnswers.Worded(
        Vector(
          Answer.Choice(
            "decision",
            Vector(Answer.Weight("question", 0.25), Answer.Weight("decision", 0.75)),
            0.5
          ),
          Answer.YesNo(0.125)
        )
      )
    )

  private def answeredAs(cost: Option[String], answers: ShadowAnswers): Shadowed =
    Shadowed.Answered(
      "d1g35t",
      answers,
      Usage(Tokens(812), Tokens(40), Tokens.Zero, cost.map(BigDecimal(_))),
      "jev-1.13.0",
      "jev-1.13.0+r2",
      1234.millis
    )

  private val failed: Shadowed =
    Shadowed.Failed("f41l3d", ClassifierError.Kind.Unavailable, 30.seconds)

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

  private val unanswered = Tags.Unanswered("unavailable: timeout")

  private def question(text: String): QuestionName =
    QuestionName.of(text).getOrElse(throw new java.lang.AssertionError(text))

  /** A question set's answers, in an order that is not their names' sorted order. */
  private val asked: Vector[(QuestionName, Answer)] = Vector(
    question("to") -> Answer.YesNo(0.125),
    question("gap") -> Answer.Choice("asks", Vector(Answer.Weight("asks", 1.0)), 1.0),
    QuestionName.per(
      question("source"),
      KnowledgeSourceName.of("github").getOrElse(throw new java.lang.AssertionError("github"))
    ) -> Answer.YesNo(0.5)
  )

  val tests = Tests {
    test("a question set's answers read back under their names, in the order asked") {
      val c = conversation("shadows-named")
      val set = named("named-set")
      val a = hear(c, "who owns the deploy?")
      val row = answeredAs(Some("0.00004"), ShadowAnswers.Named(VectorMap.from(asked)))
      transaction(shadows.record(a.id, set, row, At)) ==> Right(true)
      transaction(shadows.of(set, Vector(a.id))).map(_.get(a.id)) ==> Right(Some(row))
      // A VectorMap's equality ignores its order.
      transaction(shadows.of(set, Vector(a.id))).map(_.get(a.id).map {
        case Shadowed.Answered(_, ShadowAnswers.Named(as), _, _, _, _) => as.toVector
        case Shadowed.Answered(_, ShadowAnswers.Worded(_), _, _, _, _) | Shadowed.Failed(_, _, _) =>
          Vector.empty
      }) ==> Right(Some(asked))
    }

    test("a row is kept once per variant; a second is false and keeps the first") {
      val c = conversation("shadows-once")
      val (words, replica) = (named("once-words"), named("once-replica"))
      val a = hear(c, "standup moves to 10:00")
      val b = hear(c, "lunch?")
      transaction {
        for {
          first <- shadows.record(a.id, words, answered(Some("0.00004")), At)
          again <- shadows.record(a.id, words, failed, At)
          other <- shadows.record(a.id, replica, failed, At)
          second <- shadows.record(b.id, words, failed, At)
        } yield (first, again, other, second)
      } ==> Right((true, false, true, true))
      transaction(shadows.of(words, Vector(a.id, b.id, EntryId("never")))) ==>
        Right(Map(a.id -> answered(Some("0.00004")), b.id -> failed))
      transaction(shadows.of(replica, Vector(a.id, b.id))) ==> Right(Map(a.id -> failed))
    }

    test("rows go with their entry: a purged entry has none, and none is recorded for it") {
      val c = conversation("shadows-purged")
      val words = named("purged-words")
      val a = hear(c, "standup moves to 10:00")
      transaction(shadows.record(a.id, words, answered(Some("0.00004")), At)) ==> Right(true)
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
      transaction(shadows.of(words, Vector(a.id))) ==> Right(Map.empty)
      transaction(shadows.record(a.id, words, failed, At)) ==> Right(false)
      transaction(shadows.spent(words, At)) ==> Right(BigDecimal(0))
    }

    test(
      "unshadowed lists the messages tagged since, not yet shadowed by the variant, oldest first, up to the limit"
    ) {
      val c = conversation("shadows-unshadowed")
      val (words, replica) = (named("unshadowed-words"), named("unshadowed-replica"))
      // Far from every other test's tags: the stores' database is shared.
      val t0 = Instant.parse("2041-01-01T00:00:00Z")
      val es = Vector("before", "first", "second", "third", "fourth").map(hear(c, _))
      transaction {
        es.zipWithIndex.foldLeft[Either[StoreError, Unit]](Right(())) { case (acc, (e, i)) =>
          // Tagged in reverse of their order, so oldest-tagged is not oldest-heard.
          acc.flatMap(_ => triage.record(e.id, unanswered, t0.plusSeconds(100L - i)).map(_ => ()))
        }
      } ==> Right(())
      val Vector(_, first, second, third, fourth) = es: @unchecked
      transaction {
        for {
          _ <- shadows.record(third.id, words, failed, At)
          _ <- shadows.record(fourth.id, replica, failed, At)
        } yield ()
      } ==> Right(())
      def ref(e: Entry) = TriageRef(PeriodRef(c, PeriodSeq.First), e.turnSeq)
      // Tagged at or after t0 + 97: third, second, first, then before; third is shadowed by
      // words. From t0 + 96, fourth comes first: it is shadowed by replica alone.
      transaction(shadows.unshadowed(words, t0.plusSeconds(97), 10)) ==>
        Right(Vector(ref(second), ref(first), ref(es(0))))
      transaction(shadows.unshadowed(words, t0.plusSeconds(96), 2)) ==>
        Right(Vector(ref(fourth), ref(second)))
    }

    test(
      "spent sums the variant's reported costs kept from a time on; recent is its latest answered costs, newest first"
    ) {
      val c = conversation("shadows-spent")
      val (words, other) = (named("spent-words"), named("spent-other"))
      val es = Vector("a", "b", "c", "d", "e").map(hear(c, _))
      transaction {
        for {
          _ <- shadows.record(es(0).id, words, answered(Some("0.0001")), At)
          _ <- shadows.record(es(1).id, words, answered(Some("0.0002")), At.plusSeconds(10))
          _ <- shadows.record(es(2).id, words, failed, At.plusSeconds(20))
          _ <- shadows.record(es(3).id, words, answered(None), At.plusSeconds(30))
          _ <- shadows.record(es(4).id, words, answered(Some("0.0004")), At.plusSeconds(40))
          _ <- shadows.record(es(0).id, other, answered(Some("0.5")), At.plusSeconds(10))
        } yield ()
      } ==> Right(())
      transaction(shadows.spent(words, At)) ==> Right(BigDecimal("0.0007"))
      transaction(shadows.spent(words, At.plusSeconds(10))) ==> Right(BigDecimal("0.0006"))
      transaction(shadows.spent(words, At.plusSeconds(41))) ==> Right(BigDecimal(0))
      transaction(shadows.recent(words, 2)) ==> Right(
        Vector(BigDecimal("0.0004"), BigDecimal("0.0002"))
      )
      transaction(shadows.recent(words, 10)) ==>
        Right(Vector(BigDecimal("0.0004"), BigDecimal("0.0002"), BigDecimal("0.0001")))
    }
  }
}
