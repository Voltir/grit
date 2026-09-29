package grit.lifecycle.triage

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.core.classify.{Answer, Answers, Classifier, ClassifierError, Question}
import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.id.{
  CloseRef,
  ConversationId,
  EntryId,
  PeriodRef,
  PeriodSeq,
  PrincipalId,
  TriageRef,
  TurnRef,
  WorkflowId
}
import grit.core.message.{Message, Tokens, Usage}
import grit.core.period.{CloseReason, TestClosings}
import grit.core.store.{
  Db,
  Entry,
  InMemoryEntryStore,
  InMemoryPeriodStore,
  InMemoryPrincipals,
  Payload,
  StoreError,
  Tx
}
import grit.core.triage.InMemoryTriageStore
import grit.dbos.sql.TestTx

/** The triage's test world: one conversation in core's in-memory stores, and a classifier the
  * test scripts.
  */
object TriageFixtures {

  val c: ConversationId = ConversationId("c1")

  val p1: PeriodRef = PeriodRef(c, PeriodSeq.First)

  val Start: Instant = Instant.parse("2026-09-28T10:00:00Z")

  def at(minutes: Long): Instant = Start.plusSeconds(minutes * 60)

  /** What a scripted classifier's answers cost. */
  val Spent: Usage = Usage(Tokens(640), Tokens(0), Tokens.Zero, Some(BigDecimal("0.00002688")))

  /** A classifier answering each choice with `kinds` (weights in the options' order) and each
    * yes/no, in order, with `yes`; unavailable when `kinds` is empty. It counts its calls,
    * keeps each state it was shown, and runs `during` as each call is made.
    */
  final class Scripted(kinds: Vector[Double], yes: Vector[Double], during: () -> Unit = () => ())
      extends Classifier {
    // Counts and states, read only by the test that owns the classifier.
    @caps.unsafe.untrackedCaptures
    var calls = 0

    @caps.unsafe.untrackedCaptures
    var states = Vector.empty[ujson.Value]

    @caps.unsafe.untrackedCaptures
    var asked = Vector.empty[Question]

    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] = {
      calls += 1
      states = states :+ state
      asked = asked ++ questions
      during()
      if (kinds.isEmpty) Left(ClassifierError.Unavailable("no classifier"))
      else {
        val yeses = yes.iterator
        val answers = questions.flatMap {
          case q: Question.Choice =>
            Answer.choice(q.keys.map(_.name).zip(kinds).map(Answer.Weight(_, _)))
          case Question.YesNo(_, _, _) => yeses.nextOption().map(Answer.YesNo(_))
        }
        Right(Answers(answers, Spent, "jev-1.13.0"))
      }
    }
  }

  final class Stopped(time: Instant) extends Clock {
    def now(): Instant = time
    def millis(): Long = time.toEpochMilli
    def sleep(duration: FiniteDuration): Unit = ()
  }

  object FakeDb extends Db {
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  /** One conversation's stores. */
  final class World {
    val entries = new InMemoryEntryStore
    val periods = new InMemoryPeriodStore(entries)
    val principals = new InMemoryPrincipals
    val triage = new InMemoryTriageStore(entries)

    private def insert(payload: Payload, by: Option[(String, String)], minutes: Long): Entry = {
      given Tx = TestTx.fake
      val next = entries.lockNext(c).getOrElse(sys.error("in-memory"))
      periods.openFor(c, next.turnSeq, at(minutes))
      val e = Entry(EntryId(s"e${next.seq}"), c, next.turnSeq, None, next.seq, payload, at(minutes))
      entries.insert(e)
      by.foreach { (id, name) =>
        principals.enroll(PrincipalId(id), name)
        principals.authored(e.id, PrincipalId(id))
      }
      e
    }

    /** `text` heard from `name` as the next turn at `minutes`; its triage. */
    def hear(text: String, name: String, minutes: Long): TriageRef = {
      val e = insert(Payload.Heard(text), Some(s"u-$name" -> name), minutes)
      TriageRef(p1, e.turnSeq)
    }

    /** `text` said to grit as the next turn at `minutes`; its turn. */
    def say(text: String, minutes: Long): TurnRef = {
      val e = insert(Payload.Message(Message.User(text)), None, minutes)
      TurnRef(c, e.turnSeq)
    }

    /** Period 1 closed after `last` and its raw entries purged: every entry gone. */
    def purge(last: TurnRef): Unit = {
      given Tx = TestTx.fake
      periods.seal(CloseRef(p1, last.turnSeq, at(60)), CloseReason.Lapsed, TestClosings.prose("x"), at(60))
      periods.purge(p1, at(61))
      ()
    }

    def tags(triage: TriageRef) =
      entries
        .list(c)(using TestTx.fake)
        .getOrElse(Vector.empty)
        .find(_.turnSeq == triage.turn)
        .flatMap(e => this.triage.of(Vector(e.id))(using TestTx.fake).toOption.flatMap(_.get(e.id)))

    /** The triage's body over this world, with `classifier`, at `minutes`. */
    def body(classifier: Classifier^, minutes: Long)(id: WorkflowId)(using Durable^): String =
      Triage.body(
        TriageEnv(TriageRecords(entries, triage, principals), classifier, FakeDb, new Stopped(at(minutes)))
      )(id)
  }
}
