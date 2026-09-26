package grit.lifecycle.settle

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.core.classify.{Answer, Answers, Classifier, ClassifierError, Question}
import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.id.{ConversationId, EntryId, PeriodRef, PeriodSeq, SettleRef, TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.Activity
import grit.core.store.{
  Db,
  Entry,
  InMemoryEntryStore,
  InMemoryLifecycleStore,
  InMemoryPeriodStore,
  Payload,
  StoreError,
  Tx
}
import grit.dbos.sql.TestTx

/** The settle's test world: one conversation in core's in-memory stores, a clock the test
  * sets, and a classifier the test scripts.
  */
object SettleFixtures {

  val c: ConversationId = ConversationId("c1")

  val p1: PeriodRef = PeriodRef(c, PeriodSeq.First)

  val Start: Instant = Instant.parse("2026-09-20T10:00:00Z")

  /** `minutes` after [[Start]]. */
  def at(minutes: Long): Instant = Start.plusSeconds(minutes * 60)

  /** A clock stopped at `time`. */
  final class Stopped(time: Instant) extends Clock {
    def now(): Instant = time
    def millis(): Long = time.toEpochMilli
    def sleep(duration: FiniteDuration): Unit = ()
  }

  /** A classifier weighing a choice's options by `weights`, in the question's order, and
    * counting its calls; unavailable with none. `during` runs as each call is made, for a
    * test that changes the store while the classifier is asked.
    */
  final class Weigher(weights: Option[Vector[Double]], during: () -> Unit = () => ())
      extends Classifier {
    // A count, read only by the test that owns the classifier.
    @caps.unsafe.untrackedCaptures
    var calls = 0

    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] = {
      calls += 1
      during()
      weights match {
        case None => Left(ClassifierError.Unavailable("no classifier"))
        case Some(ws) =>
          val answers = questions.flatMap {
            case q: Question.Choice =>
              Answer.choice(q.keys.map(_.name).zip(ws).map(Answer.Weight(_, _)))
            case Question.YesNo(_, _, _) => None
          }
          Right(Answers(answers, Usage(Tokens(1), Tokens(1), Tokens.Zero, None), "jev"))
      }
    }
  }

  object FakeDb extends Db {
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  /** One conversation's stores. */
  final class World {
    val entries = new InMemoryEntryStore
    val periods = new InMemoryPeriodStore(entries)
    val lifecycle = new InMemoryLifecycleStore

    /** A user message starting the next turn at `minutes`, and a reply, its period opened as
      * the inbox does.
      */
    def turn(text: String, minutes: Long): TurnRef = {
      given Tx = TestTx.fake
      val next = entries.lockNext(c).getOrElse(sys.error("in-memory"))
      periods.openFor(c, next.turnSeq, at(minutes))
      entries.insert(
        Entry(
          EntryId(s"in:$text"),
          c,
          next.turnSeq,
          None,
          next.seq,
          Payload.Message(Message.User(text)),
          at(minutes)
        )
      )
      val reply = entries.lockNext(c).getOrElse(sys.error("in-memory"))
      entries.insert(
        Entry(
          EntryId(s"reply:$text"),
          c,
          next.turnSeq,
          None,
          reply.seq,
          Payload.Message(
            Message.Assistant(
              Vector(AssistantBlock.Text(s"re: $text")),
              StopReason.EndTurn,
              Usage(Tokens(1), Tokens(1), Tokens.Zero, None),
              "turn"
            )
          ),
          at(minutes)
        )
      )
      TurnRef(c, next.turnSeq)
    }

    def activity: Option[Activity] =
      periods.activity(p1)(using TestTx.fake).getOrElse(None)

    /** The question a sweep would ask about period 1 as it stands. */
    def question: SettleRef = activity.map(_.question).getOrElse(sys.error("period 1 is not open"))

    /** The settle's body over this world, with `classifier`, at `minutes`. */
    def body(classifier: Classifier^, minutes: Long)(id: WorkflowId)(using Durable^): String =
      Settle.body(
        SettleEnv(SettleRecords(entries, periods, lifecycle), classifier, FakeDb, new Stopped(at(minutes)))
      )(id)
  }
}
