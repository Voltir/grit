package grit.lifecycle.settle

import java.time.{Instant, ZoneOffset}

import scala.concurrent.duration.FiniteDuration

import grit.act.moves.{MoveRecords, MovesEnv}
import grit.core.act.MovesFixtures.{Answering, PerChar}
import grit.core.classify.{Answer, Answers, Classifier, ClassifierError, Question}
import grit.core.clock.Clock
import grit.core.document.InMemoryDocuments
import grit.core.durable.Durable
import grit.core.edge.InMemoryEdges
import grit.core.id.{ConversationId, EntryId, PeriodRef, PeriodSeq, SettleRef, TurnRef, WorkflowId}
import grit.core.identity.Principal
import grit.core.job.InMemorySchedules
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.Activity
import grit.core.spend.Budget
import grit.core.spend.DailyCap
import grit.core.store.{Askers, InMemoryToolSets}
import grit.core.store.{
  Db,
  Entry,
  InMemoryEntryStore,
  InMemoryLifecycleStore,
  InMemoryPeriodStore,
  InMemoryPrincipals,
  InMemoryUsageLedger,
  Payload,
  StoreError,
  Tx
}
import grit.core.visibility.Integrity
import grit.core.visibility.Subject
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

    // Only ever replaced by a new immutable vector; read only by the test that owns it.
    @caps.unsafe.untrackedCaptures
    var asked = Vector.empty[Question]

    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] = {
      calls += 1
      asked = asked ++ questions
      during()
      weights match {
        case None => Left(ClassifierError.Unavailable("no classifier"))
        case Some(ws) =>
          val answers = questions.flatMap {
            case q: Question.Choice =>
              Answer.choice(q.keys.map(_.name).zip(ws).map(Answer.Weight(_, _)))
            case Question.YesNo(_, _, _) | Question.Score(_, _, _, _) => None
          }
          Right(Answers(answers, Usage(Tokens(1), Tokens(1), Tokens.Zero, None), "jev"))
      }
    }
  }

  object FakeDb extends Db {
    def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  /** One conversation's stores. */
  final class World {
    val entries = new InMemoryEntryStore
    val periods = new InMemoryPeriodStore(entries)
    val lifecycle = new InMemoryLifecycleStore
    val principals = new InMemoryPrincipals
    val ledger = new InMemoryUsageLedger

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
          at(minutes),
          Integrity.Member
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
          at(minutes),
          Integrity.Member
        )
      )
      TurnRef(c, next.turnSeq)
    }

    /** A user message added to `turn` at `minutes`: activity with no new turn. */
    def more(turn: TurnRef, text: String, minutes: Long): Unit = {
      given Tx = TestTx.fake
      val next = entries.lockNext(c).getOrElse(sys.error("in-memory"))
      entries.insert(
        Entry(
          EntryId(s"more:$text"),
          c,
          turn.turnSeq,
          None,
          next.seq,
          Payload.Message(Message.User(text)),
          at(minutes),
          Integrity.Member
        )
      )
    }

    def activity: Option[Activity] =
      periods.activity(p1)(using TestTx.fake).getOrElse(None)

    /** The question a sweep would ask about period 1 as it stands. */
    def question: SettleRef = activity.map(_.question).getOrElse(sys.error("period 1 is not open"))

    /** The settle's body over this world, with `classifier`, at `minutes`, reading through
      * `db`, its asks admitted under `cap` against what [[ledger]] recorded that day.
      */
    def body(
        classifier: Classifier^,
        minutes: Long,
        db: Db^ = FakeDb,
        cap: Option[DailyCap] = None
    )(id: WorkflowId)(using
        Durable^
    ): String = {
      val clock = new Stopped(at(minutes))
      Settle.body(
        SettleEnv(
          SettleRecords(entries, periods, lifecycle, principals),
          db,
          clock,
          movesOf(ledger, classifier, db, clock),
          Budget(ZoneOffset.UTC, cap)
        )
      )(id)
    }
  }

  /** What a lifecycle workflow's asks are made with in a test: `classifier`, cleared as the test
    * classifiers are, its costs kept in `ledger`, read through `db` at `clock`, and nothing else
    * a move could reach (no model answers, no edge, no tool set, no schedule, no asker).
    */
  def movesOf(
      ledger: InMemoryUsageLedger,
      classifier: Classifier^,
      db: Db^,
      clock: Clock^
  ): MovesEnv^ = {
    val edges = new InMemoryEdges
    MovesEnv(
      MoveRecords(
        ledger,
        ledger,
        edges,
        edges,
        new InMemoryToolSets,
        new InMemorySchedules(),
        NoAskers,
        PerChar,
        new InMemoryDocuments().savepoints
      ),
      new Answering("unused"),
      grit.core.classify.TestClassifiers.cleared(classifier),
      db,
      clock
    )
  }

  /** No turn's asker: a lifecycle ask is made for nobody's request. */
  object NoAskers extends Askers {
    def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Principal]] = Right(None)
  }
}
