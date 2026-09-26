package grit.lifecycle.close

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.core.classify.{Answer, Answers, Classifier, ClassifierError, Question}
import grit.core.clock.Clock
import grit.core.durable.{Durable, InMemoryDurable}
import grit.core.id.{CloseRef, ConversationId, EntryId, PeriodRef, PeriodSeq, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Assignment, Catalog, ModelId, ModelRef, Pinned, Policy}
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError, TokenEstimator}
import grit.core.period.{Activity, CloseOrdinal, CloseReason, Closing, Period, Purgeable}
import grit.core.store.{
  ClosedPeriod,
  Db,
  Entry,
  InMemoryEntryStore,
  InMemoryLifecycleStore,
  InMemoryPeriodStore,
  InMemoryUsageLedger,
  Jot,
  Payload,
  PeriodStore,
  Sealed,
  StoreError,
  Tx
}
import grit.dbos.sql.TestTx

/** The close's test world: one conversation in core's in-memory stores, a clock the test
  * sets, and a summary model and classifier the test scripts.
  */
object CloseFixtures {

  val c: ConversationId = ConversationId("c1")

  val p1: PeriodRef = PeriodRef(c, PeriodSeq.First)

  val Start: Instant = Instant.parse("2026-09-20T10:00:00Z")

  /** `minutes` after [[Start]]. */
  def at(minutes: Long): Instant = Start.plusSeconds(minutes * 60)

  /** A clock the test moves. */
  final class SetClock(start: Instant) extends Clock {
    // An immutable Instant, replaced; read only by the test that owns the clock.
    @caps.unsafe.untrackedCaptures
    var time: Instant = start

    def now(): Instant = time
    def millis(): Long = time.toEpochMilli
    def sleep(duration: FiniteDuration): Unit = time = time.plusMillis(duration.toMillis)
  }

  /** One character a token, and nothing for the prompt. */
  object Chars extends TokenEstimator {
    def message(message: Message): Tokens = Tokens(message.toString.length.toLong)
    def system(prompt: String): Tokens = Tokens.Zero
  }

  val summaryUsage: Usage = Usage(Tokens(100), Tokens(20), Tokens.Zero, Some(BigDecimal("0.0002")))

  /** A reply of `text`, as the summary model would give it. */
  def replyOf(text: String): Message.Assistant =
    Message.Assistant(Vector(AssistantBlock.Text(text)), StopReason.EndTurn, summaryUsage, "summariser")

  /** A summary model answering `script` each call, keeping each request; `during` runs as
    * each call is made, for a test that changes the store while the summary is written.
    */
  final class Summariser(
      script: Int -> Either[ProviderError, Message.Assistant],
      during: () -> Unit = () => ()
  ) extends Provider {
    // Only ever replaced by a new immutable vector; read only by the test that owns it.
    @caps.unsafe.untrackedCaptures
    var requests = Vector.empty[ModelRequest]

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      val n = requests.size
      requests = requests :+ request
      during()
      script(n)
    }
  }

  val TestCatalog: Catalog = {
    val a = Assignment(
      ModelRef(ModelId.of("test/summary").getOrElse(throw new java.lang.AssertionError("id")), None),
      1024,
      None
    )
    Catalog.of(Policy(a, a, a), Vector.empty)
  }

  /** Every role's calls to `provider`. */
  final class OneModel(provider: Provider^) extends Models {
    def catalog(): Either[String, Catalog] = Right(TestCatalog)
    def provider(pinned: Pinned): Provider^ = provider
  }

  /** A classifier answering each yes/no with the next of `yes`, counting its calls; or, with
    * none, unavailable.
    */
  final class Gate(yes: Option[Vector[Double]]) extends Classifier {
    // A count, read only by the test that owns the gate.
    @caps.unsafe.untrackedCaptures
    var calls = 0

    protected def answer(state: ujson.Value, questions: Vector[Question]): Either[ClassifierError, Answers] = {
      calls += 1
      yes match {
        case None => Left(ClassifierError.Unavailable("no classifier"))
        case Some(ps) =>
          Right(Answers(ps.take(questions.size).map(Answer.YesNo(_)), Usage(Tokens(1), Tokens(1), Tokens.Zero, None), "jev"))
      }
    }
  }

  /** Writes straight through to the in-memory stores, never rolled back. */
  final class FakeJot extends Jot {
    def write[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = body(using TestTx.fake)
  }

  object FakeDb extends Db {
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = body(using TestTx.fake)
  }

  /** One conversation's stores. */
  final class World {
    val entries = new InMemoryEntryStore
    val periods = new InMemoryPeriodStore(entries)
    val lifecycle = new InMemoryLifecycleStore
    val ledger = new InMemoryUsageLedger

    /** A user message starting the next turn at `minutes`, its period opened as the inbox
      * does.
      */
    def say(text: String, minutes: Long): TurnRef = {
      given Tx = TestTx.fake
      val next = entries.lockNext(c).getOrElse(sys.error("in-memory"))
      periods.openFor(c, next.turnSeq, at(minutes))
      entries.insert(Entry(EntryId(s"in:$text"), c, next.turnSeq, None, next.seq, Payload.Message(Message.User(text)), at(minutes)))
      TurnRef(c, next.turnSeq)
    }

    /** `payload` added to `turn` at `minutes`. */
    def add(turn: TurnRef, payload: Payload, minutes: Long, id: String): Unit = {
      given Tx = TestTx.fake
      val next = entries.lockNext(c).getOrElse(sys.error("in-memory"))
      val _ = entries.insert(Entry(EntryId(id), c, turn.turnSeq, None, next.seq, payload, at(minutes)))
    }

    /** A turn of `question`, its reply and its summary, all at `minutes`. */
    def turn(question: String, answer: String, summary: String, minutes: Long): TurnRef = {
      val t = say(question, minutes)
      add(t, Payload.Message(replyOf(answer)), minutes, s"reply:$question")
      add(t, Payload.Summary(summary), minutes, s"summary:$question")
      t
    }

    def all: Vector[Entry] = entries.list(c)(using TestTx.fake).getOrElse(Vector.empty)

    def closingEntry: Option[Entry] = entries.get(p1.closingId)(using TestTx.fake).getOrElse(None)

    /** The close's body over this world, with `gate`, `summary` and `clock`, its periods
      * `sealing`.
      */
    def body(gate: Classifier^, summary: Provider^, clock: Clock^, sealing: PeriodStore = periods)(
        id: WorkflowId
    )(using Durable^): String =
      Close.body(
        CloseEnv(
          CloseRecords(entries, sealing, lifecycle, ledger, Chars),
          gate,
          new OneModel(summary),
          FakeDb,
          clock
        )
      )(id)
  }

  /** The attempt to close period 1 when `last` was its newest turn. */
  def attempt(last: TurnRef): CloseRef = CloseRef(p1, last.turnSeq)

  /** `underlying`, dying once inside the first seal, before it writes. */
  final class CrashOnSeal(underlying: PeriodStore) extends PeriodStore {
    // A flag, set once; nothing but this store reads it.
    @caps.unsafe.untrackedCaptures
    var armed = true

    def seal(attempt: CloseRef, reason: CloseReason, closing: Closing, at: Instant)(using
        Tx^
    ): Either[StoreError, Sealed] =
      if (armed) { armed = false; throw new InMemoryDurable.Crash }
      else underlying.seal(attempt, reason, closing, at)

    def openFor(conversation: ConversationId, turn: TurnSeq, at: Instant)(using Tx^): Either[StoreError, Period] =
      underlying.openFor(conversation, turn, at)
    def get(period: PeriodRef)(using Tx^): Either[StoreError, Option[Period]] = underlying.get(period)
    def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Period]] = underlying.of(turn)
    def signal(conversation: ConversationId, at: Instant)(using Tx^): Either[StoreError, Boolean] =
      underlying.signal(conversation, at)
    def open()(using Tx^): Either[StoreError, Vector[Activity]] = underlying.open()
    def activity(period: PeriodRef)(using Tx^): Either[StoreError, Option[Activity]] = underlying.activity(period)
    def closingsBefore(turn: TurnRef, n: Int)(using Tx^): Either[StoreError, Vector[Entry]] =
      underlying.closingsBefore(turn, n)
    def closedAfter(after: CloseOrdinal, n: Int)(using Tx^): Either[StoreError, Vector[ClosedPeriod]] =
      underlying.closedAfter(after, n)
    def expired(cutoff: Instant)(using Tx^): Either[StoreError, Vector[Purgeable]] = underlying.expired(cutoff)
    def purge(period: PeriodRef, at: Instant)(using Tx^): Either[StoreError, Unit] = underlying.purge(period, at)
  }

  /** Turn numbers. */
  def turn(n: Long): TurnSeq = TurnSeq(n)
}
