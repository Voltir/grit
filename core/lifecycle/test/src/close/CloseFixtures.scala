package grit.lifecycle.close

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.core.classify.{Answer, Answers, Classifier, ClassifierError, Question}
import grit.core.clock.Clock
import grit.core.durable.{Durable, InMemoryDurable}
import grit.core.id.{
  CloseRef,
  ConversationId,
  EntryId,
  PeriodRef,
  PeriodSeq,
  PrincipalId,
  TurnRef,
  TurnSeq,
  WorkflowId
}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Assignment, Catalog, ModelId, ModelRef, Pinned, Policy}
import grit.core.period.{Activity, CloseOrdinal, CloseReason, Closing, Period, Verdict}
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError, TokenEstimator}
import grit.core.store.{
  ClosedElsewhere,
  ClosedPeriod,
  ClosingEntry,
  Db,
  Entry,
  InMemoryEntryStore,
  InMemoryLifecycleStore,
  InMemoryPeriodStore,
  InMemoryPrincipals,
  InMemoryTombstones,
  InMemoryUsageLedger,
  Jot,
  OpenPeriod,
  Payload,
  PeriodStore,
  Sealed,
  StoreError,
  Tx
}
import grit.core.triage.{InMemoryTriageStore, Tags}
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
    Message.Assistant(
      Vector(AssistantBlock.Text(text)),
      StopReason.EndTurn,
      summaryUsage,
      "summariser"
    )

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
      ModelRef(
        ModelId.of("test/summary").getOrElse(throw new java.lang.AssertionError("id")),
        None
      ),
      1024,
      None
    )
    // The heard role on its own budget, so a test can tell its pin from the summary's.
    Catalog.of(Policy(a, a, a, a.copy(maxTokens = 512)), Vector.empty)
  }

  /** Every role's calls to `provider`, keeping each pin a provider was asked for. */
  final class OneModel(provider: Provider^) extends Models {
    // Only ever replaced by a new immutable vector; read only by the test that owns it.
    @caps.unsafe.untrackedCaptures
    var pins = Vector.empty[Pinned]

    def catalog(): Either[String, Catalog] = Right(TestCatalog)
    def provider(pinned: Pinned): Provider^ = {
      pins = pins :+ pinned
      provider
    }
  }

  /** A classifier answering each yes/no with the next of `yes`, counting its calls; or, with
    * none, unavailable.
    */
  final class Gate(yes: Option[Vector[Double]]) extends Classifier {
    // A count, read only by the test that owns the gate.
    @caps.unsafe.untrackedCaptures
    var calls = 0

    // Every state it was asked about, as sent; read only by the test that owns the gate.
    @caps.unsafe.untrackedCaptures
    var states = Vector.empty[ujson.Value]

    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] = {
      calls += 1
      states = states :+ state
      yes match {
        case None => Left(ClassifierError.Unavailable("no classifier"))
        case Some(ps) =>
          Right(
            Answers(
              ps.take(questions.size).map(Answer.YesNo(_)),
              Usage(Tokens(1), Tokens(1), Tokens.Zero, None),
              "jev"
            )
          )
      }
    }
  }

  /** Writes straight through to the in-memory stores, never rolled back. */
  final class FakeJot extends Jot {
    def write[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = body(using
      TestTx.fake
    )
  }

  object FakeDb extends Db {
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = body(using
      TestTx.fake
    )
  }

  /** One conversation's stores. */
  final class World {
    val entries = new InMemoryEntryStore
    val periods = new InMemoryPeriodStore(entries)
    val lifecycle = new InMemoryLifecycleStore
    val principals = new InMemoryPrincipals
    val ledger = new InMemoryUsageLedger
    val tombstones = new InMemoryTombstones
    val triage = new InMemoryTriageStore(entries, periods)

    /** What triage made of the message `text` heard ([[hear]]). */
    def tag(text: String, tags: Tags): Unit = {
      val _ = triage.record(EntryId(s"heard:$text"), tags, Start)(using TestTx.fake)
    }

    /** A user message starting the next turn at `minutes`, its period opened as the inbox
      * does.
      */
    def say(text: String, minutes: Long): TurnRef = {
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
      TurnRef(c, next.turnSeq)
    }

    /** A message `text` by `name`, heard where grit listens, as the next turn at `minutes`, its
      * period opened as the inbox does.
      */
    def hear(text: String, name: String, minutes: Long): TurnRef = {
      given Tx = TestTx.fake
      val next = entries.lockNext(c).getOrElse(sys.error("in-memory"))
      periods.openFor(c, next.turnSeq, at(minutes))
      val id = EntryId(s"heard:$text")
      entries.insert(Entry(id, c, next.turnSeq, None, next.seq, Payload.Heard(text), at(minutes)))
      val by = PrincipalId(s"test:$name")
      principals.enroll(by, name)
      principals.authored(id, by)
      TurnRef(c, next.turnSeq)
    }

    /** `payload` added to `turn` at `minutes`. */
    def add(turn: TurnRef, payload: Payload, minutes: Long, id: String): Unit = {
      given Tx = TestTx.fake
      val next = entries.lockNext(c).getOrElse(sys.error("in-memory"))
      val _ =
        entries.insert(Entry(EntryId(id), c, turn.turnSeq, None, next.seq, payload, at(minutes)))
    }

    /** A turn of `question`, its reply and its summary, all at `minutes`. */
    def turn(question: String, answer: String, summary: String, minutes: Long): TurnRef = {
      val t = say(question, minutes)
      add(t, Payload.Message(replyOf(answer)), minutes, s"reply:$question")
      add(t, Payload.Summary(summary), minutes, s"summary:$question")
      t
    }

    /** The attempt a sweep would make on period 1 as it stands, under the settings in force. */
    def attempt: CloseRef = attemptOn(p1)

    /** The attempt a sweep would make on `period` as it stands, under the settings in force. */
    def attemptOn(period: PeriodRef): CloseRef = {
      given Tx = TestTx.fake
      (for {
        settings <- lifecycle.current()
        open <- periods.activity(period)
      } yield open.map(_.attempt(settings))).toOption.flatten
        .getOrElse(sys.error(s"$period is not open"))
    }

    def all: Vector[Entry] = entries.list(c)(using TestTx.fake).getOrElse(Vector.empty)

    def closingEntry: Option[Entry] = closingOf(p1)

    def closingOf(period: PeriodRef): Option[Entry] =
      entries.get(period.closingId)(using TestTx.fake).getOrElse(None)

    /** The close's body over this world, with `gate`, `summary` and `clock`, its periods
      * `sealing`.
      */
    def body(gate: Classifier^, summary: Provider^, clock: Clock^, sealing: PeriodStore = periods)(
        id: WorkflowId
    )(using Durable^): String =
      bodyOver(gate, new OneModel(summary), clock, sealing)(id)

    /** The close's body over this world, its models `models`. */
    def bodyOver(gate: Classifier^, models: Models^, clock: Clock^, sealing: PeriodStore = periods)(
        id: WorkflowId
    )(using Durable^): String =
      Close.body(
        CloseEnv(
          CloseRecords(entries, sealing, lifecycle, ledger, tombstones, Chars, principals, triage),
          gate,
          models,
          FakeDb,
          clock
        )
      )(id)
  }

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

    def openFor(conversation: ConversationId, turn: TurnSeq, at: Instant)(using
        Tx^
    ): Either[StoreError, Period] =
      underlying.openFor(conversation, turn, at)
    def get(period: PeriodRef)(using Tx^): Either[StoreError, Option[Period]] =
      underlying.get(period)
    def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Period]] = underlying.of(turn)
    def judged(period: PeriodRef, verdict: Verdict)(using Tx^): Either[StoreError, Boolean] =
      underlying.judged(period, verdict)
    def open()(using Tx^): Either[StoreError, Vector[Activity]] = underlying.open()
    def activity(period: PeriodRef)(using Tx^): Either[StoreError, Option[Activity]] =
      underlying.activity(period)
    def closingBefore(turn: TurnRef)(using Tx^): Either[StoreError, Option[ClosingEntry]] =
      underlying.closingBefore(turn)
    def openElsewhere(conversation: ConversationId)(using
        Tx^
    ): Either[StoreError, Vector[OpenPeriod]] =
      underlying.openElsewhere(conversation)
    def closedElsewhere(conversation: ConversationId)(using
        Tx^
    ): Either[StoreError, Vector[ClosedElsewhere]] =
      underlying.closedElsewhere(conversation)
    def closedAfter(after: CloseOrdinal, n: Int)(using
        Tx^
    ): Either[StoreError, Vector[ClosedPeriod]] =
      underlying.closedAfter(after, n)
    def purge(period: PeriodRef, at: Instant)(using Tx^): Either[StoreError, Unit] =
      underlying.purge(period, at)
    def drop(period: PeriodRef)(using Tx^): Either[StoreError, Boolean] = underlying.drop(period)
    def all(conversation: ConversationId)(using Tx^): Either[StoreError, Vector[Period]] =
      underlying.all(conversation)
  }

  /** Turn numbers. */
  def turn(n: Long): TurnSeq = TurnSeq(n)
}
