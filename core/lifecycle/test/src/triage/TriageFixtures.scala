package grit.lifecycle.triage

import java.time.{Instant, ZoneOffset}

import scala.concurrent.duration.FiniteDuration

import grit.core.classify.{Answer, Answers, Classifier, ClassifierError, Question, Request}
import grit.core.clock.Clock
import grit.core.durable.{Durable, InMemoryDurable}
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
import grit.core.recipe.InMemoryRoomReads
import grit.core.speech.{InMemorySpeechStore, Reach, Speaking}
import grit.core.spend.Budget
import grit.core.stitch.{InMemoryStitchStore, Opening, Placements, StitchReads, Tuning}
import grit.core.store.{
  Db,
  Entry,
  EntrySearch,
  InMemoryConversationStore,
  InMemoryEntryStore,
  InMemoryLifecycleStore,
  InMemoryPeriodStore,
  InMemoryPrincipals,
  InMemoryUsageLedger,
  OpenPeriod,
  Origin,
  Payload,
  StoreError,
  Tx
}
import grit.core.triage.{Corpora, InMemoryTriageStore}
import grit.dbos.sql.TestTx
import grit.lifecycle.stitch.{Stitch, StitchEnv}

/** The triage's test world: one conversation in core's in-memory stores, and a classifier the
  * test scripts.
  */
object TriageFixtures {

  val c: ConversationId = ConversationId("c1")

  val p1: PeriodRef = PeriodRef(c, PeriodSeq.First)

  val Start: Instant = Instant.parse("2026-09-28T10:00:00Z")

  def at(minutes: Long): Instant = Start.plusSeconds(minutes * 60)

  /** The turn whose heard message `triage` asks about. */
  extension (triage: TriageRef) {
    def message: TurnRef = TurnRef(triage.period.conversationId, triage.turn)
  }

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

  /** The requests a classifier was sent, kept by [[recording]]. */
  final class Requests {
    @caps.unsafe.untrackedCaptures
    var sent: Vector[Request] = Vector.empty
  }

  /** A classifier that keeps each request it is sent in `into`, and is unavailable. */
  def recording(into: Requests): Classifier^ = {
    val none = Classifier.none("recording")
    Classifier.around(none) { (request, ask) =>
      into.sent = into.sent :+ request
      ask()
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

  /** A search that finds nothing. */
  final class NoSearch extends EntrySearch {
    def search(
        conversation: ConversationId,
        from: grit.core.id.TurnSeq,
        before: grit.core.id.TurnSeq,
        query: String,
        limit: Int
    )(using Tx^): Either[StoreError, Vector[EntrySearch.Hit]] = Right(Vector.empty)

    def nearby(open: Vector[OpenPeriod], query: String, limit: Int)(using
        Tx^
    ): Either[StoreError, Vector[EntrySearch.Hit]] = Right(Vector.empty)

    def closings(conversations: Vector[ConversationId], query: String, limit: Int)(using
        Tx^
    ): Either[StoreError, Vector[EntrySearch.Hit]] = Right(Vector.empty)

    def room(
        room: grit.core.place.Place,
        from: Instant,
        until: Instant,
        query: String,
        limit: Int
    )(using Tx^): Either[StoreError, Vector[EntrySearch.Hit]] = Right(Vector.empty)
  }

  /** A channel's stores: conversation c1, a Slack thread, and any others [[World.thread]]
    * begins in the same channel.
    */
  final class World {
    val entries = new InMemoryEntryStore
    val conversations = new InMemoryConversationStore
    private def originOf(id: ConversationId): Origin =
      conversations
        .get(id)(using TestTx.fake)
        .toOption
        .flatten
        .fold(Origin.Task("unknown", ConversationId.value(id)))(_.origin)
    val periods = new InMemoryPeriodStore(entries, originOf)
    val principals = new InMemoryPrincipals
    val triage = new InMemoryTriageStore(entries, periods)
    val ledger = new InMemoryUsageLedger
    val speech = new InMemorySpeechStore(entries, ledger)
    val acknowledgements = new grit.core.edge.InMemoryAcknowledgements
    val deliveries = new grit.core.edge.InMemoryDeliveries
    val stitches = new InMemoryStitchStore(entries, originOf)
    val search = new NoSearch
    val lifecycle = new InMemoryLifecycleStore
    val rooms = new InMemoryRoomReads(entries, originOf, principals)

    // The conversation is c1: the first a fresh store creates.
    conversations.findOrCreate(Origin.Slack("T", "C", "1.0"), PrincipalId.Local)(using TestTx.fake)

    /** Another thread of channel C, rooted at `ts`. */
    def thread(ts: String): ConversationId =
      conversations
        .findOrCreate(Origin.Slack("T", "C", ts), PrincipalId.Local)(using TestTx.fake)
        .fold(e => sys.error(e.toString), _.id)

    /** The turns started, in order. */
    @caps.unsafe.untrackedCaptures
    var started = Vector.empty[TurnRef]

    private def insert(
        payload: Payload,
        by: Option[(String, String)],
        minutes: Long,
        in: ConversationId = c
    ): Entry = {
      given Tx = TestTx.fake
      val next = entries.lockNext(in).getOrElse(sys.error("in-memory"))
      periods.openFor(in, next.turnSeq, at(minutes))
      val id = if (in == c) s"e${next.seq}" else s"${ConversationId.value(in)}:e${next.seq}"
      val e = Entry(EntryId(id), in, next.turnSeq, None, next.seq, payload, at(minutes))
      entries.insert(e)
      by.foreach { (id, name) =>
        principals.enroll(PrincipalId(id), name)
        principals.authored(e.id, PrincipalId(id))
      }
      e
    }

    /** `text` heard from `name` as the next turn at `minutes`, with a reply address; its
      * triage.
      */
    def hear(text: String, name: String, minutes: Long, in: ConversationId = c): TriageRef = {
      val e = insert(Payload.Heard(text), Some(s"u-$name" -> name), minutes, in)
      speech.heard(TurnRef(in, e.turnSeq), Reach(Some("C/1.0"), Set.empty))(using TestTx.fake)
      TriageRef(PeriodRef(in, PeriodSeq.First), e.turnSeq)
    }

    /** `text` said to grit as the next turn at `minutes`, by `name` when given; its turn. */
    def say(text: String, minutes: Long, name: Option[String] = None): TurnRef = {
      val e = insert(Payload.Message(Message.User(text)), name.map(n => s"u-$n" -> n), minutes)
      TurnRef(c, e.turnSeq)
    }

    /** Period 1 closed after `last` and its raw entries purged: every entry gone. */
    def purge(last: TurnRef): Unit = {
      given Tx = TestTx.fake
      periods.seal(
        CloseRef(p1, last.turnSeq, at(60)),
        CloseReason.Lapsed,
        TestClosings.prose("x"),
        at(60)
      )
      periods.purge(p1, at(61))
      ()
    }

    /** Where a triage's stitch and question read this world. */
    def reads: StitchReads =
      StitchReads(entries, conversations, lifecycle, stitches, search, principals)

    def tags(triage: TriageRef) =
      entries
        .list(c)(using TestTx.fake)
        .getOrElse(Vector.empty)
        .find(_.turnSeq == triage.turn)
        .flatMap(e => this.triage.of(Vector(e.id))(using TestTx.fake).toOption.flatMap(_.get(e.id)))

    /** An opening's placement over this world, asking `classifier`, at `minutes`. */
    def placement(classifier: Classifier^, minutes: Long)(id: WorkflowId)(using Durable^): String =
      Stitch.body(StitchEnv(reads, classifier, FakeDb, new Stopped(at(minutes)), Tuning.Default))(
        id
      )

    /** The openings whose placements a triage waited for, in order. */
    @caps.unsafe.untrackedCaptures
    var awaited = Vector.empty[Opening]

    /** Placements as the engine makes them, each opening's placement run once over this world
      * (its own workflow, run to its end at once), asking `classifier` at `minutes`; each
      * opening waited for kept in [[awaited]].
      */
    def placements(classifier: Classifier^, minutes: Long): Placements^ = {
      val placing = new InMemoryDurable
      new Placements {
        def awaited(opening: Opening): Either[String, String] = {
          World.this.awaited = World.this.awaited :+ opening
          Right(placing.run(opening.ref.workflowId)(placement(classifier, minutes)))
        }
        def awaitedWithin(
            opening: Opening,
            within: FiniteDuration,
            clock: Clock^
        ): Either[grit.core.stitch.Placements.Unplaced, String] =
          awaited(opening).left.map(grit.core.stitch.Placements.Unplaced.Failed(_))
      }
    }

    /** The triage's body over this world, with `classifier`, at `minutes`, speaking as
      * `speaking` says, with no daily cap, the deployment's knowledge `sources` those given; a
      * turn it starts is kept in [[started]].
      */
    def body(
        classifier: Classifier^,
        minutes: Long,
        speaking: Speaking = Speaking.Off,
        sources: Corpora = Corpora.Empty
    )(
        id: WorkflowId
    )(using Durable^): String =
      Triage.body(
        TriageEnv(
          TriageRecords(
            entries,
            triage,
            principals,
            conversations,
            speech,
            ledger,
            acknowledgements,
            deliveries,
            stitches,
            search,
            lifecycle,
            rooms
          ),
          classifier,
          FakeDb,
          new Stopped(at(minutes)),
          TriageSpeech(
            speaking,
            Budget(ZoneOffset.UTC, None),
            turn => {
              started = started :+ turn
              Right(())
            }
          ),
          Tuning.Default,
          placements(classifier, minutes),
          sources,
          grit.lifecycle.triage.TriageQuestions.shipped(grit.core.persona.Persona.Grit)
        )
      )(id)
  }
}
