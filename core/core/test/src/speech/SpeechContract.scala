package grit.core.speech

import java.time.Instant

import grit.core.id.{CloseRef, ConversationId, EntryId, PrincipalId, TurnRef}
import grit.core.message.{AssistantBlock, Cost, Message, StopReason, Tokens, Usage}
import grit.core.period.{CloseReason, Probability, TestClosings}
import grit.core.place.{Namespace, Place}
import grit.core.spend.{Day, Spend}
import grit.core.store.{Entry, EntryStore, Payload, PeriodStore, StoreError, Tx, UsageLedger}
import grit.core.triage.{Kind, Tags}

import utest.*

/** The contract every [[SpeechStore]] keeps, run against the in-memory fake in core and the
  * SQL store in grit.dbos. Tests share the store's database, so each names its own
  * conversation, and reads of the whole ledger are filtered to their own turns.
  */
abstract class SpeechContract extends TestSuite {

  /** The entry store the heard messages and replies are in. */
  protected def entries: EntryStore

  /** The periods of those entries, whose purge deletes them. */
  protected def periods: PeriodStore

  /** The usage ledger the unprompted turns' calls are recorded in. */
  protected def ledger: UsageLedger

  /** The speech store under test, over [[entries]] and [[ledger]]. */
  protected def speech: SpeechStore

  protected def transaction[A](body: (Tx^) ?=> A): A

  /** A conversation entries may be written to, the same one for the same `name`. */
  protected def conversation(name: String): ConversationId

  /** The day a row [[ledger]] records now falls on. */
  protected def today: Day

  private val At = Instant.parse("2026-09-30T10:00:00Z")
  private val room = Place.under(Namespace.Slack, Vector("T", "C"))

  private def right[A](result: Either[StoreError, A]): A =
    result.fold(e => throw new java.lang.AssertionError(s"store failed: $e"), identity)

  private def p(x: Double) = Probability.clamped(x)

  private val usage = Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.001")))

  private val reply: Message.Assistant =
    Message.Assistant(Vector(AssistantBlock.Text("it moved")), StopReason.EndTurn, usage, "m")

  /** An entry of `c`'s next turn holding `payload`: that turn. */
  private def next(c: ConversationId, payload: Payload, id: String): TurnRef = transaction {
    val n = right(entries.lockNext(c))
    right(entries.insert(Entry(EntryId(id), c, n.turnSeq, None, n.seq, payload, At)))
    TurnRef(c, n.turnSeq)
  }

  private def heardAs(turn: TurnRef): Heard =
    Heard(
      turn,
      0,
      room,
      At,
      Reach(Some("C/1"), Set.empty),
      Tags.Weighed(Kind.Question, p(0.9), p(0.5), p(0.5), p(0.8), "jev", usage)
    )

  /** A heard message as `c`'s next turn, decided on at `at` as `decision` does to it. */
  private def decide(c: ConversationId, at: Instant, drafting: Boolean): TurnRef = {
    val turn = next(c, Payload.Heard("is it Thursday?"), s"${ConversationId.value(c)}:h:$at")
    val decision =
      if (drafting) Decision.Drafting(turn) else Decision.Held(Silence.Chatter)
    transaction(speech.decided(heardAs(turn), decision, at)) ==> Right(true)
    turn
  }

  private def mine(c: ConversationId, since: Instant): Vector[Spoken] =
    transaction(right(speech.spoken(since))).filter(_.turn.conversationId == c)

  private val judged = Judged(p(0.6), p(0.8), "jev", usage)

  val tests = Tests {
    test("a heard message's reach is kept, the first one standing; none for another turn") {
      val c = conversation("speech-reach")
      val turn = next(c, Payload.Heard("hi"), "speech-reach:h")
      val first = Reach(Some("C/1"), Set(PrincipalId("slack:T/U1")))
      transaction(speech.heard(turn, first)) ==> Right(())
      transaction(speech.heard(turn, Reach(None, Set.empty))) ==> Right(())
      transaction(speech.reach(turn)) ==> Right(Some(first))
      val said = next(c, Payload.Message(Message.User("hi")), "speech-reach:u")
      transaction(speech.reach(said)) ==> Right(None)
      transaction(speech.heard(said, first)) match {
        case Left(StoreError.Invalid(_)) => ()
        case other => throw new java.lang.AssertionError(s"not refused: $other")
      }
    }

    test("a heard message's reach goes with its entry: none once its period is purged") {
      val c = conversation("speech-reach-purged")
      val turn = transaction {
        val n = right(entries.lockNext(c))
        right(periods.openFor(c, n.turnSeq, At))
        right(
          entries.insert(
            Entry(
              EntryId("speech-reach-purged:h"),
              c,
              n.turnSeq,
              None,
              n.seq,
              Payload.Heard("hi"),
              At
            )
          )
        )
        TurnRef(c, n.turnSeq)
      }
      val reach = Reach(Some("C/1"), Set.empty)
      transaction(speech.heard(turn, reach)) ==> Right(())
      transaction(speech.reach(turn)) ==> Right(Some(reach))
      val period = right(transaction(periods.of(turn)))
        .map(_.ref)
        .getOrElse(throw new java.lang.AssertionError("no period"))
      transaction {
        for {
          _ <- periods.seal(
            CloseRef(period, turn.turnSeq, At),
            CloseReason.Lapsed,
            TestClosings.prose("heard", None),
            At
          )
          _ <- periods.purge(period, At)
        } yield ()
      } ==> Right(())
      transaction(speech.reach(turn)) ==> Right(None)
    }

    test("a decision is kept once; only drafting ones are spoken, from a time on, oldest first") {
      val c = conversation("speech-decided")
      // Decided out of time order, so oldest first is not the order they were kept in.
      val late = decide(c, At.plusSeconds(120), drafting = true)
      val _ = decide(c, At.plusSeconds(60), drafting = false)
      val early = decide(c, At, drafting = true)
      transaction(speech.decided(heardAs(early), Decision.Held(Silence.Off), At)) ==> Right(false)
      mine(c, At) ==> Vector(
        Spoken(early, room, At, Stage.Drafting),
        Spoken(late, room, At.plusSeconds(120), Stage.Drafting)
      )
      mine(c, At.plusSeconds(61)) ==> Vector(
        Spoken(late, room, At.plusSeconds(120), Stage.Drafting)
      )
    }

    test("a posted outcome keeps its reply's position; any other settles; each kept once") {
      val c = conversation("speech-drafted")
      val posted = decide(c, At, drafting = true)
      val below = decide(c, At.plusSeconds(1), drafting = true)
      val seq = transaction {
        val n = right(entries.lockNext(c))
        right(
          entries.insert(
            Entry(posted.replyId, c, posted.turnSeq, None, n.seq, Payload.Message(reply), At)
          )
        )
        n.seq
      }
      transaction(speech.drafted(posted, Outcome.Posted(judged), Some("it moved"), At)) ==> Right(
        true
      )
      transaction(speech.drafted(below, Outcome.Below(judged, p(0.9)), Some("x"), At)) ==> Right(
        true
      )
      transaction(speech.drafted(below, Outcome.Passed, None, At)) ==> Right(false)
      mine(c, At).map(_.stage) ==> Vector(Stage.Posted(seq), Stage.Settled)
    }

    test("an outcome for a turn never decided on, or posted with no reply, is refused") {
      val c = conversation("speech-refused")
      val never = next(c, Payload.Heard("hi"), "speech-refused:h")
      val drafting = decide(c, At, drafting = true)
      Vector(
        transaction(speech.drafted(never, Outcome.Passed, None, At)),
        transaction(speech.drafted(drafting, Outcome.Posted(judged), None, At))
      ).map {
        case Left(StoreError.Invalid(_)) => "invalid"
        case other => other.toString
      } ==> Vector("invalid", "invalid")
      mine(c, At).map(_.stage) ==> Vector(Stage.Drafting)
    }

    test(
      "forget deletes a conversation's decisions on turns from..to, both included, and no other"
    ) {
      val c = conversation("speech-forget")
      val other = conversation("speech-forget-other")
      val turns = (0 to 2).toVector.map(i => decide(c, At.plusSeconds(i.toLong), drafting = true))
      val kept = decide(other, At, drafting = true)
      transaction(speech.forget(c, turns(0).turnSeq, turns(1).turnSeq)) ==> Right(())
      (mine(c, At).map(_.turn), mine(other, At).map(_.turn)) ==> (Vector(turns(2)), Vector(kept))
    }

    test(
      "a day's speech spend with a call recorded unpriced is at least what the priced ones cost"
    ) {
      val c = conversation("speech-unpriced")
      val drafting = decide(c, At, drafting = true)
      val before = transaction(right(speech.spentOn(today)))
      transaction {
        right(
          ledger.record(drafting.draftId, drafting, drafting.workflowId, "m", usage, Tokens(10))
        )
        right(
          ledger.record(
            EntryId(s"judge:${drafting.workflowId}"),
            drafting,
            drafting.workflowId,
            "jev",
            usage.copy(costUsd = None),
            Tokens(10)
          )
        )
      }
      transaction(speech.spentOn(today)) ==>
        Right(before + Spend(2, Cost.AtLeast(BigDecimal("0.001"))))
    }

    test("the day's speech spend: the drafting turns' recorded calls, and no other turn's") {
      val c = conversation("speech-spent")
      val drafting = decide(c, At, drafting = true)
      val held = decide(c, At.plusSeconds(1), drafting = false)
      val before = transaction(right(speech.spentOn(today)))
      transaction {
        right(
          ledger.record(drafting.draftId, drafting, drafting.workflowId, "m", usage, Tokens(10))
        )
        right(
          ledger.record(
            EntryId(s"judge:${drafting.workflowId}"),
            drafting,
            drafting.workflowId,
            "jev",
            usage,
            Tokens(10)
          )
        )
        right(ledger.record(held.replyId, held, held.workflowId, "m", usage, Tokens(10)))
      }
      transaction(speech.spentOn(today)) ==>
        Right(before + Spend(2, Cost.Exact(BigDecimal("0.002"))))
    }
  }
}
