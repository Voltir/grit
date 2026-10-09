package grit.act.phase

import java.time.{Instant, ZoneOffset}

import grit.core.act.Allowance
import grit.core.clock.SetClock
import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{Delta, ModelRequest, Provider, ProviderError}
import grit.core.spend.{Budget, DailyCap, Day, Spend, Spending}
import grit.core.store.{InMemoryUsageLedger, StoreError, Tx, UsageLedger}
import grit.dbos.sql.TestTx

import utest.*

/** An ask's phases: the reply and its retries, whether an allowance admits it, and its cost. */
object AskingTests extends TestSuite {

  private val turn = TurnRef(ConversationId("c"), TurnSeq.First)
  private val start = Instant.parse("2026-10-08T12:00:00Z")
  private val request = ModelRequest("system", Vector.empty)

  private def answer(text: String, usd: String = "0.01"): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)),
      StopReason.EndTurn,
      Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal(usd))),
      "m"
    )

  /** A provider that answers each call with the next of `script`, the last again after. */
  private final class Scripted(script: Vector[Either[ProviderError, Message.Assistant]])
      extends Provider {
    @caps.unsafe.untrackedCaptures
    var calls = 0
    def complete(r: ModelRequest): Either[ProviderError, Message.Assistant] = {
      val i = calls
      calls += 1
      script.lift(i).orElse(script.lastOption).getOrElse(Left(ProviderError.Refused("none")))
    }
  }

  /** A hearing that keeps what each attempt, by its index, was told, and which were done. */
  private final class Kept extends Hearing {
    @caps.unsafe.untrackedCaptures
    var told = Vector.empty[(Int, Delta)]
    @caps.unsafe.untrackedCaptures
    var ended = Vector.empty[Int]
    @caps.unsafe.untrackedCaptures
    private var made = 0
    def attempt(): Heard^ = {
      val i = made
      made += 1
      new Heard {
        def tell(delta: Delta): Unit = told = told :+ (i -> delta)
        def done(): Unit = ended = ended :+ i
      }
    }
  }

  private val down = Left(ProviderError.Unavailable("upstream down"))

  private def budget(cap: String): Budget =
    Budget(ZoneOffset.UTC, DailyCap.of(cap).toOption)

  /** Spending that cannot be read: an allowance that reads it fails. */
  private val unreadable: Spending = new Spending {
    def on(day: Day)(using Tx^): Either[StoreError, Spend] = Left(StoreError.DatabaseError("x"))
    def conversation(id: ConversationId)(using Tx^): Either[StoreError, Spend] =
      Left(StoreError.DatabaseError("x"))
  }

  val tests = Tests {
    test("an unavailable provider is asked again after 2 s and 6 s, then fails naming its tries") {
      val provider = new Scripted(Vector(down))
      val clock = new SetClock(start)
      val got = Asking.reply(provider, request, Hearing.silent(), clock)
      (got, provider.calls, clock.at) ==>
        (Left("upstream down (after 3 tries)"), 3, start.plusSeconds(8))
    }

    test("a provider unavailable once answers on its second try, each try heard afresh") {
      val provider = new Scripted(Vector(down, Right(answer("hi"))))
      val clock = new SetClock(start)
      val hearing = new Kept
      val got = Asking.reply(provider, request, hearing, clock)
      (got, hearing.told, hearing.ended, clock.at) ==>
        (Right(answer("hi")), Vector(1 -> Delta.Text("hi")), Vector(0, 1), start.plusSeconds(2))
    }

    test("a refused provider is not asked again, and its cause names no tries") {
      val provider = new Scripted(Vector(Left(ProviderError.Refused("bad request"))))
      val got = Asking.reply(provider, request, Hearing.silent(), new SetClock(start))
      (got, provider.calls) ==> (Left("bad request"), 1)
    }

    test("a daily allowance admits an ask while the day's spend is under its cap, then refuses") {
      val ledger = new InMemoryUsageLedger
      given Tx = TestTx.fake
      ledger.now = start
      val daily = Allowance.Daily(budget("0.02"))
      val before = Asking.admits(daily, ledger, start)
      val _ = ledger.record(EntryId("a"), turn, turn.workflowId, "m", answer("a").usage, Tokens(1))
      val under = Asking.admits(daily, ledger, start)
      val _ = ledger.record(EntryId("b"), turn, turn.workflowId, "m", answer("b").usage, Tokens(1))
      val at = Asking.admits(daily, ledger, start)
      val tomorrow = Asking.admits(daily, ledger, start.plus(1, java.time.temporal.ChronoUnit.DAYS))
      (before, under, at, tomorrow) ==> (Right(true), Right(true), Right(false), Right(true))
    }

    test("an admitted allowance admits every ask without reading what was spent") {
      given Tx = TestTx.fake
      Asking.admits(Allowance.Admitted, unreadable, start) ==> Right(true)
      Asking.admits(Allowance.Daily(budget("1")), unreadable, start) ==>
        Left(StoreError.DatabaseError("x"))
    }

    test("an ask's cost is recorded once, as its reply's beside the estimate; again, DuplicateId") {
      val ledger = new InMemoryUsageLedger
      given Tx = TestTx.fake
      val reply = answer("hi")
      val first = Asking.spent(ledger, EntryId("e"), turn, reply, Tokens(7))
      val second = Asking.spent(ledger, EntryId("e"), turn, reply, Tokens(7))
      (first, second, ledger.of(turn.workflowId)) ==>
        (
          Right(()),
          Left(StoreError.DuplicateId(EntryId("e"))),
          Right(Vector(UsageLedger.Row(EntryId("e"), "m", reply.usage, Tokens(7))))
        )
    }

    test(
      "a call's cost is recorded once, as its model's and usage beside the estimate; again, DuplicateId"
    ) {
      val ledger = new InMemoryUsageLedger
      given Tx = TestTx.fake
      val usage = Usage(Tokens(30), Tokens(4), Tokens(10), Some(BigDecimal("0.002")))
      val first = Asking.cost(ledger, EntryId("e"), turn, "judge", usage, Tokens(28))
      val second = Asking.cost(ledger, EntryId("e"), turn, "judge", usage, Tokens(28))
      (first, second, ledger.of(turn.workflowId)) ==>
        (
          Right(()),
          Left(StoreError.DuplicateId(EntryId("e"))),
          Right(Vector(UsageLedger.Row(EntryId("e"), "judge", usage, Tokens(28))))
        )
    }
  }
}
