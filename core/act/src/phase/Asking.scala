package grit.act.phase

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.act.Allowance
import grit.core.clock.Clock
import grit.core.id.{EntryId, TurnRef}
import grit.core.message.{Message, Tokens, Usage}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.spend.Spending
import grit.core.store.{StoreError, Tx, UsageLedger}

/** An ask's phases: the model's reply, whether an allowance admits it, and its cost recorded. */
object Asking {

  /** How long a reply waits before each retry of a provider that was
    * [[ProviderError.Unavailable]]: 2 s, then 6 s; three tries in all. A pinned upstream has no
    * fallback.
    */
  val Retries: List[FiniteDuration] = List(2.seconds, 6.seconds)

  /** `request` sent to `provider`, its response told to `hearing` as it is generated. A
    * provider that is `Unavailable` is asked again after each of [[Retries]], waiting on
    * `clock`. The last failure, or a `Refused` one, is `Left`, its cause followed by
    * " (after n tries)" when there was more than one.
    */
  def reply(
      provider: Provider^,
      request: ModelRequest,
      hearing: Hearing^,
      clock: Clock^
  ): Either[String, Message.Assistant] = {
    def attempt(waits: List[FiniteDuration], tries: Int): Either[String, Message.Assistant] = {
      val heard = hearing.attempt()
      val result = provider.stream(request, heard.tell)
      heard.done()
      (result, waits) match {
        case (Left(ProviderError.Unavailable(_)), wait :: rest) =>
          clock.sleep(wait)
          attempt(rest, tries + 1)
        case (Left(error), _) =>
          val after = if (tries == 1) "" else s" (after $tries tries)"
          Left(error.cause + after)
        case (Right(reply), _) => Right(reply)
      }
    }
    attempt(Retries, 1)
  }

  /** Whether `allowance` admits an ask at `now`: always when `Admitted`, reading nothing; when
    * `Daily`, as its budget admits what `spending` recorded that day.
    */
  def admits(allowance: Allowance, spending: Spending, now: Instant)(using
      Tx^
  ): Either[StoreError, Boolean] =
    allowance match {
      case Allowance.Admitted => Right(true)
      case Allowance.Daily(budget) => spending.on(budget.today(now)).map(budget.admits)
    }

  /** The cost of `reply`, the response held by `entry` (an entry's id, or a move's), recorded
    * for `turn` (under its conversation and workflow) beside `estimate`, the request's
    * estimated input: as [[cost]], of `reply`'s model and usage. `DuplicateId` when `entry`'s
    * is recorded already.
    */
  def spent(
      ledger: UsageLedger,
      entry: EntryId,
      turn: TurnRef,
      reply: Message.Assistant,
      estimate: Tokens
  )(using Tx^): Either[StoreError, Unit] =
    cost(ledger, entry, turn, reply.model, reply.usage, estimate)

  /** As [[spent]], for any model's call: `usage` of the call made by `model`, recorded for
    * `turn` under `entry` beside `estimate`; the one way an ask's cost reaches the ledger.
    * `DuplicateId` when `entry`'s is recorded already.
    */
  def cost(
      ledger: UsageLedger,
      entry: EntryId,
      turn: TurnRef,
      model: String,
      usage: Usage,
      estimate: Tokens
  )(using Tx^): Either[StoreError, Unit] =
    ledger.record(entry, turn, turn.workflowId, model, usage, estimate)
}
