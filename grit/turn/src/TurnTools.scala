package grit.turn

import java.time.Instant

import grit.core.approval.Approval
import grit.core.id.{EntryId, ToolCallId, TurnRef, WorkflowId}
import grit.core.message.Message
import grit.core.store.{Entry, EntryStore, Jot, Payload, StoreError, Tx}
import grit.core.tool.{Bound, Outcome, ToolName, Toolbox}

import TurnLoop.{Pending, Round}

/** How a turn settles one tool call, inside the call's own step. A step cut short by a crash
  * runs again, so the call's result is kept in the store under an id fixed by its place in
  * the loop ([[Slot.resultId]]), never in the journal, and a rerun that finds it returns it
  * without running anything. A tool that only reads runs again when a crash came between
  * its run and its result. One a person approves runs at most once ([[Settling.decide]]).
  */
object TurnTools {

  /** Where a call's settling left it: the entry holding its result, and whether that result
    * is an error.
    */
  final case class Settled(result: EntryId, failed: Boolean)

  /** A tool call's place in its turn's loop: the call at `index` (from 0) of the reply to
    * `round` in `turn`. Every id and step name its settling uses comes from it.
    */
  final case class Slot(turn: TurnRef, round: Round, index: Int) {

    /** The `tool:n:j` step that settles the call. */
    def step: String = Turn.Step.tool(round, index)

    /** The id of the entry keeping the call's result. */
    def resultId: EntryId = EntryId(s"result:$key")

    /** The id of the entry recording that the call, a gated one, began. */
    def attemptId: EntryId = EntryId(s"attempt:$key")

    private def key: String = s"${WorkflowId.value(turn.workflowId)}:${round.index}:$index"
  }

  /** The id of the entry keeping `turn`'s reply to `round`, which called tools. */
  def callId(turn: TurnRef, round: Round): EntryId =
    EntryId(s"call:${WorkflowId.value(turn.workflowId)}:${round.index}")

  /** What a gated call is answered when no approval can be asked for in this turn. */
  def notOffered(tool: ToolName): Outcome =
    Outcome.Failed(
      s"`${ToolName.value(tool)}` needs a person's approval, which this turn cannot ask for; " +
        "it was not run."
    )

  /** `pending` read against `tools`, ready to run; or the outcome it is answered with,
    * unrun: a refused call's, or the [[grit.core.tool.CallError]] of one that does not bind.
    */
  def read[C^](tools: Toolbox[C], pending: Pending): Either[Outcome, Bound^{C}] =
    pending match {
      case Pending.Refused(_, refused) => Left(refused)
      case Pending.Run(call) => tools.bind(call).left.map(_.outcome)
    }

  /** Where a turn keeps what its tool calls came to: in `entries`, written through `jot` from
    * inside each call's step. Each method settles the call at a [[Slot]], whose id is `call`,
    * and keeps its result as a [[Payload.Exchange]] entry at [[Slot.resultId]], dated `at`,
    * after everything in the conversation. A result already kept is returned as it is, and
    * nothing runs. Each fails only when `jot` cannot read or write the store.
    */
  final class Settling(jot: Jot^, entries: EntryStore) {

    /** The call answered with `outcome`, unrun. */
    def answer(
        slot: Slot,
        call: ToolCallId,
        outcome: Outcome,
        at: Instant
    ): Either[TurnFailure, Settled] =
      settle(slot, call, at)(() => Right(outcome))

    /** `free` run. */
    def run(
        slot: Slot,
        call: ToolCallId,
        free: Bound.Free^,
        at: Instant
    ): Either[TurnFailure, Settled] =
      settle(slot, call, at)(() => Right(free()))

    /** `gated` answered by `approval`. Run when approved, at most once: its attempt is
      * recorded at [[Slot.attemptId]] before it starts, and a rerun that finds the attempt
      * and no result answers [[Outcome.Interrupted]] without running it. Denied otherwise.
      */
    def decide(
        slot: Slot,
        call: ToolCallId,
        gated: Bound.Gated^,
        approval: Approval,
        at: Instant
    ): Either[TurnFailure, Settled] =
      settle(slot, call, at)(() =>
        approval match {
          case Approval.Approved =>
            began(slot, Payload.Attempt(call), at).map {
              case true => Outcome.Interrupted
              case false => gated(Approval.Approved)
            }
          case other => Right(gated(other))
        }
      )

    private def settle(slot: Slot, call: ToolCallId, at: Instant)(
        outcome: () => Either[TurnFailure, Outcome]
    ): Either[TurnFailure, Settled] =
      jot.write(entries.get(slot.resultId)).left.map(storeFailure).flatMap {
        case Some(kept) => settledBy(kept)
        case None =>
          outcome().flatMap { o =>
            keep(slot.turn, slot.resultId, Payload.Exchange(o.result(call)), at)
              .flatMap(settledBy)
          }
      }

    /** Whether `slot`'s attempt was already recorded; if not, it is, as `marker`, before
      * this returns.
      */
    private def began(slot: Slot, marker: Payload, at: Instant): Either[TurnFailure, Boolean] = {
      val (turn, id) = (slot.turn, slot.attemptId)
      def record(using Tx^): Either[StoreError, Boolean] =
        entries.get(id).flatMap {
          case Some(_) => Right(true)
          case None =>
            for {
              next <- entries.lockNext(turn.conversationId)
              _ <- entries.insert(
                Entry(id, turn.conversationId, turn.turnSeq, None, next.seq, marker, at)
              )
            } yield false
        }
      jot.write(record) match {
        case Left(StoreError.DuplicateId(_)) => Right(true)
        case other => other.left.map(storeFailure)
      }
    }

    /** `payload` kept as `turn`'s entry `id`, dated `at`, after everything in the
      * conversation; the entry already there when one has that id.
      */
    private def keep(
        turn: TurnRef,
        id: EntryId,
        payload: Payload,
        at: Instant
    ): Either[TurnFailure, Entry] = {
      def write(using Tx^): Either[StoreError, Entry] =
        for {
          next <- entries.lockNext(turn.conversationId)
          entry = Entry(id, turn.conversationId, turn.turnSeq, None, next.seq, payload, at)
          _ <- entries.insert(entry)
        } yield entry
      jot.write(write) match {
        case Left(StoreError.DuplicateId(_)) =>
          jot
            .write(entries.get(id))
            .left
            .map(storeFailure)
            .flatMap(_.toRight(TurnFailure.Store(s"entry ${EntryId.value(id)} vanished")))
        case other => other.left.map(storeFailure)
      }
    }
  }

  private def settledBy(kept: Entry): Either[TurnFailure, Settled] = kept.payload match {
    case Payload.Exchange(Message.ToolResult(_, _, failed)) => Right(Settled(kept.id, failed))
    case _ => Left(TurnFailure.Store(s"entry ${EntryId.value(kept.id)} is not a tool result"))
  }

  private def storeFailure(error: StoreError): TurnFailure = error match {
    case StoreError.DuplicateId(id) =>
      TurnFailure.Store(s"entry ${EntryId.value(id)} already exists")
    case StoreError.DatabaseError(cause) => TurnFailure.Store(cause)
  }
}
