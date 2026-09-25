package grit.turn

import java.time.Instant

import grit.core.approval.Approval
import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.message.Message
import grit.core.store.{Entry, EntryStore, Jot, Payload, StoreError, Tx}
import grit.core.tool.{Bound, Outcome, ToolName, Toolbox}

import TurnLoop.{Pending, Round}

/** How a turn settles one tool call, inside the call's own step. A step cut short by a crash
  * runs again, so the call's result is kept in the store under an id fixed by its place in
  * the loop ([[resultId]]), never in the journal, and a rerun that finds it returns it
  * without running anything. A tool that only reads runs again when a crash came between
  * its run and its result. One a person approves runs at most once: an attempt is recorded
  * ([[attemptId]]) before it starts, and a rerun that finds the attempt and no result
  * answers [[Outcome.Interrupted]] without running it.
  */
object TurnTools {

  /** Where a call's settling left it: the entry holding its result, and whether that result
    * is an error.
    */
  final case class Settled(result: EntryId, failed: Boolean)

  /** The id of the entry keeping `turn`'s reply to `round`, which called tools. */
  def callId(turn: TurnRef, round: Round): EntryId =
    EntryId(s"call:${WorkflowId.value(turn.workflowId)}:${round.index}")

  /** The id of the entry keeping the result of the call at `index` of `round`'s reply. */
  def resultId(turn: TurnRef, round: Round, index: Int): EntryId =
    EntryId(s"result:${WorkflowId.value(turn.workflowId)}:${round.index}:$index")

  /** The id of the entry recording that the gated call at `index` of `round`'s reply began. */
  def attemptId(turn: TurnRef, round: Round, index: Int): EntryId =
    EntryId(s"attempt:${WorkflowId.value(turn.workflowId)}:${round.index}:$index")

  /** What a gated call is answered when no approval can be asked for in this turn. */
  def notOffered(tool: ToolName): Outcome =
    Outcome.Failed(
      s"`${ToolName.value(tool)}` needs a person's approval, which this turn cannot ask for; " +
        "it was not run."
    )

  /** Settles `pending`, the call at `index` of `round`'s reply in `turn`, against `tools`,
    * and keeps its result as an [[Payload.Exchange]] entry at [[resultId]], dated `at`, after
    * everything in the conversation. A result already kept is returned as it is, and nothing
    * runs. Otherwise: a refused call is answered with its outcome; one that does not bind,
    * with its [[grit.core.tool.CallError]]; a free call runs; a gated one runs only with
    * `approval` approving it, is denied by any other answer, and without one is answered
    * [[notOffered]]. Fails only when `jot` cannot read or write the store.
    */
  def settle[C^](
      jot: Jot^,
      entries: EntryStore,
      tools: Toolbox[C],
      approval: Option[Approval],
      turn: TurnRef,
      round: Round,
      index: Int,
      pending: Pending,
      at: Instant
  ): Either[TurnFailure, Settled] = {
    val id = resultId(turn, round, index)
    jot.write(entries.get(id)).left.map(storeFailure).flatMap {
      case Some(kept) => settledBy(kept)
      case None =>
        outcome(jot, entries, tools, approval, turn, round, index, pending, at).flatMap { o =>
          keep(jot, entries, turn, id, Payload.Exchange(o.result(pending.call.id)), at)
            .flatMap(settledBy)
        }
    }
  }

  private def outcome[C^](
      jot: Jot^,
      entries: EntryStore,
      tools: Toolbox[C],
      approval: Option[Approval],
      turn: TurnRef,
      round: Round,
      index: Int,
      pending: Pending,
      at: Instant
  ): Either[TurnFailure, Outcome] =
    pending match {
      case Pending.Refused(_, refused) => Right(refused)
      case Pending.Run(call) =>
        tools.bind(call) match {
          case Left(error) => Right(error.outcome)
          case Right(free: Bound.Free) => Right(free())
          case Right(gated: Bound.Gated) =>
            approval match {
              case None => Right(notOffered(gated.tool))
              case Some(Approval.Approved) =>
                val marker = Payload.Attempt(call.id)
                began(jot, entries, turn, attemptId(turn, round, index), marker, at).map {
                  case true => Outcome.Interrupted
                  case false => gated(Approval.Approved)
                }
              case Some(other) => Right(gated(other))
            }
        }
    }

  /** Whether an attempt at `id` was already recorded; if not, it is, before this returns. */
  private def began(
      jot: Jot^,
      entries: EntryStore,
      turn: TurnRef,
      id: EntryId,
      marker: Payload,
      at: Instant
  ): Either[TurnFailure, Boolean] = {
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
      jot: Jot^,
      entries: EntryStore,
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
