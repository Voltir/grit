package grit.turn

import java.time.Instant

import grit.core.context.{AssemblyError, AssemblyRequest, ContextAssembler, Window}
import grit.core.durable.Durable
import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.message.Message
import grit.core.provider.{ModelRequest, Provider, ProviderError, TokenEstimator}
import grit.core.store.{Db, Entry, EntryStore, Payload, StoreError, Tx, UsageLedger}

/** The durable turn: one workflow per turn, in three steps. Each step's output is
  * recorded, so a turn resumed after a crash never calls the model twice.
  *
  *   1. `assemble` — a fresh window over what came before the turn.
  *   2. `call-model` — the window, then the turn's own messages, sent to the provider.
  *   3. `append` — the reply recorded as the turn's entry, with its cost in the usage
  *      ledger beside `estimator`'s estimate of the request, atomically with the step.
  *
  * A step that fails returns a [[TurnFailure]], which ends the turn and is recorded like
  * any other output: a rerun ends the same way without calling anything.
  */
object Turn {

  /** The turn's compatibility epoch (ADR 0004). Every turn in flight was started under an
    * epoch, and only an engine of the same epoch resumes it. Change the steps compatibly
    * with `Durable.patch` and keep the epoch; change the epoch only for a break a patch
    * cannot carry, which strands the turns in flight under the old one. `TurnReplayTests`
    * replays the histories recorded under this epoch.
    */
  val Epoch = "2026-09-23"

  /** The turn workflow's body, for the turn whose workflow id is `workflowId`. Returns
    * what the turn did, for logs: its reply is in the store, never in this string.
    */
  def body(
      system: String,
      entries: EntryStore,
      ledger: UsageLedger,
      assembler: ContextAssembler,
      estimator: TokenEstimator,
      provider: Provider^,
      db: Db^
  )(workflowId: WorkflowId)(using d: Durable^): String =
    TurnRef.fromWorkflowId(workflowId) match {
      case None => s"not a turn: ${WorkflowId.value(workflowId)}"
      case Some(turn) =>
        run(system, entries, ledger, assembler, estimator, provider, db, turn) match {
          case Right(reply) => s"replied: ${EntryId.value(reply)}"
          case Left(failure) => s"failed: $failure"
        }
    }

  /** The id of `turn`'s reply entry. */
  def replyId(turn: TurnRef): EntryId =
    EntryId(s"reply:${WorkflowId.value(turn.workflowId)}")

  private def run(
      system: String,
      entries: EntryStore,
      ledger: UsageLedger,
      assembler: ContextAssembler,
      estimator: TokenEstimator,
      provider: Provider^,
      db: Db^,
      turn: TurnRef
  )(using d: Durable^): Either[TurnFailure, EntryId] = {
    import TurnJournal.given
    val reply = replyId(turn)
    for {
      window <- d.step("assemble") { () =>
        assembler.assemble(AssemblyRequest(turn))(using db).left.map {
          case AssemblyError.Store(error) => TurnFailure.Assembly(describe(error))
        }
      }
      message <- d.step("call-model") { () =>
        request(system, entries, db, turn, window).flatMap { req =>
          provider.complete(req).left.map { case ProviderError.Unavailable(cause) =>
            TurnFailure.Model(cause)
          }
        }
      }
      appended <- d.transact("append")(
        append(system, entries, ledger, estimator, turn, window, reply, message)
      )
    } yield appended
  }

  /** The window's entries, then the turn's own, as one model request. */
  private def request(
      system: String,
      entries: EntryStore,
      db: Db^,
      turn: TurnRef,
      window: Window
  ): Either[TurnFailure, ModelRequest] =
    db.read(entries.list(turn.conversationId))
      .left
      .map(storeFailure)
      .flatMap(requestOf(system, _, turn, window))

  /** The request [[request]] builds, from `all` of the conversation's entries. */
  private def requestOf(
      system: String,
      all: Vector[Entry],
      turn: TurnRef,
      window: Window
  ): Either[TurnFailure, ModelRequest] = {
    val byId = all.map(e => e.id -> e).toMap
    window.entries.filterNot(byId.contains) match {
      case missing if missing.nonEmpty =>
        Left(
          TurnFailure.Assembly(
            s"window names unknown entries: ${missing.map(EntryId.value).mkString(", ")}"
          )
        )
      case _ =>
        val seen = window.entries.flatMap(byId.get) ++ all.filter(_.turnSeq == turn.turnSeq)
        Right(
          ModelRequest(system, seen.map(e => e.payload match { case Payload.Message(m) => m }))
        )
    }
  }

  /** Records `message` as `turn`'s entry `id`, after everything already in the
    * conversation, and what it cost in the ledger beside the estimate of the request that
    * produced it. The request is rebuilt from the same window and the same entries: the
    * turn's own were all recorded before its call, and the reply is not yet among them.
    */
  private def append(
      system: String,
      entries: EntryStore,
      ledger: UsageLedger,
      estimator: TokenEstimator,
      turn: TurnRef,
      window: Window,
      id: EntryId,
      message: Message.Assistant
  )(using Tx^): Either[TurnFailure, EntryId] =
    for {
      next <- entries.lockNext(turn.conversationId).left.map(storeFailure)
      all <- entries.list(turn.conversationId).left.map(storeFailure)
      sent <- requestOf(system, all, turn, window)
      _ <- entries
        .insert(
          Entry(
            id,
            turn.conversationId,
            turn.turnSeq,
            None,
            next.seq,
            Payload.Message(message),
            Instant.now()
          )
        )
        .left
        .map(storeFailure)
      _ <- ledger
        .record(id, turn.workflowId, message.model, message.usage, estimator.request(sent))
        .left
        .map(storeFailure)
    } yield id

  private def storeFailure(error: StoreError): TurnFailure = TurnFailure.Store(describe(error))

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
  }
}
