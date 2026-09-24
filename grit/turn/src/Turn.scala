package grit.turn

import java.time.Instant

import grit.core.context.{AssemblyError, AssemblyNote, AssemblyRequest, ContextAssembler, Window}
import grit.core.durable.Durable
import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.message.Message
import grit.core.provider.{ModelRequest, Provider, ProviderError, TokenEstimator}
import grit.core.store.{Db, Entry, EntryStore, Payload, StoreError, Tx, UsageLedger}

/** The durable turn: one workflow per turn, in five steps. Each step's output is
  * recorded, so a turn resumed after a crash never calls a model twice.
  *
  *   1. `assemble` — a fresh window over what came before the turn.
  *   2. `call-model` — the window, then the turn's own messages, sent to the provider.
  *   3. `append` — the reply recorded as the turn's entry, with its cost in the usage
  *      ledger beside `estimator`'s estimate of the request, atomically with the step;
  *      before it, any search query assembly wrote, as its own entry with its own cost.
  *   4. `summarise` — the turn's own messages sent to the summarizer ([[TurnSummary]]).
  *   5. `append-summary` — the summary recorded as the turn's entry after the reply, with
  *      its cost in the ledger as in `append`.
  *
  * A step that fails returns a [[TurnFailure]], recorded like any other output, so a rerun
  * ends the same way without calling anything. A failure before the reply ends the turn;
  * a failed summary leaves the reply standing, and the turn without a summary.
  */
object Turn {

  /** The turn's compatibility epoch (ADR 0004). Every turn in flight was started under an
    * epoch, and only an engine of the same epoch resumes it. Change the steps compatibly
    * with `Durable.patch` and keep the epoch; change the epoch only for a break a patch
    * cannot carry, which strands the turns in flight under the old one. `TurnReplayTests`
    * replays the histories recorded under this epoch.
    */
  val Epoch = "2026-09-23"

  /** The turn workflow's body, for the turn whose workflow id is `workflowId`: `provider`
    * answers, `summarizer` summarises. Returns what the turn did, for logs: its reply and
    * summary are in the store, never in this string.
    */
  def body(
      system: String,
      entries: EntryStore,
      ledger: UsageLedger,
      assembler: ContextAssembler^,
      estimator: TokenEstimator,
      provider: Provider^,
      summarizer: Provider^,
      db: Db^
  )(workflowId: WorkflowId)(using d: Durable^): String =
    TurnRef.fromWorkflowId(workflowId) match {
      case None => s"not a turn: ${WorkflowId.value(workflowId)}"
      case Some(turn) =>
        run(system, entries, ledger, assembler, estimator, provider, db, turn) match {
          case Left(failure) => s"failed: $failure"
          case Right(reply) =>
            val summary = summarise(entries, ledger, estimator, summarizer, db, turn, reply) match {
              case Right(id) => s"summarised: ${EntryId.value(id)}"
              case Left(failure) => s"no summary: $failure"
            }
            s"replied: ${EntryId.value(reply)}; $summary"
        }
    }

  /** The id of the `i`-th search query assembly wrote for `turn`, from 0. */
  def queryId(turn: TurnRef, i: Int): EntryId = {
    val base = s"query:${WorkflowId.value(turn.workflowId)}"
    EntryId(if (i == 0) base else s"$base:$i")
  }

  /** The id of `turn`'s reply entry. */
  def replyId(turn: TurnRef): EntryId =
    EntryId(s"reply:${WorkflowId.value(turn.workflowId)}")

  private def run(
      system: String,
      entries: EntryStore,
      ledger: UsageLedger,
      assembler: ContextAssembler^,
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

  /** Steps 4 and 5: the summary of `turn`, whose reply is `answered`. */
  private def summarise(
      entries: EntryStore,
      ledger: UsageLedger,
      estimator: TokenEstimator,
      summarizer: Provider^,
      db: Db^,
      turn: TurnRef,
      answered: EntryId
  )(using d: Durable^): Either[TurnFailure, EntryId] = {
    import TurnJournal.given
    for {
      message <- d.step("summarise") { () =>
        db.read(entries.list(turn.conversationId))
          .left
          .map(storeFailure)
          .flatMap { all =>
            summarizer.complete(TurnSummary.request(own(all, turn))).left.map {
              case ProviderError.Unavailable(cause) => TurnFailure.Model(cause)
            }
          }
      }
      appended <- d.transact("append-summary")(
        appendSummary(entries, ledger, estimator, turn, answered, message)
      )
    } yield appended
  }

  /** Records the text of `message` as `turn`'s summary, a child of its reply `answered`,
    * after everything already in the conversation; and what it cost in the ledger beside
    * the estimate of the request that produced it, rebuilt as [[append]] rebuilds its own.
    */
  private def appendSummary(
      entries: EntryStore,
      ledger: UsageLedger,
      estimator: TokenEstimator,
      turn: TurnRef,
      answered: EntryId,
      message: Message.Assistant
  )(using Tx^): Either[TurnFailure, EntryId] = {
    val id = TurnSummary.id(turn)
    for {
      text <- TurnSummary
        .text(message)
        .toRight(TurnFailure.Model(s"the summary has no text (stop: ${message.stop})"))
      next <- entries.lockNext(turn.conversationId).left.map(storeFailure)
      all <- entries.list(turn.conversationId).left.map(storeFailure)
      _ <- entries
        .insert(
          Entry(
            id,
            turn.conversationId,
            turn.turnSeq,
            Some(answered),
            next.seq,
            Payload.Summary(text),
            Instant.now()
          )
        )
        .left
        .map(storeFailure)
      _ <- ledger
        .record(
          id,
          turn.workflowId,
          message.model,
          message.usage,
          estimator.request(TurnSummary.request(own(all, turn)))
        )
        .left
        .map(storeFailure)
    } yield id
  }

  /** `turn`'s own entries, from `all` of its conversation's. */
  private def own(all: Vector[Entry], turn: TurnRef): Vector[Entry] =
    all.filter(_.turnSeq == turn.turnSeq)

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

  /** The request [[request]] builds, from `all` of the conversation's entries. Only
    * messages are sent: a summary is not shown to the model yet.
    */
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
        val seen = window.entries.flatMap(byId.get) ++ own(all, turn)
        Right(ModelRequest(system, seen.map(_.payload).collect { case Payload.Message(m) => m }))
    }
  }

  /** Records `message` as `turn`'s entry `id`, after everything already in the
    * conversation, and what it cost in the ledger beside the estimate of the request that
    * produced it. The request is rebuilt from the same window and the same entries: the
    * turn's own were all recorded before its call, and the reply is not yet among them.
    * Each query `window`'s notes say assembly wrote goes in first, as [[queryId]], with
    * its own cost.
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
      queries = window.notes.collect { case q: AssemblyNote.Queried => q }
      _ <- queries.zipWithIndex.foldLeft[Either[TurnFailure, Unit]](Right(())) {
        case (done, (q, i)) =>
          done.flatMap { _ =>
            val qid = queryId(turn, i)
            val entry = Entry(
              qid,
              turn.conversationId,
              turn.turnSeq,
              None,
              next.seq + i,
              Payload.Query(q.query),
              Instant.now()
            )
            entries
              .insert(entry)
              .flatMap(_ => ledger.record(qid, turn.workflowId, q.model, q.usage, q.estimate))
              .left
              .map(storeFailure)
          }
      }
      _ <- entries
        .insert(
          Entry(
            id,
            turn.conversationId,
            turn.turnSeq,
            None,
            next.seq + queries.size,
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
