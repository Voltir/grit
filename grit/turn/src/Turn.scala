package grit.turn

import java.time.Instant
import java.util.UUID

import grit.core.context.{AssemblyError, AssemblyNote, AssemblyRequest, ContextAssembler, Window}
import grit.core.durable.Durable
import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.message.Message
import grit.core.provider.{ModelRequest, Provider, ProviderError, TokenEstimator}
import grit.core.store.{Db, Entry, EntryStore, Payload, StoreError, Tx, UsageLedger}

/** The durable turn: one workflow per turn, in six steps. Each step's output is
  * recorded, so a turn resumed after a crash never calls a model twice.
  *
  *   1. `assemble` — a fresh window over what came before the turn.
  *   2. `record-window` — the window recorded as an entry, after any search query
  *      assembly wrote (its own entry, with its own cost in the usage ledger), so an edge
  *      sees what the model will see while it answers. Turns that passed this point
  *      before the step existed record both in `append` instead ([[Patches]]).
  *   3. `call-model` — the window, then the turn's own messages, sent to the provider,
  *      whose reply is told to edges as it arrives ([[TurnStream]]).
  *   4. `append` — the reply recorded as the turn's entry, with its cost in the ledger
  *      beside `estimator`'s estimate of the request, atomically with the step.
  *   5. `summarise` — the turn's own messages sent to the summarizer ([[TurnSummary]]).
  *   6. `append-summary` — the summary recorded as the turn's entry after the reply, with
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

  /** The turn's steps, as DBOS records their names, in the order they run. */
  object Step {
    val Assemble = "assemble"
    val RecordWindow = "record-window"
    val CallModel = "call-model"
    val Append = "append"
    val Summarise = "summarise"
    val AppendSummary = "append-summary"

    val all: Vector[String] =
      Vector(Assemble, RecordWindow, CallModel, Append, Summarise, AppendSummary)
  }

  /** The patches the turn's steps have taken within this epoch (ADR 0004). */
  object Patches {

    /** The window and its queries are recorded in their own step before the model call,
      * not with the reply (2026-09-24).
      */
    val RecordWindow = "record-window"
  }

  /** The step a running turn is in, given the names of the steps it has `recorded` (a step
    * is recorded when it completes). Names that are not the turn's steps are skipped; once
    * the last step is recorded the turn is finishing, and that step is named.
    */
  def running(recorded: Vector[String]): String = {
    val done = recorded.map(Step.all.indexOf).filter(_ >= 0).maxOption.getOrElse(-1)
    Step.all.lift(done + 1).orElse(Step.all.lastOption).getOrElse(Step.Assemble)
  }

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

  /** The id of the record of `turn`'s window. */
  def windowId(turn: TurnRef): EntryId = EntryId(s"window:${WorkflowId.value(turn.workflowId)}")

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
    val heard = d.stream(TurnStream.Key)
    for {
      window <- d.step(Step.Assemble) { () =>
        assembler.assemble(AssemblyRequest(turn))(using db).left.map {
          case AssemblyError.Store(error) => TurnFailure.Assembly(describe(error))
        }
      }
      // Turns that passed this point before the patch record their window with the reply.
      early = d.patch(Patches.RecordWindow)
      _ <-
        if (early) d.transact(Step.RecordWindow)(recordWindow(entries, ledger, turn, window))
        else Right(windowId(turn))
      message <- d.step(Step.CallModel) { () =>
        // A fresh attempt each time the step runs: a rerun's pieces follow a crashed run's.
        val told =
          new TurnStream.Writer(
            heard,
            UUID.randomUUID().toString,
            () => System.nanoTime() / 1000000
          )
        val result = request(system, entries, db, turn, window).flatMap { req =>
          provider.stream(req, told.tell).left.map { case ProviderError.Unavailable(cause) =>
            TurnFailure.Model(cause)
          }
        }
        told.flush()
        result
      }
      appended <- d.transact(Step.Append)(
        append(system, entries, ledger, estimator, turn, window, reply, message, early)
      )
    } yield appended
  }

  /** Steps 5 and 6: the summary of `turn`, whose reply is `answered`. */
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
      message <- d.step(Step.Summarise) { () =>
        db.read(entries.list(turn.conversationId))
          .left
          .map(storeFailure)
          .flatMap { all =>
            summarizer.complete(TurnSummary.request(own(all, turn))).left.map {
              case ProviderError.Unavailable(cause) => TurnFailure.Model(cause)
            }
          }
      }
      appended <- d.transact(Step.AppendSummary)(
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
    * Unless the `record-window` step already wrote them (`windowRecorded`), the window's
    * queries and the window itself go in first ([[writeWindow]]).
    */
  private def append(
      system: String,
      entries: EntryStore,
      ledger: UsageLedger,
      estimator: TokenEstimator,
      turn: TurnRef,
      window: Window,
      id: EntryId,
      message: Message.Assistant,
      windowRecorded: Boolean
  )(using Tx^): Either[TurnFailure, EntryId] =
    for {
      next <- entries.lockNext(turn.conversationId).left.map(storeFailure)
      all <- entries.list(turn.conversationId).left.map(storeFailure)
      sent <- requestOf(system, all, turn, window)
      written <-
        if (windowRecorded) Right(0) else writeWindow(entries, ledger, turn, window, next.seq)
      _ <- entries
        .insert(
          Entry(
            id,
            turn.conversationId,
            turn.turnSeq,
            None,
            next.seq + written,
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

  /** The `record-window` step: `turn`'s window and the queries that chose it, recorded
    * before the model is called, so an edge sees them while the turn runs.
    */
  private def recordWindow(
      entries: EntryStore,
      ledger: UsageLedger,
      turn: TurnRef,
      window: Window
  )(using Tx^): Either[TurnFailure, EntryId] =
    for {
      next <- entries.lockNext(turn.conversationId).left.map(storeFailure)
      _ <- writeWindow(entries, ledger, turn, window, next.seq)
    } yield windowId(turn)

  /** Writes each query `window`'s notes say assembly wrote, as [[queryId]] with its own
    * cost, then the window itself, as [[windowId]], from position `from`; how many
    * entries that was.
    */
  private def writeWindow(
      entries: EntryStore,
      ledger: UsageLedger,
      turn: TurnRef,
      window: Window,
      from: Long
  )(using Tx^): Either[TurnFailure, Int] = {
    val queries = window.notes.collect { case q: AssemblyNote.Queried => q }
    val recalled = window.notes.flatMap {
      case AssemblyNote.Recalled(turns) => turns
      case _ => Vector.empty
    }
    for {
      _ <- queries.zipWithIndex.foldLeft[Either[TurnFailure, Unit]](Right(())) {
        case (done, (q, i)) =>
          done.flatMap { _ =>
            val qid = queryId(turn, i)
            val entry = Entry(
              qid,
              turn.conversationId,
              turn.turnSeq,
              None,
              from + i,
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
            windowId(turn),
            turn.conversationId,
            turn.turnSeq,
            None,
            from + queries.size,
            Payload.Window(window.entries, recalled),
            Instant.now()
          )
        )
        .left
        .map(storeFailure)
    } yield queries.size + 1
  }

  private def storeFailure(error: StoreError): TurnFailure = TurnFailure.Store(describe(error))

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
  }
}
