package grit.turn

import java.time.Instant
import java.util.UUID

import grit.core.classify.Classifier
import grit.core.context.{AssemblyError, AssemblyNote, AssemblyRequest, ContextAssembler, Window}
import grit.core.durable.{Durable, StreamWriter}
import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.message.Message
import grit.core.provider.{ModelRequest, Provider, ProviderError, TokenEstimator}
import grit.core.store.{Db, Entry, EntryStore, Payload, StoreError, Tx, UsageLedger}
import grit.core.topic.Verdict

/** The durable turn: one workflow per turn. Each step's output is recorded, so a turn
  * resumed after a crash never calls a model twice.
  *
  *   1. `classify` — where the turn's message goes among the conversation's topics
  *      ([[TurnTopics]]). Never fails the turn.
  *   1. `record-topic` — that placement recorded as an entry, with the classifier's cost.
  *      Turns that passed this point before topics existed have neither step
  *      ([[Patches.Topics]]).
  *   1. `assemble` — a fresh window over what came before the turn.
  *   1. `record-window` — the window recorded as an entry, after any search query
  *      assembly wrote (its own entry, with its own cost in the usage ledger), so an edge
  *      sees what the model will see while it answers. Turns that passed this point
  *      before the step existed record both in `append` instead ([[Patches]]).
  *   1. `call-model` — the window, then the turn's own messages, sent to the provider,
  *      whose reply is told to edges as it arrives ([[TurnStream]]). When the classifier
  *      was unsure, the request offers the `topic` tool ([[TurnVerdict]]).
  *   1. `call-model-again`, `call-model-plain`, `record-verdict` — only when the tool was
  *      offered: the model called again after its call to the tool, a plain call if that
  *      does not answer, and its verdict recorded ([[Step.optional]]).
  *   1. `append` — the reply recorded as the turn's entry, with its cost in the ledger
  *      beside `estimator`'s estimate of the request, atomically with the step.
  *   1. `summarise` — the turn's own messages sent to the summarizer ([[TurnSummary]]).
  *   1. `append-summary` — the summary recorded as the turn's entry after the reply, with
  *      its cost in the ledger as in `append`.
  *
  * A step that fails returns a [[TurnFailure]], recorded like any other output, so a rerun
  * ends the same way without calling anything. A failure before the reply ends the turn;
  * a failed summary leaves the reply standing, and the turn without a summary. Topics
  * never fail a turn.
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
    val Classify = "classify"
    val RecordTopic = "record-topic"
    val Assemble = "assemble"
    val RecordWindow = "record-window"
    val CallModel = "call-model"
    val CallModelAgain = "call-model-again"
    val CallModelPlain = "call-model-plain"
    val RecordVerdict = "record-verdict"
    val Append = "append"
    val Summarise = "summarise"
    val AppendSummary = "append-summary"

    /** The steps only a turn whose model was asked about its topic takes. */
    val optional: Vector[String] = Vector(CallModelAgain, CallModelPlain, RecordVerdict)

    val all: Vector[String] =
      Vector(
        Classify,
        RecordTopic,
        Assemble,
        RecordWindow,
        CallModel,
        CallModelAgain,
        CallModelPlain,
        RecordVerdict,
        Append,
        Summarise,
        AppendSummary
      )
  }

  /** The patches the turn's steps have taken within this epoch (ADR 0004). */
  object Patches {

    /** The window and its queries are recorded in their own step before the model call,
      * not with the reply (2026-09-24).
      */
    val RecordWindow = "record-window"

    /** The turn's message is placed among the conversation's topics before its window is
      * assembled (2026-09-24).
      */
    val Topics = "topics"

    /** A turn whose classifier was unsure offers its model the `topic` tool, and records
      * its verdict (2026-09-24).
      */
    val Verdict = "verdict"
  }

  /** The step a running turn is in, given the names of the steps it has `recorded` (a step
    * is recorded when it completes): the next step after the last recorded that every turn
    * takes ([[Step.optional]] ones only once recorded). Names that are not the turn's steps
    * are skipped; once the last step is recorded the turn is finishing, and that step is named.
    */
  def running(recorded: Vector[String]): String = {
    val done = recorded.map(Step.all.indexOf).filter(_ >= 0).maxOption.getOrElse(-1)
    Step.all
      .drop(done + 1)
      .find(!Step.optional.contains(_))
      .orElse(Step.all.lastOption)
      .getOrElse(Step.Assemble)
  }

  /** The turn workflow's body, for the turn whose workflow id is `workflowId`: `classifier`
    * places its message among the topics, `provider` answers, `summarizer` summarises.
    * Returns what the turn did, for logs: its reply and summary are in the store, never in
    * this string.
    */
  def body(
      system: String,
      entries: EntryStore,
      ledger: UsageLedger,
      assembler: ContextAssembler^,
      estimator: TokenEstimator,
      classifier: Classifier^,
      provider: Provider^,
      summarizer: Provider^,
      db: Db^
  )(workflowId: WorkflowId)(using d: Durable^): String =
    TurnRef.fromWorkflowId(workflowId) match {
      case None => s"not a turn: ${WorkflowId.value(workflowId)}"
      case Some(turn) =>
        import TurnJournal.given
        // Turns that passed this point before topics existed go straight to assembly.
        val topical = d.patch(Patches.Topics)
        val placed: Option[TurnTopics.Classification] =
          Option.when(topical)(
            d.step(Step.Classify) { () =>
              TurnTopics.classify(classifier, entries, db, estimator, turn)
            }
          )
        val topicFailure = placed.flatMap { c =>
          d.transact(Step.RecordTopic)(TurnTopics.record(entries, ledger, turn, c)).left.toOption
        }
        val topics = topicFailure.fold("")(f => s"; topics not recorded: $f")
        run(system, entries, ledger, assembler, estimator, provider, db, turn, placed) match {
          case Left(failure) => s"failed: $failure$topics"
          case Right(reply) =>
            val summary =
              summarise(entries, ledger, estimator, summarizer, db, turn, reply, topical) match {
                case Right(id) => s"summarised: ${EntryId.value(id)}"
                case Left(failure) => s"no summary: $failure"
              }
            s"replied: ${EntryId.value(reply)}; $summary$topics"
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
      turn: TurnRef,
      placed: Option[TurnTopics.Classification]
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
      // Turns unsure of their topic that passed this point before the verdict round offer
      // no tool.
      asking = placed.filter(_.uncertain).filter(_ => d.patch(Patches.Verdict))
      first: (ModelRequest -> ModelRequest) = base => asking.fold(base)(TurnVerdict.offer(base, _))
      message <- d.step(Step.CallModel) { () =>
        callModel(system, entries, provider, heard, db, turn, window, first)
      }
      answered <- asking match {
        case None => Right((message, first))
        case Some(c) =>
          verdictRound(
            system,
            entries,
            ledger,
            estimator,
            provider,
            heard,
            db,
            turn,
            window,
            c,
            message
          )
      }
      appended <- d.transact(Step.Append)(
        append(system, entries, ledger, estimator, turn, window, reply, answered, early)
      )
    } yield appended
  }

  /** The request built from `turn`'s `window` and shaped by `shape`, sent to `provider`,
    * whose reply is told to edges as it arrives, as a fresh attempt each time this runs:
    * a rerun's pieces follow a crashed run's.
    */
  private def callModel(
      system: String,
      entries: EntryStore,
      provider: Provider^,
      heard: StreamWriter^,
      db: Db^,
      turn: TurnRef,
      window: Window,
      shape: ModelRequest -> ModelRequest
  ): Either[TurnFailure, Message.Assistant] = {
    val told =
      new TurnStream.Writer(heard, UUID.randomUUID().toString, () => System.nanoTime() / 1000000)
    val result = request(system, entries, db, turn, window).flatMap { req =>
      provider.stream(shape(req), told.tell).left.map { case ProviderError.Unavailable(cause) =>
        TurnFailure.Model(cause)
      }
    }
    told.flush()
    result
  }

  /** After a first call that offered the `topic` tool ([[TurnVerdict]]): its verdict
    * recorded, and the reply with the shape of the request that produced it. A call to the
    * tool is answered and the model called again (`call-model-again`); if that fails, or
    * calls a tool again and says nothing, a plain call answers (`call-model-plain`). Only
    * that plain call failing fails the turn.
    */
  private def verdictRound(
      system: String,
      entries: EntryStore,
      ledger: UsageLedger,
      estimator: TokenEstimator,
      provider: Provider^,
      heard: StreamWriter^,
      db: Db^,
      turn: TurnRef,
      seen: Window,
      c: TurnTopics.Classification,
      first: Message.Assistant
  )(using d: Durable^): Either[TurnFailure, (Message.Assistant, ModelRequest -> ModelRequest)] = {
    import TurnJournal.given
    val offered: ModelRequest -> ModelRequest = base => TurnVerdict.offer(base, c)
    val verdict = TurnVerdict.of(first)
    val (outcome, shape, anomaly, spent) =
      if (TurnVerdict.calls(first).isEmpty)
        (
          Right(first),
          offered,
          None,
          Vector.empty[(Message.Assistant, ModelRequest -> ModelRequest)]
        )
      else {
        val again: ModelRequest -> ModelRequest = base => TurnVerdict.again(offered(base), first)
        val second = d.step(Step.CallModelAgain) { () =>
          callModel(system, entries, provider, heard, db, turn, seen, again)
        }
        second.toOption.flatMap(TurnVerdict.answer) match {
          case Some(answer) =>
            val dropped =
              Option.when(second.exists(m => TurnVerdict.calls(m).nonEmpty))(
                "the second call called a tool again; its calls were dropped"
              )
            (Right(answer), again, dropped, Vector(first -> offered))
          case None =>
            val plainShape: ModelRequest -> ModelRequest = base => base
            val plain = d.step(Step.CallModelPlain) { () =>
              callModel(system, entries, provider, heard, db, turn, seen, plainShape)
            }
            val why = second match {
              case Left(failure) => s"the second call failed ($failure)"
              case Right(_) => "the second call called a tool again and said nothing"
            }
            (
              plain,
              plainShape,
              Some(s"$why; a plain call answered"),
              Vector(first -> offered) ++ second.toOption.map(_ -> again)
            )
        }
      }
    val _ = d.transact(Step.RecordVerdict)(
      recordVerdict(system, entries, ledger, estimator, turn, seen, c, verdict, anomaly, spent)
    )
    outcome.map(_ -> shape)
  }

  /** The `record-verdict` step: where `verdict` places `turn`'s message, recorded as its
    * entry [[TurnVerdict.verdictId]] with `anomaly`, and what the calls `spent` on it cost
    * (each with the shape of its request) in the ledger beside it.
    */
  private def recordVerdict(
      system: String,
      entries: EntryStore,
      ledger: UsageLedger,
      estimator: TokenEstimator,
      turn: TurnRef,
      window: Window,
      c: TurnTopics.Classification,
      verdict: Verdict,
      anomaly: Option[String],
      spent: Vector[(Message.Assistant, ModelRequest -> ModelRequest)]
  )(using Tx^): Either[TurnFailure, EntryId] =
    for {
      all <- entries.list(turn.conversationId).left.map(storeFailure)
      base <- requestOf(system, all, turn, window)
      id <- TurnTopics.writeEvents(
        entries,
        ledger,
        turn,
        TurnVerdict.verdictId(turn),
        TurnVerdict.events(verdict, anomaly, c, turn),
        TurnVerdict.cost(spent.map((m, shape) => m -> estimator.request(shape(base))))
      )
    } yield id

  /** The last two steps: the summary of `turn`, whose reply is `answered`; when `topical`,
    * with the name and description of its message's topic.
    */
  private def summarise(
      entries: EntryStore,
      ledger: UsageLedger,
      estimator: TokenEstimator,
      summarizer: Provider^,
      db: Db^,
      turn: TurnRef,
      answered: EntryId,
      topical: Boolean
  )(using d: Durable^): Either[TurnFailure, EntryId] = {
    import TurnJournal.given
    for {
      message <- d.step(Step.Summarise) { () =>
        db.read(entries.list(turn.conversationId))
          .left
          .map(storeFailure)
          .flatMap { all =>
            summarizer.complete(summaryRequest(all, turn, topical)).left.map {
              case ProviderError.Unavailable(cause) => TurnFailure.Model(cause)
            }
          }
      }
      appended <- d.transact(Step.AppendSummary)(
        appendSummary(entries, ledger, estimator, turn, answered, message, topical)
      )
    } yield appended
  }

  /** The summary request for `turn`, from `all` of its conversation's entries: asking after
    * its topic too when `topical` and its message has one.
    */
  private def summaryRequest(all: Vector[Entry], turn: TurnRef, topical: Boolean): ModelRequest =
    TurnSummary.request(
      own(all, turn),
      if (topical) TurnTopics.topicOf(all, turn) else None
    )

  /** Records the summary in `message` as `turn`'s summary, a child of its reply `answered`,
    * after everything already in the conversation; and what it cost in the ledger beside
    * the estimate of the request that produced it, rebuilt as [[append]] rebuilds its own.
    * When `topical`, what it says of the topic is recorded after it
    * ([[TurnTopics.describedId]]).
    */
  private def appendSummary(
      entries: EntryStore,
      ledger: UsageLedger,
      estimator: TokenEstimator,
      turn: TurnRef,
      answered: EntryId,
      message: Message.Assistant,
      topical: Boolean
  )(using Tx^): Either[TurnFailure, EntryId] = {
    val id = TurnSummary.id(turn)
    for {
      read <- (if (topical) TurnSummary.read(message)
               else TurnSummary.text(message).map(TurnSummary.Read(_, None)))
        .toRight(TurnFailure.Model(s"the summary has no text (stop: ${message.stop})"))
      text = read.summary
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
          estimator.request(summaryRequest(all, turn, topical))
        )
        .left
        .map(storeFailure)
      described = TurnTopics
        .topicOf(all, turn)
        .filter(_ => topical)
        .toVector
        .flatMap(TurnTopics.described(_, read))
      _ <- TurnTopics.writeEvents(
        entries,
        ledger,
        turn,
        TurnTopics.describedId(turn),
        described,
        None
      )
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

  /** Records the `answered` message as `turn`'s entry `id`, after everything already in the
    * conversation, and what it cost in the ledger beside the estimate of the request that
    * produced it: rebuilt from the same window and the same entries (the turn's own were
    * all recorded before its call, and the reply is not yet among them), in the shape
    * `answered` gives it.
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
      answered: (Message.Assistant, ModelRequest -> ModelRequest),
      windowRecorded: Boolean
  )(using Tx^): Either[TurnFailure, EntryId] = {
    val (message, shape) = answered
    for {
      next <- entries.lockNext(turn.conversationId).left.map(storeFailure)
      all <- entries.list(turn.conversationId).left.map(storeFailure)
      sent <- requestOf(system, all, turn, window).map(shape)
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
  }

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
