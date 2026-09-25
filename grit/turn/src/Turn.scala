package grit.turn

import java.time.Instant
import java.util.UUID

import grit.core.context.{AssemblyError, AssemblyNote, AssemblyRequest, Window}
import grit.core.durable.{Durable, StreamWriter}
import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.message.Message
import grit.core.provider.{ModelRequest, ProviderError}
import grit.core.store.{Entry, Payload, StoreError, Tx}
import grit.core.topic.Topic

import TurnVerdict.Shape

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
  *      beside the estimate of its request, atomically with the step.
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

  /** The turn workflow's body, for the turn whose workflow id is `workflowId`. Returns what
    * the turn did, for logs: its reply and summary are in the store, never in this string.
    */
  def body(env: TurnEnv^)(workflowId: WorkflowId)(using d: Durable^): String =
    TurnRef.fromWorkflowId(workflowId) match {
      case None => s"not a turn: ${WorkflowId.value(workflowId)}"
      case Some(turn) =>
        import TurnJournal.given
        val records = env.records
        val placing =
          if (d.patch(Patches.Topics))
            Placing.Placed(d.step(Step.Classify) { () =>
              TurnTopics.classify(env.classifier, records.entries, env.db, records.estimator, turn)
            })
          else Placing.Unplaced
        val topicFailure = placing match {
          case Placing.Placed(c) =>
            d.transact(Step.RecordTopic)(
              TurnTopics.record(records.entries, records.ledger, turn, c)
            ).left
              .toOption
          case Placing.Unplaced => None
        }
        val ran = run(turn, placing)(using env, d)
        val topics = topicFailure.fold("")(f => s"; topics not recorded: $f") +
          ran.verdictUnrecorded.fold("")(f => s"; verdict not recorded: $f")
        ran.result match {
          case Left(failure) => s"failed: $failure$topics"
          case Right(reply) =>
            val summary =
              summarise(turn, reply, placing)(using env, d) match {
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

  /** Whether `turn`'s message was placed among its conversation's topics
    * ([[Patches.Topics]]).
    */
  private enum Placing {

    /** Placed: `c` is what the `classify` step decided. */
    case Placed(c: TurnTopics.Classification)

    /** Not placed: the turn passed `classify` before topics existed. */
    case Unplaced
  }

  /** Where a turn records its window and the queries that chose it
    * ([[Patches.RecordWindow]]).
    */
  private enum WindowRecord {

    /** In the `record-window` step, before the model is called. */
    case OwnStep

    /** With the reply, in `append`: the turn passed `record-window` before it existed. */
    case WithReply
  }

  /** The classification the model is to be asked about: `placing`'s, when the classifier
    * was unsure and the turn takes the verdict patch. The patch is consulted only for an
    * unsure placement ([[Patches.Verdict]]).
    */
  private def asking(placing: Placing)(using d: Durable^): Option[TurnTopics.Classification] =
    placing match {
      case Placing.Placed(c) if c.uncertain => Option.when(d.patch(Patches.Verdict))(c)
      case _ => None
    }

  /** What steps came to, `result`, and the verdict's record when it failed without failing
    * the turn.
    */
  private final case class Ran[A](
      result: Either[TurnFailure, A],
      verdictUnrecorded: Option[TurnFailure]
  )

  /** The steps from `assemble` to `append`: `turn`'s reply entry, or why it has none. */
  private def run(
      turn: TurnRef,
      placing: Placing
  )(using env: TurnEnv^, d: Durable^): Ran[EntryId] = {
    import TurnJournal.given
    val heard = d.stream(TurnStream.Key)
    val called = for {
      window <- d.step(Step.Assemble) { () =>
        env.assembler.assemble(AssemblyRequest(turn))(using env.db).left.map {
          case AssemblyError.Store(error) => TurnFailure.Assembly(describe(error))
        }
      }
      recorded =
        if (d.patch(Patches.RecordWindow)) WindowRecord.OwnStep else WindowRecord.WithReply
      _ <- recorded match {
        case WindowRecord.OwnStep =>
          d.transact(Step.RecordWindow)(recordWindow(env.records, turn, window))
        case WindowRecord.WithReply => Right(windowId(turn))
      }
      asked = asking(placing)
      message <- d.step(Step.CallModel) { () =>
        callModel(heard, turn, window, asked.fold(Shape.Plain)(Shape.Offered(_)))
      }
    } yield (window, recorded, asked, message)
    called match {
      case Left(failure) => Ran(Left(failure), None)
      case Right((window, recorded, asked, message)) =>
        val answered = asked match {
          case None => Ran(Right(TurnVerdict.Replied(message, Shape.Plain)), None)
          case Some(c) => verdictRound(heard, turn, window, c, message)
        }
        val appended = answered.result.flatMap { a =>
          d.transact(Step.Append)(
            append(env.system, env.records, turn, window, replyId(turn), a, recorded)
          )
        }
        Ran(appended, answered.verdictUnrecorded)
    }
  }

  /** The request built from `turn`'s `window` and shaped by `shape`, sent to the provider,
    * whose reply is told to edges as it arrives, as a fresh attempt each time this runs:
    * a rerun's pieces follow a crashed run's.
    */
  private def callModel(
      heard: StreamWriter^,
      turn: TurnRef,
      window: Window,
      shape: Shape
  )(using env: TurnEnv^): Either[TurnFailure, Message.Assistant] = {
    val told =
      new TurnStream.Writer(heard, UUID.randomUUID().toString, () => System.nanoTime() / 1000000)
    val result = request(env, turn, window).flatMap { req =>
      env.provider.stream(shape(req), told.tell).left.map { case ProviderError.Unavailable(cause) =>
        TurnFailure.Model(cause)
      }
    }
    told.flush()
    result
  }

  /** After a first call that offered the `topic` tool for `c` ([[TurnVerdict.round]]): the
    * reply that answers the turn, its further calls made as the `call-model-again` and
    * `call-model-plain` steps and its verdict recorded as `record-verdict`.
    */
  private def verdictRound(
      heard: StreamWriter^,
      turn: TurnRef,
      seen: Window,
      c: TurnTopics.Classification,
      first: Message.Assistant
  )(using env: TurnEnv^, d: Durable^): Ran[TurnVerdict.Replied] = {
    import TurnJournal.given
    val further = new TurnVerdict.Calls {
      def again(shape: Shape.Again): Either[TurnFailure, Message.Assistant] =
        d.step(Step.CallModelAgain) { () => callModel(heard, turn, seen, shape) }
      def plain(): Either[TurnFailure, Message.Assistant] =
        d.step(Step.CallModelPlain) { () => callModel(heard, turn, seen, Shape.Plain) }
    }
    val round = TurnVerdict.round(c, first, further)
    val recorded = d.transact(Step.RecordVerdict)(
      recordVerdict(env.system, env.records, turn, seen, c, round)
    )
    Ran(round.answer, recorded.left.toOption)
  }

  /** The `record-verdict` step: where `round`'s verdict places `turn`'s message, recorded
    * as its entry [[TurnVerdict.verdictId]] with the round's anomaly, and what the replies
    * it spent cost in the ledger beside it.
    */
  private def recordVerdict(
      system: String,
      records: TurnRecords,
      turn: TurnRef,
      window: Window,
      c: TurnTopics.Classification,
      round: TurnVerdict.Round
  )(using Tx^): Either[TurnFailure, EntryId] =
    for {
      all <- records.entries.list(turn.conversationId).left.map(storeFailure)
      base <- requestOf(system, all, turn, window)
      id <- TurnTopics.writeEvents(
        records.entries,
        records.ledger,
        turn,
        TurnVerdict.verdictId(turn),
        TurnVerdict.events(round.verdict, round.anomaly, c, turn),
        TurnVerdict.cost(round.spent.map(r => r.reply -> records.estimator.request(r.shape(base))))
      )
    } yield id

  /** The last two steps: the summary of `turn`, whose reply is `answered`, with the name
    * and description of its message's topic when `placing` placed it.
    */
  private def summarise(
      turn: TurnRef,
      answered: EntryId,
      placing: Placing
  )(using env: TurnEnv^, d: Durable^): Either[TurnFailure, EntryId] = {
    import TurnJournal.given
    for {
      message <- d.step(Step.Summarise) { () =>
        env.db
          .read(env.records.entries.list(turn.conversationId))
          .left
          .map(storeFailure)
          .flatMap { all =>
            env.summarizer.complete(summaryRequest(all, turn, placing)).left.map {
              case ProviderError.Unavailable(cause) => TurnFailure.Model(cause)
            }
          }
      }
      appended <- d.transact(Step.AppendSummary)(
        appendSummary(env.records, turn, answered, message, placing)
      )
    } yield appended
  }

  /** The summary request for `turn`, from `all` of its conversation's entries: asking after
    * its topic too when `placing` placed it and it has one.
    */
  private def summaryRequest(all: Vector[Entry], turn: TurnRef, placing: Placing): ModelRequest =
    TurnSummary.request(own(all, turn), topicOf(all, turn, placing))

  /** The topic `turn`'s message is in, as `all` of its conversation's entries leave it, when
    * `placing` placed it.
    */
  private def topicOf(all: Vector[Entry], turn: TurnRef, placing: Placing): Option[Topic] =
    placing match {
      case Placing.Placed(_) => TurnTopics.topicOf(all, turn)
      case Placing.Unplaced => None
    }

  /** Records the summary in `message` as `turn`'s summary, a child of its reply `answered`,
    * after everything already in the conversation; and what it cost in the ledger beside
    * the estimate of the request that produced it, rebuilt as [[append]] rebuilds its own.
    * What it says of the topic `placing` placed the message in is recorded after it
    * ([[TurnTopics.describedId]]).
    */
  private def appendSummary(
      records: TurnRecords,
      turn: TurnRef,
      answered: EntryId,
      message: Message.Assistant,
      placing: Placing
  )(using Tx^): Either[TurnFailure, EntryId] = {
    val id = TurnSummary.id(turn)
    val TurnRecords(entries, ledger, estimator) = records
    for {
      read <- (placing match {
        case Placing.Placed(_) => TurnSummary.read(message)
        case Placing.Unplaced => TurnSummary.text(message).map(TurnSummary.Read(_, None))
      })
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
          estimator.request(summaryRequest(all, turn, placing))
        )
        .left
        .map(storeFailure)
      described = topicOf(all, turn, placing).toVector.flatMap(TurnTopics.described(_, read))
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

  /** The window's entries, then the turn's own, as one model request, read through `env`'s
    * `db`.
    */
  private def request(
      env: TurnEnv^,
      turn: TurnRef,
      window: Window
  ): Either[TurnFailure, ModelRequest] =
    env.db
      .read(env.records.entries.list(turn.conversationId))
      .left
      .map(storeFailure)
      .flatMap(requestOf(env.system, _, turn, window))

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
    * all recorded before its call, and the reply is not yet among them), shaped as
    * `answered` says.
    * When the window is `recorded` with the reply, its queries and the window itself go in
    * first ([[writeWindow]]).
    */
  private def append(
      system: String,
      records: TurnRecords,
      turn: TurnRef,
      window: Window,
      id: EntryId,
      answered: TurnVerdict.Replied,
      recorded: WindowRecord
  )(using Tx^): Either[TurnFailure, EntryId] = {
    val TurnVerdict.Replied(message, shape) = answered
    val TurnRecords(entries, ledger, estimator) = records
    for {
      next <- entries.lockNext(turn.conversationId).left.map(storeFailure)
      all <- entries.list(turn.conversationId).left.map(storeFailure)
      sent <- requestOf(system, all, turn, window).map(shape(_))
      written <- recorded match {
        case WindowRecord.OwnStep => Right(0)
        case WindowRecord.WithReply => writeWindow(records, turn, window, next.seq)
      }
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
      records: TurnRecords,
      turn: TurnRef,
      window: Window
  )(using Tx^): Either[TurnFailure, EntryId] =
    for {
      next <- records.entries.lockNext(turn.conversationId).left.map(storeFailure)
      _ <- writeWindow(records, turn, window, next.seq)
    } yield windowId(turn)

  /** Writes each query `window`'s notes say assembly wrote, as [[queryId]] with its own
    * cost, then the window itself, as [[windowId]], from position `from`; how many
    * entries that was.
    */
  private def writeWindow(
      records: TurnRecords,
      turn: TurnRef,
      window: Window,
      from: Long
  )(using Tx^): Either[TurnFailure, Int] = {
    val TurnRecords(entries, ledger, _) = records
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
