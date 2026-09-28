package grit.turn

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.approval.Approval
import grit.core.context.{AssemblyError, AssemblyNote, AssemblyRequest, Shown, Window}
import grit.core.durable.{Durable, StreamWriter}
import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.message.Message
import grit.core.model.{AfterToolResult, StrictSchemas, ToolGuidance, TurnProfile}
import grit.core.provider.{ModelRequest, ProviderError, ToolSchema, ToolUse}
import grit.core.store.{Entry, EntryStore, Jot, Payload, Speakers, StoreError, Tx}
import grit.core.tool.{Bound, DuplicateName, Repairs, ToolName, Toolbox}
import grit.core.topic.Topic

import TurnLoop.{Pending, Round}
import TurnVerdict.Shape

/** The durable turn: one workflow per turn. Each step's output is recorded, so a turn
  * resumed after a crash never calls a model twice.
  *
  *   1. `pin-models` — the catalog in force, pinned as the turn's profile.
  *   1. `offer` — what the turn offers its model ([[TurnOffer]]): its conversation's
  *      workspace, the tool set (its own tools, and the hosted ones the edge serving that
  *      workspace advertises) and the system prompt (ADR 0016), kept by content id and
  *      recorded by id; a rerun reads them back, so it is offered what it first was.
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
  *      whose reply is told to edges as it arrives ([[TurnStream]]), offering the turn's
  *      tools ([[TurnTooling]]), and `topic` too when the classifier was unsure
  *      ([[TurnVerdict]]).
  *   1. `record-call:n`, `tool:n:j`, `call-model:n` — the tool loop ([[TurnLoop]]): a reply
  *      that called tools kept as an entry, each of its calls settled in its own step
  *      ([[TurnTools]]), and the model called again, until a reply calls no tool or the
  *      budget's last call, made with tools off and told so ([[TurnLoop.LastCall]]). A call a person approves first is asked about in `ask:n:j`, an
  *      entry an edge shows, then waits up to [[TurnTooling.answerWithin]] for the answer
  *      [[grit.core.inbox.Inbox.answer]] sends ([[grit.core.durable.Durable.recv]]) before
  *      its `tool:n:j`; unanswered, it is denied. A call to a hosted tool is a request an
  *      edge runs ([[TurnHosted]], ADR 0017): a round's free ones sent together in
  *      `dispatch:n`, a gated one in `dispatch:n:j` once approved; its answer waited for,
  *      then `expire:n:j` when none came and, if an edge had claimed it, `abandon:n:j`
  *      when none came again; the outcome kept by `tool:n:j`.
  *   1. `record-verdict` — when `topic` was offered: the verdict of the first reply, and
  *      what went wrong with the loop's `topic` calls ([[TurnVerdict.anomaly]]).
  *   1. Turns that passed the loop's patch before it shipped ([[Patches.Tools]]) took
  *      `call-model-again` and `call-model-plain` instead, when `topic` was offered: the
  *      model called again after its call to the tool, a plain call if that does not
  *      answer, then `record-verdict` ([[Step.optional]]).
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
  val Epoch = "2026-09-27"

  /** The turn's steps, as DBOS records their names, in the order they run. */
  object Step {
    val PinModels = "pin-models"
    val Offer = "offer"
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
        PinModels,
        Offer,
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

    /** The family of `record-call:n`, the tool loop's record of round n's reply that called
      * tools.
      */
    val RecordCall = "record-call"

    /** The family of `tool:n:j`, the tool loop's settling of call j of round n's reply. */
    val Tool = "tool"

    /** The family of `ask:n:j`, the tool loop's asking a person about call j of round n's
      * reply, a gated one, before it waits for their answer.
      */
    val Ask = "ask"

    /** The family of `dispatch:n`, round n's free hosted calls sent to an edge together, and
      * of `dispatch:n:j`, call j's, a gated one, sent once a person approved it.
      */
    val Dispatch = "dispatch"

    /** The family of `expire:n:j`, call j of round n given up on when no edge claimed it in
      * time, or found claimed and waited on again.
      */
    val Expire = "expire"

    /** The family of `abandon:n:j`, call j of round n given up on when its edge claimed it
      * and did not answer in time.
      */
    val Abandon = "abandon"

    /** The family of `wait:n:j`, a person's time to answer about call j of round n's reply:
      * not a step the turn records, but its wait between `ask:n:j` and the answer, which
      * DBOS records as [[Answered]] ([[named]] and [[running]] give it this name).
      */
    val Wait = "wait"

    /** The name DBOS records a wait for a message under once it ends, answered or run out
      * ([[grit.core.durable.Durable.recv]]).
      */
    val Answered = "DBOS.recv"

    /** The name of `round`'s record of its reply that called tools. */
    def recordCall(round: TurnLoop.Round): String = s"$RecordCall:${round.index}"

    /** The name of the step settling call `index` (from 0) of `round`'s reply. */
    def tool(round: TurnLoop.Round, index: Int): String = s"$Tool:${round.index}:$index"

    /** The name of the step asking about call `index` (from 0) of `round`'s reply. */
    def ask(round: TurnLoop.Round, index: Int): String = s"$Ask:${round.index}:$index"

    /** The name of the step sending `round`'s free hosted calls. */
    def dispatch(round: TurnLoop.Round): String = s"$Dispatch:${round.index}"

    /** The name of the step sending call `index` of `round`'s reply, a gated hosted one. */
    def dispatchOne(round: TurnLoop.Round, index: Int): String =
      s"$Dispatch:${round.index}:$index"

    /** The name of the step giving up on call `index` of `round` when no edge claimed it. */
    def expire(round: TurnLoop.Round, index: Int): String = s"$Expire:${round.index}:$index"

    /** The name of the step giving up on call `index` of `round` when its edge was silent. */
    def abandon(round: TurnLoop.Round, index: Int): String = s"$Abandon:${round.index}:$index"

    /** The step `name` stands for among [[all]], [[RecordCall]], [[Ask]], [[Wait]] and
      * [[Tool]]: its round and index dropped (`call-model:3` is [[CallModel]], `tool:1:0` is
      * [[Tool]]). `None` for a name that is not the turn's, such as a patch's marker or
      * DBOS's own records of a wait ([[Answered]] before [[named]] renames it).
      */
    def family(name: String): Option[String] =
      Loop.of(name) match {
        case Some(Loop.Call(_)) => Some(CallModel)
        case Some(Loop.Record(_)) => Some(RecordCall)
        case Some(Loop.Ask(_, _)) => Some(Ask)
        case Some(Loop.Wait(_, _)) => Some(Wait)
        case Some(Loop.Tool(_, _)) => Some(Tool)
        case Some(Loop.Sent(_) | Loop.SentOne(_, _)) => Some(Dispatch)
        case Some(Loop.Expired(_, _)) => Some(Expire)
        case Some(Loop.Abandoned(_, _)) => Some(Abandon)
        case None => Option.when(all.contains(name))(name)
      }

    /** The names of `recorded`, the steps a turn recorded in order, as a watcher is shown
      * them: an [[Answered]] that ends the wait after `ask:n:j` is `wait:n:j`; every other
      * name is as recorded.
      */
    def named(recorded: Vector[String]): Vector[String] =
      recorded
        .foldLeft((Vector.empty[String], Option.empty[Loop.Ask])) { case ((out, asked), name) =>
          (Loop.of(name), asked) match {
            case (Some(ask: Loop.Ask), _) => (out :+ name, Some(ask))
            case (_, Some(Loop.Ask(n, j))) if name == Answered => (out :+ s"$Wait:$n:$j", None)
            case _ if family(name).nonEmpty => (out :+ name, None)
            case _ => (out :+ name, asked)
          }
        }
        ._1
  }

  /** A step of the tool loop, by what its name says. */
  private enum Loop {
    case Call(round: Int)
    case Record(round: Int)
    case Ask(round: Int, index: Int)
    case Wait(round: Int, index: Int)
    case Tool(round: Int, index: Int)
    case Sent(round: Int)
    case SentOne(round: Int, index: Int)
    case Expired(round: Int, index: Int)
    case Abandoned(round: Int, index: Int)
  }

  private object Loop {
    private val CallName = """call-model:(\d+)""".r
    private val RecordName = """record-call:(\d+)""".r
    private val AskName = """ask:(\d+):(\d+)""".r
    private val WaitName = """wait:(\d+):(\d+)""".r
    private val ToolName = """tool:(\d+):(\d+)""".r
    private val SentName = """dispatch:(\d+)""".r
    private val SentOneName = """dispatch:(\d+):(\d+)""".r
    private val ExpiredName = """expire:(\d+):(\d+)""".r
    private val AbandonedName = """abandon:(\d+):(\d+)""".r

    def of(name: String): Option[Loop] = name match {
      case CallName(n) => n.toIntOption.map(Call(_))
      case RecordName(n) => n.toIntOption.map(Record(_))
      case AskName(n, j) => n.toIntOption.zip(j.toIntOption).map(Ask(_, _))
      case WaitName(n, j) => n.toIntOption.zip(j.toIntOption).map(Wait(_, _))
      case ToolName(n, j) => n.toIntOption.zip(j.toIntOption).map(Tool(_, _))
      case SentName(n) => n.toIntOption.map(Sent(_))
      case SentOneName(n, j) => n.toIntOption.zip(j.toIntOption).map(SentOne(_, _))
      case ExpiredName(n, j) => n.toIntOption.zip(j.toIntOption).map(Expired(_, _))
      case AbandonedName(n, j) => n.toIntOption.zip(j.toIntOption).map(Abandoned(_, _))
      case _ => None
    }
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

    /** The model's reply is a tool loop ([[TurnLoop]]): each model call a step of its own,
      * and each tool call too, the verdict read from the first call's reply
      * (2026-09-25).
      */
    val Tools = "tools"
  }

  /** The step a running turn is in, given the names of the steps it has `recorded` (a step
    * is recorded when it completes). After a loop's `record-call:n`, its first tool,
    * `tool:n:0`; after `ask:n:j`, `wait:n:j`, a person's time to answer ([[Step.Wait]]);
    * once the wait has ended, `tool:n:j`; after `tool:n:j`, the next model call,
    * `call-model:n+1`, which takes far longer than any further tool. Otherwise the next step
    * after the last recorded that every turn takes ([[Step.optional]] ones only once
    * recorded), `call-model:n` counting as `call-model`. Names that are not the turn's steps
    * are skipped; once the last step is recorded the turn is finishing, and that step is
    * named.
    */
  def running(recorded: Vector[String]): String = {
    val own = Step.named(recorded).filter(Step.family(_).nonEmpty)
    own.lastOption.flatMap(Loop.of) match {
      case Some(Loop.Record(n)) => s"${Step.Tool}:$n:0"
      case Some(Loop.Ask(n, j)) => s"${Step.Wait}:$n:$j"
      case Some(Loop.Wait(n, j)) => s"${Step.Tool}:$n:$j"
      case Some(Loop.Tool(n, _)) => s"${Step.CallModel}:${n + 1}"
      case Some(Loop.Sent(n)) => s"${Step.Tool}:$n:0"
      case Some(Loop.SentOne(n, j)) => s"${Step.Tool}:$n:$j"
      case Some(Loop.Expired(n, j)) => s"${Step.Tool}:$n:$j"
      case Some(Loop.Abandoned(n, j)) => s"${Step.Tool}:$n:$j"
      case _ =>
        val done =
          own.flatMap(Step.family).map(Step.all.indexOf).filter(_ >= 0).maxOption.getOrElse(-1)
        Step.all
          .drop(done + 1)
          .find(!Step.optional.contains(_))
          .orElse(Step.all.lastOption)
          .getOrElse(Step.Assemble)
    }
  }

  /** The turn workflow's body, for the turn whose workflow id is `workflowId`, its model
    * offered the tools its `offer` step recorded from `tooling`'s. Returns what the turn did,
    * for logs: its reply and summary are in the store, never in this string.
    */
  def body[C^](env: TurnEnv^, tooling: TurnTooling[C]^)(
      workflowId: WorkflowId
  )(using d: Durable^): String =
    TurnRef.fromWorkflowId(workflowId) match {
      case None => s"not a turn: ${WorkflowId.value(workflowId)}"
      case Some(turn) =>
        import TurnJournal.given
        d.transact(Step.PinModels)(pinModels(env, turn)) match {
          case Left(failure) => s"failed: $failure"
          case Right(profile) =>
            val hosting = env.hosting
            d.transact(Step.Offer)(TurnOffer.decide(hosting, tooling, turn))
              .flatMap(TurnOffer.load(hosting, env.db, _)) match {
              case Left(failure) => s"failed: $failure"
              case Right(offer) => pinned(env, tooling, turn)(using profile, offer, d)
            }
        }
    }

  /** The catalog in force, pinned as `turn`'s profile: kept by the store, and the step's
    * output, so a replay makes every call under the profile the turn started with.
    */
  private def pinModels(env: TurnEnv^, turn: TurnRef)(using Tx^): Either[TurnFailure, TurnProfile] =
    for {
      catalog <- env.models.catalog().left.map(why => TurnFailure.Model(s"no model catalog: $why"))
      profile = catalog.pin
      _ <- env.records.profiles.pin(turn.workflowId, profile).left.map(storeFailure)
    } yield profile

  /** [[body]] once `turn`'s models are pinned and its offer is made. */
  private def pinned[C^](env: TurnEnv^, tooling: TurnTooling[C]^, turn: TurnRef)(using
      pins: TurnProfile,
      offer: TurnOffer,
      d: Durable^
  ): String = {
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
          TurnTopics.record(records.entries, records.ledger, turn, c, env.clock.now())
        ).left
          .toOption
      case Placing.Unplaced => None
    }
    val ran = run(turn, placing, tooling)(using env, pins, offer, d)
    val topics = topicFailure.fold("")(f => s"; topics not recorded: $f") +
      ran.verdictUnrecorded.fold("")(f => s"; verdict not recorded: $f")
    ran.result match {
      case Left(failure) => s"failed: $failure$topics"
      case Right(reply) =>
        val summary =
          summarise(turn, reply, placing)(using env, pins, d) match {
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

  /** The steps from `assemble` to `append`: `turn`'s reply entry, or why it has none, its
    * model offered `tooling`'s tools.
    */
  private def run[C^](
      turn: TurnRef,
      placing: Placing,
      tooling: TurnTooling[C]^
  )(using env: TurnEnv^, pins: TurnProfile, offer: TurnOffer, d: Durable^): Ran[EntryId] = {
    import TurnJournal.given
    val heard = d.stream(TurnStream.Key)
    val prepared = for {
      window <- d.step(Step.Assemble) { () =>
        env.assembler.assemble(AssemblyRequest(turn))(using env.db).left.map {
          case AssemblyError.Store(error) => TurnFailure.Assembly(describe(error))
        }
      }
      recorded =
        if (d.patch(Patches.RecordWindow)) WindowRecord.OwnStep else WindowRecord.WithReply
      _ <- recorded match {
        case WindowRecord.OwnStep =>
          d.transact(Step.RecordWindow)(recordWindow(env.records, turn, window, env.clock.now()))
        case WindowRecord.WithReply => Right(windowId(turn))
      }
    } yield (window, recorded)
    prepared match {
      case Left(failure) => Ran(Left(failure), None)
      case Right((window, recorded)) =>
        val asked = asking(placing)
        if (d.patch(Patches.Tools)) loop(heard, turn, window, recorded, asked, tooling)
        else {
          val answered = d.step(Step.CallModel) { () =>
            callModel(heard, turn, window, asked.fold(Shape.Plain)(Shape.Offered(_)))
          } match {
            case Left(failure) => Ran(Left(failure), None)
            case Right(message) =>
              asked match {
                case None => Ran(Right(TurnVerdict.Replied(message, Shape.Plain)), None)
                case Some(c) => verdictRound(heard, turn, window, c, message)
              }
          }
          val appended = answered.result.flatMap { a =>
            val shape = a.shape
            d.transact(Step.Append)(
              append(
                offer.system,
                env.records,
                turn,
                window,
                a.reply,
                shape(_),
                recorded,
                env.clock.now()
              )
            )
          }
          Ran(appended, answered.verdictUnrecorded)
        }
    }
  }

  /** The steps from the first model call to `append` under the tool loop
    * ([[Patches.Tools]]): each call of the loop, `call-model` then `call-model:n`, offered
    * the turn's tools, and `topic` too when `asked` holds the classification the model is
    * asked about; each reply that called tools kept by `record-call:n`, and each of its
    * calls settled by `tool:n:j` ([[TurnTools]]); then `record-verdict` when `asked`, and
    * the answer appended as `turn`'s reply entry. The tools are `tooling`'s.
    */
  private def loop[C^](
      heard: StreamWriter^,
      turn: TurnRef,
      seen: Window,
      recorded: WindowRecord,
      asked: Option[TurnTopics.Classification],
      tooling: TurnTooling[C]^
  )(using env: TurnEnv^, pins: TurnProfile, offer: TurnOffer, d: Durable^): Ran[EntryId] =
    looping(
      heard,
      turn,
      seen,
      recorded,
      asked,
      TurnOffer.toolbox(tooling, offer),
      tooling.jot,
      tooling.budget,
      pins.turn.settings.strict == StrictSchemas.Enforced,
      tooling.answerWithin
    )

  /** [[loop]], offering `own`, the turn's tools, their results kept through `jot`, as
    * `budget` and `answerWithin` say ([[TurnTooling]]), their schemas `strict` when the turn's
    * pair is known to enforce them. A hosted call ([[grit.core.tool.Bound.Hosted]]) is sent to
    * the edge serving the offer's workspace ([[TurnHosted]]); every other runs in its step.
    */
  private def looping[C^](
      heard: StreamWriter^,
      turn: TurnRef,
      seen: Window,
      recorded: WindowRecord,
      asked: Option[TurnTopics.Classification],
      own: Toolbox[C],
      jot: Jot^,
      budget: TurnLoop.Budget,
      strict: Boolean,
      answerWithin: FiniteDuration
  )(using env: TurnEnv^, pins: TurnProfile, offer: TurnOffer, d: Durable^): Ran[EntryId] = {
    import TurnJournal.given
    offered(own, asked) match {
      case Left(failure) => Ran(Left(failure), None)
      case Right(tools) =>
        val schemas = tools.schemas(strict)
        // How this turn's pair is read and told: the pin's, fixed for the turn.
        val settings = pins.turn.settings
        val repairs = Repairs(settings.names, settings.repairs)
        val after = settings.afterResult
        val guidance = settings.guidance
        val hosting = env.hosting
        val workspace = offer.workspace
        // Whether each round's free hosted calls were sent to a serving edge, as its dispatch
        // step recorded: read by the round's settles, after its record.
        val dispatched = scala.collection.mutable.Map.empty[Int, Boolean]
        def shape(round: Round): ModelRequest -> ModelRequest =
          loopShape(asked, round, TurnLoop.use(budget, round), schemas, after, guidance)
        val moves = new TurnLoop.Moves {
          def call(round: Round, use: ToolUse): Either[TurnFailure, Message.Assistant] = {
            val shaped = loopShape(asked, round, use, schemas, after, guidance)
            d.step(round.step) { () => callShaped(heard, turn, seen, shaped) }
          }

          def record(
              round: Round,
              reply: Message.Assistant,
              calls: Vector[Pending]
          ): Either[TurnFailure, Unit] = {
            val shaped = shape(round)
            d.transact(Step.recordCall(round))(
              recordCall(
                offer.system,
                env.records,
                turn,
                seen,
                round,
                reply,
                shaped,
                env.clock.now()
              )
            ).flatMap { _ =>
              // Every free hosted call of the round goes to the edge at once, in one step.
              val free = calls.zipWithIndex.flatMap { (pending, index) =>
                TurnTools.read(tools, pending, repairs) match {
                  case Right(h: Bound.Hosted) if h.ask.isEmpty => Some((index, pending.call.id, h))
                  case _ => None
                }
              }
              TurnHosted.requests(turn, round, free, workspace) match {
                case Left(failure) => Left(failure)
                case Right(requests) if requests.isEmpty => Right(())
                case Right(requests) =>
                  d.transact(Step.dispatch(round))(TurnHosted.dispatch(hosting, requests)).map {
                    sent => dispatched.update(round.index, sent)
                  }
              }
            }
          }

          def settle(round: Round, index: Int, pending: Pending): Either[TurnFailure, Unit] = {
            val slot = TurnTools.Slot(turn, round, index)
            val call = pending.call.id
            val clock = env.clock
            val settling = new TurnTools.Settling(jot, env.records.entries)
            val settled = TurnTools.read(tools, pending, repairs) match {
              case Left(outcome) =>
                val named = pending.call.name
                d.step(slot.step) { () => settling.answer(slot, call, named, outcome, clock.now()) }
              case Right(free: Bound.Free) =>
                d.step(slot.step) { () => settling.run(slot, call, free, clock.now()) }
              case Right(gated: Bound.Gated) =>
                val (entries, shown) = (env.records.entries, gated.ask)
                d.transact(slot.askStep)(TurnTools.ask(entries, slot, call, shown, clock.now()))
                  .flatMap { _ =>
                    val received = d.recv(Approval.topic(call), answerWithin)
                    val approval = TurnTools.approval(received)
                    d.step(slot.step) { () =>
                      settling.decide(slot, call, gated, approval, clock.now())
                    }
                  }
              case Right(hosted: Bound.Hosted) =>
                val sent = dispatched.getOrElse(round.index, false)
                TurnHosted.settle(
                  hosting,
                  slot,
                  call,
                  hosted,
                  workspace,
                  sent,
                  answerWithin,
                  settling,
                  clock
                )
            }
            settled.map(_ => ())
          }
        }
        moves.call(Round.First, TurnLoop.use(budget, Round.First)) match {
          case Left(failure) => Ran(Left(failure), None)
          case Right(first) =>
            val looped = TurnLoop.from(budget, first, moves)
            val replies = looped.fold(_ => Vector(first), _.replies)
            val verdictUnrecorded = asked.flatMap { c =>
              d.transact(Step.RecordVerdict)(
                TurnTopics.writeEvents(
                  env.records.entries,
                  env.records.ledger,
                  turn,
                  TurnVerdict.verdictId(turn),
                  TurnVerdict
                    .events(TurnVerdict.of(c, first), TurnVerdict.anomaly(replies), c, turn),
                  None,
                  env.clock.now()
                )
              ).left
                .toOption
            }
            val appended = looped.flatMap { l =>
              val shaped = shape(l.round)
              d.transact(Step.Append)(
                append(
                  offer.system,
                  env.records,
                  turn,
                  seen,
                  l.answer,
                  shaped,
                  recorded,
                  env.clock.now()
                )
              )
            }
            Ran(appended, verdictUnrecorded)
        }
    }
  }

  /** The tools each call of the loop offers: `own`, after `topic` for the classification
    * `asked` holds. `topic` stays on after the first call, which alone asks for it
    * ([[loopShape]]): a request whose messages hold a call names the tool it called.
    * `TurnFailure.Model` when `own` already names one `topic`.
    */
  private def offered[C^](
      own: Toolbox[C],
      asked: Option[TurnTopics.Classification]
  ): Either[TurnFailure, Toolbox[C]] =
    asked match {
      case None => Right(own)
      case Some(c) =>
        own.including(TurnVerdict.tool(c)).left.map { case DuplicateName(name) =>
          TurnFailure.Model(s"two tools are named ${ToolName.value(name)}")
        }
    }

  /** How the loop's call `round` is built from the plain request: tagged for `asked` on the
    * first call, offering `tools` as `use` says, guided as [[TurnLoop.guided]] says under
    * `guidance`, and told as [[TurnLoop.told]] says under `after`.
    */
  private def loopShape(
      asked: Option[TurnTopics.Classification],
      round: Round,
      use: ToolUse,
      tools: Vector[ToolSchema],
      after: AfterToolResult,
      guidance: ToolGuidance
  ): ModelRequest -> ModelRequest =
    base => {
      val tagged = asked.filter(_ => round == Round.First).fold(base)(TurnVerdict.tagged(base, _))
      TurnLoop.told(use, TurnLoop.guided(tagged.copy(tools = tools, use = use), guidance), after)
    }

  /** The `record-call:n` step: `reply`, the reply to `round` that called tools, kept as
    * [[TurnTools.callId]] dated `at` after everything in the conversation, and what it cost
    * in the ledger beside the estimate of its request, rebuilt by `shape` from the same
    * window and entries.
    */
  private def recordCall(
      system: String,
      records: TurnRecords,
      turn: TurnRef,
      window: Window,
      round: Round,
      reply: Message.Assistant,
      shape: ModelRequest -> ModelRequest,
      at: Instant
  )(using Tx^): Either[TurnFailure, EntryId] = {
    val TurnRecords(entries, ledger, estimator, _, principals) = records
    val id = TurnTools.callId(turn, round)
    for {
      next <- entries.lockNext(turn.conversationId).left.map(storeFailure)
      all <- entries.list(turn.conversationId).left.map(storeFailure)
      near <- nearbyOf(entries, window).left.map(storeFailure)
      named <- principals.speakers(all.map(_.id)).left.map(storeFailure)
      sent <- requestOf(system, all, near, turn, window, named).map(shape)
      _ <- entries
        .insert(
          Entry(
            id,
            turn.conversationId,
            turn.turnSeq,
            None,
            next.seq,
            Payload.Exchange(reply),
            at
          )
        )
        .left
        .map(storeFailure)
      _ <- ledger
        .record(id, turn, turn.workflowId, reply.model, reply.usage, estimator.request(sent))
        .left
        .map(storeFailure)
    } yield id
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
  )(using
      env: TurnEnv^,
      pins: TurnProfile,
      offer: TurnOffer
  ): Either[TurnFailure, Message.Assistant] =
    callShaped(heard, turn, window, shape(_))

  /** As [[callModel]], the request shaped by `shape`. A provider that is
    * [[ProviderError.Unavailable]] is asked again after each wait of [[Retries]], each try a
    * fresh attempt on the stream; the last failure, or a [[ProviderError.Refused]], fails the
    * call.
    */
  private def callShaped(
      heard: StreamWriter^,
      turn: TurnRef,
      window: Window,
      shape: ModelRequest -> ModelRequest
  )(using
      env: TurnEnv^,
      pins: TurnProfile,
      offer: TurnOffer
  ): Either[TurnFailure, Message.Assistant] =
    request(env, offer.system, turn, window).flatMap { req =>
      val sent = shape(req)
      def attempt(
          waits: List[FiniteDuration],
          tries: Int
      ): Either[TurnFailure, Message.Assistant] = {
        val told = new TurnStream.Writer(heard, env.fresh.nonce(), () => env.clock.millis())
        val result = env.models.provider(pins.turn).stream(sent, told.tell)
        told.flush()
        (result, waits) match {
          case (Left(ProviderError.Unavailable(_)), wait :: rest) =>
            env.clock.sleep(wait)
            attempt(rest, tries + 1)
          case (Left(error), _) =>
            val after = if (tries == 1) "" else s" (after $tries tries)"
            Left(TurnFailure.Model(error.cause + after))
          case (Right(reply), _) => Right(reply)
        }
      }
      attempt(Retries, 1)
    }

  /** How long a model call waits before each retry of a provider that was
    * [[ProviderError.Unavailable]]: two retries, 2 s then 6 s, so three tries in all. A pinned
    * upstream has no fallback, and one 5xx would otherwise end a turn that had gone well.
    */
  val Retries: List[FiniteDuration] = List(2.seconds, 6.seconds)

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
  )(using
      env: TurnEnv^,
      pins: TurnProfile,
      offer: TurnOffer,
      d: Durable^
  ): Ran[TurnVerdict.Replied] = {
    import TurnJournal.given
    val further = new TurnVerdict.Calls {
      def again(shape: Shape.Again): Either[TurnFailure, Message.Assistant] =
        d.step(Step.CallModelAgain) { () => callModel(heard, turn, seen, shape) }
      def plain(): Either[TurnFailure, Message.Assistant] =
        d.step(Step.CallModelPlain) { () => callModel(heard, turn, seen, Shape.Plain) }
    }
    val round = TurnVerdict.round(c, first, further)
    val recorded = d.transact(Step.RecordVerdict)(
      recordVerdict(offer.system, env.records, turn, seen, c, round, env.clock.now())
    )
    Ran(round.answer, recorded.left.toOption)
  }

  /** The `record-verdict` step: where `round`'s verdict places `turn`'s message, recorded
    * as its entry [[TurnVerdict.verdictId]], dated `at`, with the round's anomaly, and what
    * the replies it spent cost in the ledger beside it.
    */
  private def recordVerdict(
      system: String,
      records: TurnRecords,
      turn: TurnRef,
      window: Window,
      c: TurnTopics.Classification,
      round: TurnVerdict.Round,
      at: Instant
  )(using Tx^): Either[TurnFailure, EntryId] =
    for {
      all <- records.entries.list(turn.conversationId).left.map(storeFailure)
      near <- nearbyOf(records.entries, window).left.map(storeFailure)
      named <- records.principals.speakers(all.map(_.id)).left.map(storeFailure)
      base <- requestOf(system, all, near, turn, window, named)
      id <- TurnTopics.writeEvents(
        records.entries,
        records.ledger,
        turn,
        TurnVerdict.verdictId(turn),
        TurnVerdict.events(round.verdict, round.anomaly, c, turn),
        TurnVerdict.cost(round.spent.map(r => r.reply -> records.estimator.request(r.shape(base)))),
        at
      )
    } yield id

  /** The last two steps: the summary of `turn`, whose reply is `answered`, with the name
    * and description of its message's topic when `placing` placed it.
    */
  private def summarise(
      turn: TurnRef,
      answered: EntryId,
      placing: Placing
  )(using env: TurnEnv^, pins: TurnProfile, d: Durable^): Either[TurnFailure, EntryId] = {
    import TurnJournal.given
    for {
      message <- d.step(Step.Summarise) { () =>
        env.db
          .read(env.records.entries.list(turn.conversationId))
          .left
          .map(storeFailure)
          .flatMap { all =>
            env.models
              .provider(pins.summary)
              .complete(summaryRequest(all, turn, placing))
              .left
              .map(e => TurnFailure.Model(e.cause))
          }
      }
      appended <- d.transact(Step.AppendSummary)(
        appendSummary(env.records, turn, answered, message, placing, env.clock.now())
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

  /** Records the summary in `message` as `turn`'s summary, dated `at`, a child of its reply
    * `answered`, after everything already in the conversation; and what it cost in the
    * ledger beside the estimate of the request that produced it, rebuilt as [[append]]
    * rebuilds its own.
    * What it says of the topic `placing` placed the message in is recorded after it
    * ([[TurnTopics.describedId]]).
    */
  private def appendSummary(
      records: TurnRecords,
      turn: TurnRef,
      answered: EntryId,
      message: Message.Assistant,
      placing: Placing,
      at: Instant
  )(using Tx^): Either[TurnFailure, EntryId] = {
    val id = TurnSummary.id(turn)
    val TurnRecords(entries, ledger, estimator, _, principals) = records
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
            at
          )
        )
        .left
        .map(storeFailure)
      _ <- ledger
        .record(
          id,
          turn,
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
        None,
        at
      )
    } yield id
  }

  /** `turn`'s own entries, from `all` of its conversation's. */
  private def own(all: Vector[Entry], turn: TurnRef): Vector[Entry] =
    all.filter(_.turnSeq == turn.turnSeq)

  /** The window's entries, then the turn's own, as one model request under `system`, read
    * through `env`'s `db`.
    */
  private def request(
      env: TurnEnv^,
      system: String,
      turn: TurnRef,
      window: Window
  ): Either[TurnFailure, ModelRequest] =
    env.db
      .read { (tx: Tx^) ?=>
        for {
          all <- env.records.entries.list(turn.conversationId)
          near <- nearbyOf(env.records.entries, window)
          named <- env.records.principals.speakers(all.map(_.id))
        } yield requestOf(system, all, near, turn, window, named)
      }
      .left
      .map(storeFailure)
      .flatten

  /** The entries of other conversations `window`'s nearby sections name that still exist.
    * One gone (its period closed and purged since the window was built) is left out.
    */
  private def nearbyOf(entries: EntryStore, window: Window)(using
      Tx^
  ): Either[StoreError, Vector[Entry]] =
    window.nearby
      .flatMap(_.entries)
      .foldLeft[Either[StoreError, Vector[Entry]]](Right(Vector.empty)) { (acc, id) =>
        acc.flatMap(found => entries.get(id).map(found ++ _))
      }

  /** The request [[request]] builds, from `all` of the conversation's entries and `near`,
    * the nearby entries that still exist: each nearby section as one user message
    * ([[Shown.nearby]], a section with none of its entries left dropped), then what the
    * model is shown of the window's entries ([[Shown.own]]: its messages, a closing entry
    * as one user message, and a gap line wherever turns are left out), then the turn's own, its tool loop's exchange among them in order.
    * A summary is not shown to the model yet.
    */
  private def requestOf(
      system: String,
      all: Vector[Entry],
      near: Vector[Entry],
      turn: TurnRef,
      window: Window,
      named: Speakers
  ): Either[TurnFailure, ModelRequest] = {
    val byId = all.map(e => e.id -> e).toMap
    val nearById = near.map(e => e.id -> e).toMap
    val sections =
      window.nearby.flatMap(n => Shown.nearby(n.place, n.entries.flatMap(nearById.get)))
    window.entries.filterNot(byId.contains) match {
      case missing if missing.nonEmpty =>
        Left(
          TurnFailure.Assembly(
            s"window names unknown entries: ${missing.map(EntryId.value).mkString(", ")}"
          )
        )
      case _ =>
        val shown = Shown.own(window.entries.flatMap(byId.get), turn.turnSeq, named)
        val mine = Shown.turn(own(all, turn), named)
        Right(ModelRequest(system, sections ++ shown ++ mine))
    }
  }

  /** Records `message` as `turn`'s reply entry ([[TurnRef.replyId]]), dated `at`, after everything
    * already in the conversation, and what it cost in the ledger beside the estimate of the
    * request that produced it: rebuilt from the same window and the same entries (the
    * turn's own were all recorded before its call, and the reply is not yet among them),
    * shaped by `shape`. When the window is `recorded` with the reply, its queries and the
    * window itself go in first ([[writeWindow]]).
    */
  private def append(
      system: String,
      records: TurnRecords,
      turn: TurnRef,
      window: Window,
      message: Message.Assistant,
      shape: ModelRequest -> ModelRequest,
      recorded: WindowRecord,
      at: Instant
  )(using Tx^): Either[TurnFailure, EntryId] = {
    val id = turn.replyId
    val TurnRecords(entries, ledger, estimator, _, principals) = records
    for {
      next <- entries.lockNext(turn.conversationId).left.map(storeFailure)
      all <- entries.list(turn.conversationId).left.map(storeFailure)
      near <- nearbyOf(entries, window).left.map(storeFailure)
      named <- principals.speakers(all.map(_.id)).left.map(storeFailure)
      sent <- requestOf(system, all, near, turn, window, named).map(shape)
      written <- recorded match {
        case WindowRecord.OwnStep => Right(0)
        case WindowRecord.WithReply => writeWindow(records, turn, window, next.seq, at)
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
            at
          )
        )
        .left
        .map(storeFailure)
      _ <- ledger
        .record(id, turn, turn.workflowId, message.model, message.usage, estimator.request(sent))
        .left
        .map(storeFailure)
    } yield id
  }

  /** The `record-window` step: `turn`'s window and the queries that chose it, recorded
    * dated `at` before the model is called, so an edge sees them while the turn runs.
    */
  private def recordWindow(
      records: TurnRecords,
      turn: TurnRef,
      window: Window,
      at: Instant
  )(using Tx^): Either[TurnFailure, EntryId] =
    for {
      next <- records.entries.lockNext(turn.conversationId).left.map(storeFailure)
      _ <- writeWindow(records, turn, window, next.seq, at)
    } yield windowId(turn)

  /** Writes each query `window`'s notes say assembly wrote, as [[queryId]] with its own
    * cost, then the window itself, as [[windowId]], from position `from`, dated `at`; how many
    * entries that was.
    */
  private def writeWindow(
      records: TurnRecords,
      turn: TurnRef,
      window: Window,
      from: Long,
      at: Instant
  )(using Tx^): Either[TurnFailure, Int] = {
    val TurnRecords(entries, ledger, _, _, _) = records
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
              at
            )
            entries
              .insert(entry)
              .flatMap(_ => ledger.record(qid, turn, turn.workflowId, q.model, q.usage, q.estimate))
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
            Payload.Window(window.entries, recalled, window.nearby),
            at
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
    case StoreError.Invalid(cause) => cause
  }
}
