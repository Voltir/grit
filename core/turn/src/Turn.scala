package grit.turn

import java.time.Instant

import scala.concurrent.duration.*

import grit.act.phase.{Asking, Calling, Hearing, Shaped, Shaping}
import grit.core.act.Posed
import grit.core.allot.{Allotment, Router}
import grit.core.context.{AssemblyError, AssemblyNote, AssemblyRequest, Shown, Window}
import grit.core.document.{DocLabel, DocText, Document, DocumentSearch, Pointer}
import grit.core.durable.{Durable, Journaled, StepRecord, StreamWriter}
import grit.core.edge.Permit
import grit.core.flow.{Extracted, Mint, Shape}
import grit.core.id.{CallSlot, EntryId, EntrySeq, ToolCallId, TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message, StopReason, Usage}
import grit.core.model.{
  AfterToolResult,
  Grade,
  Pinned,
  StrictSchemas,
  ToolGuidance,
  TurnProfile,
  TurnProfileId
}
import grit.core.period.{Balance, Closing, Flows}
import grit.core.place.Place
import grit.core.provider.{ModelRequest, ToolSchema, ToolUse}
import grit.core.route.{Routed, Routing, Uncleared}
import grit.core.speech.{Outcome, Speech, SpeechJson, SpeechStore}
import grit.core.stitch.{Along, Opening, Strand}
import grit.core.store.{
  Db,
  Entry,
  EntryStore,
  Jot,
  Nearby,
  Payload,
  Principals,
  Speakers,
  StoreError,
  Tx,
  UsageLedger
}
import grit.core.tool.{Bound, DuplicateName, Repairs, Tool, ToolName, ToolSet, Toolbox}
import grit.core.triage.{Tags, Weighing}
import grit.core.visibility.Integrity
import grit.core.visibility.{Classification, Label, Subject}

import TurnLoop.{Pending, Round}

// Where this is going: each step below is written in place, and each change to them is kept
// replayable by a patch of its own. The intent is a turn built from composable steps: a step as
// a value (its recorded name, its output's encoding, the capabilities it holds, whether it can
// end the turn, its patch), with combinators for sequence, patch-gated insertion, the tool loop
// and waits. A deployment or extension could then add a step at a declared point, and the step
// list, its doc and the replay checks would follow from one description. Until then, a new step
// takes one line in the doc below and puts its detail on its own type, and a design that adds a
// step says how it would fit that shape.

/** The durable turn: one workflow per turn. Each step's output is recorded, so a turn
  * resumed after a crash never calls a model twice. A step that fails returns a
  * [[TurnFailure]], recorded like any other output, so a rerun ends the same way without
  * calling anything. A failure before the reply ends the turn; a failed summary leaves the
  * reply standing, and the turn without a summary. Topics never fail a turn.
  *
  * Its steps, as [[Step]] names them, in order:
  *   1. `pin-models` — the catalog in force, pinned as the turn's profile, which a rerun runs
  *      under.
  *   1. `weigh` — the answers the turn's root is weighed with ([[TurnWeighing]]). Never fails
  *      the turn.
  *   1. `route` — the pin each role's calls are made under, and the effort laid over the turn's
  *      ([[Route]]); `Uncleared` when no turn pin is cleared for the turn.
  *   1. `offer` — what the turn offers its model ([[TurnOffer]]), and the cost of its weighing
  *      and of its task questions.
  *   1. `record-failure` — only when `pin-models`, `route` or `offer` failed, ending the turn
  *      ([[failedEarly]]): a heard message's turn keeps `Failed`; an uncleared one said to grit
  *      is replied [[Uncleared.reply]].
  *   1. `stitched` — an opening's placement waited for ([[grit.core.stitch.Placements.awaited]]).
  *      Never fails the turn.
  *   1. `classify`, `record-topic` — the message placed among its conversation's topics, and that
  *      placement recorded ([[TurnTopics]]).
  *   1. `assemble` — a fresh window over what came before the turn, at its offer's width.
  *   1. `record-window` — the window and its queries recorded before the model is called, so an
  *      edge sees what the model will see.
  *   1. `call-model` — the window, then the turn's own messages, sent to the provider, its reply
  *      told to edges as it arrives ([[TurnStream]]).
  *   1. The tool loop ([[TurnLoop]]): `record-call:n`, `ask:n:j`, `tool:n:j`, `dispatch:n`,
  *      `reach:n:{place}`, `dispatch:n:j`, `expire:n:j`, `abandon:n:j` and `call-model:n`, each
  *      named in [[Step]], until a reply calls no tool or the budget's last call. Each model
  *      call, `call-model` among them, follows its round's `gate:n` ([[Gated]]).
  *   1. `record-verdict` — when the classifier was unsure, the model's verdict ([[TurnVerdict]]).
  *   1. `append` — the reply recorded as the turn's entry, or as a draft when its root is a
  *      heard message it drafts a reply to ([[TurnOffer.Root]]).
  *   1. `judge` — an unprompted turn's draft scored against its thread ([[TurnJudge]]).
  *   1. `record-speech` — what becomes of a heard-rooted turn's draft
  *      ([[grit.core.speech.Speech]]), or `Failed` when the turn failed before it. Only a posted
  *      draft goes on.
  *   1. `summarise`, `append-summary` — the turn's summary ([[TurnSummary]]), recorded after the
  *      reply.
  */
object Turn {

  /** The turn's compatibility epoch (ADR 0004). Every turn in flight was started under an
    * epoch, and only an engine of the same epoch resumes it. Change the steps compatibly
    * with `Durable.patch` and keep the epoch; change the epoch only for a break a patch
    * cannot carry, which strands the turns in flight under the old one. `TurnReplayTests`
    * replays the histories recorded under this epoch.
    */
  val Epoch = "2026-10-14"

  /** The turn's steps, as DBOS records their names, in the order they run. */
  object Step {
    val PinModels = "pin-models"
    val Weigh = "weigh"
    val Route = "route"
    val Offer = "offer"
    val RecordFailure = "record-failure"
    val Stitched = "stitched"
    val Classify = "classify"
    val RecordTopic = "record-topic"
    val Assemble = "assemble"
    val RecordWindow = "record-window"
    val CallModel = "call-model"
    val RecordVerdict = "record-verdict"
    val Append = "append"
    val Judge = "judge"
    val RecordSpeech = "record-speech"
    val Summarise = "summarise"
    val AppendSummary = "append-summary"

    /** The steps only some turns take: `record-verdict`, a turn whose model was asked about
      * its topic; `judge`, an unprompted turn, and `record-speech`, a turn rooted on a heard
      * message; `record-failure`, a turn that failed before its offer was made or read.
      */
    val optional: Vector[String] =
      Vector(
        RecordFailure,
        RecordVerdict,
        Judge,
        RecordSpeech
      )

    val all: Vector[String] =
      Vector(
        PinModels,
        Weigh,
        Route,
        Offer,
        RecordFailure,
        Stitched,
        Classify,
        RecordTopic,
        Assemble,
        RecordWindow,
        CallModel,
        RecordVerdict,
        Append,
        Judge,
        RecordSpeech,
        Summarise,
        AppendSummary
      )

    /** The family of `record-call:n`, the tool loop's record of round n's reply that called
      * tools.
      */
    val RecordCall = "record-call"

    /** The family of `gate:n`, what round n's request shows and offers ([[Gated]]),
      * decided before its model call `call-model:n` (`call-model` for round 0).
      */
    val Gate = "gate"

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

    /** The family of `reach:n:{place}`, round n's free calls to the tools of a service the
      * turn reaches sent to the edge at its place (written), together.
      */
    val Reach = "reach"

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

    /** The name of `round`'s gate, before its model call. */
    def gate(round: TurnLoop.Round): String = s"$Gate:${round.index}"

    /** The name of the step settling call `index` (from 0) of `round`'s reply. */
    def tool(round: TurnLoop.Round, index: Int): String = s"$Tool:${round.index}:$index"

    /** The name of the step asking about call `index` (from 0) of `round`'s reply. */
    def ask(round: TurnLoop.Round, index: Int): String = s"$Ask:${round.index}:$index"

    /** The name of the step sending `round`'s free hosted calls. */
    def dispatch(round: TurnLoop.Round): String = s"$Dispatch:${round.index}"

    /** The name of the step sending `round`'s free calls to tools at `place`, a reached
      * service's.
      */
    def reach(round: TurnLoop.Round, place: Place): String =
      s"$Reach:${round.index}:${place.written}"

    /** The name of the step sending call `index` of `round`'s reply, a gated hosted one. */
    def dispatchOne(round: TurnLoop.Round, index: Int): String =
      s"$Dispatch:${round.index}:$index"

    /** The name of the step giving up on call `index` of `round` when no edge claimed it. */
    def expire(round: TurnLoop.Round, index: Int): String = s"$Expire:${round.index}:$index"

    /** The name of the step giving up on call `index` of `round` when its edge was silent. */
    def abandon(round: TurnLoop.Round, index: Int): String = s"$Abandon:${round.index}:$index"

    /** The step `name` stands for among [[all]] and the tool loop's families ([[Gate]],
      * [[RecordCall]], [[Ask]], [[Wait]], [[Tool]], [[Dispatch]], [[Reach]], [[Expire]],
      * [[Abandon]]): its
      * round, index or place dropped (`call-model:3` is [[CallModel]], `tool:1:0` is
      * [[Tool]]). `None` for a name that is not the turn's, such as a patch's marker or
      * DBOS's own records of a wait ([[Answered]] before [[named]] renames it).
      */
    def family(name: String): Option[String] =
      Loop.of(name) match {
        case Some(Loop.Call(_)) => Some(CallModel)
        case Some(Loop.Gate(_)) => Some(Gate)
        case Some(Loop.Record(_)) => Some(RecordCall)
        case Some(Loop.Ask(_, _)) => Some(Ask)
        case Some(Loop.Wait(_, _)) => Some(Wait)
        case Some(Loop.Tool(_, _)) => Some(Tool)
        case Some(Loop.Sent(_) | Loop.SentOne(_, _)) => Some(Dispatch)
        case Some(Loop.Reached(_)) => Some(Reach)
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
    case Gate(round: Int)
    case Record(round: Int)
    case Ask(round: Int, index: Int)
    case Wait(round: Int, index: Int)
    case Tool(round: Int, index: Int)
    case Sent(round: Int)
    case SentOne(round: Int, index: Int)
    case Reached(round: Int)
    case Expired(round: Int, index: Int)
    case Abandoned(round: Int, index: Int)
  }

  private object Loop {
    private val CallName = """call-model:(\d+)""".r
    private val GateName = """gate:(\d+)""".r
    private val RecordName = """record-call:(\d+)""".r
    private val AskName = """ask:(\d+):(\d+)""".r
    private val WaitName = """wait:(\d+):(\d+)""".r
    private val ToolName = """tool:(\d+):(\d+)""".r
    private val SentName = """dispatch:(\d+)""".r
    private val SentOneName = """dispatch:(\d+):(\d+)""".r
    private val ReachedName = """reach:(\d+):.+""".r
    private val ExpiredName = """expire:(\d+):(\d+)""".r
    private val AbandonedName = """abandon:(\d+):(\d+)""".r

    def of(name: String): Option[Loop] = name match {
      case CallName(n) => n.toIntOption.map(Call(_))
      case GateName(n) => n.toIntOption.map(Gate(_))
      case RecordName(n) => n.toIntOption.map(Record(_))
      case AskName(n, j) => n.toIntOption.zip(j.toIntOption).map(Ask(_, _))
      case WaitName(n, j) => n.toIntOption.zip(j.toIntOption).map(Wait(_, _))
      case ToolName(n, j) => n.toIntOption.zip(j.toIntOption).map(Tool(_, _))
      case SentName(n) => n.toIntOption.map(Sent(_))
      case SentOneName(n, j) => n.toIntOption.zip(j.toIntOption).map(SentOne(_, _))
      case ReachedName(n) => n.toIntOption.map(Reached(_))
      case ExpiredName(n, j) => n.toIntOption.zip(j.toIntOption).map(Expired(_, _))
      case AbandonedName(n, j) => n.toIntOption.zip(j.toIntOption).map(Abandoned(_, _))
      case _ => None
    }
  }

  /** The patches the turn's steps take within this epoch (ADR 0004). */
  object Patches {

    /** A turn that fails at `pin-models`, `route` or `offer` keeps its failure in
      * `record-failure`. Never retired: a rerun whose `pin-models` or `offer` output is read
      * back and then fails to load has recorded later steps at this point, does not take it,
      * and records nothing more.
      */
    val RecordFailure = "record-failure"

    /** A turn gates each model call by its integrity ([[Step.Gate]]), its rows believed at
      * it. Taken once, before the first `gate:0`: a turn that passed that point before it
      * shipped finishes ungated, every call stamped at [[Integrity.Authority]] and every tool
      * offered, its rows believed as its entries say ([[grit.core.store.EntryStore.believed]]).
      */
    val Integrity = "integrity"
  }

  /** The step a running turn is in, given the names of the steps it has `recorded` (a step
    * is recorded when it completes). After a loop's `record-call:n`, its first tool,
    * `tool:n:0`; after `ask:n:j`, `wait:n:j`, a person's time to answer ([[Step.Wait]]);
    * once the wait has ended, `tool:n:j`; after `tool:n:j`, the next model call,
    * `call-model:n+1`, which takes far longer than any further tool, and its gate; after
    * `gate:n`, the model call it gates. Otherwise the next step
    * after the last recorded that every turn takes ([[Step.optional]] ones only once
    * recorded), `call-model:n` counting as `call-model`. Names that are not the turn's steps
    * are skipped; once the last step is recorded the turn is finishing, and that step is
    * named.
    */
  def running(recorded: Vector[String]): String = {
    val own = Step.named(recorded).filter(Step.family(_).nonEmpty)
    own.lastOption.flatMap(Loop.of) match {
      case Some(Loop.Record(n)) => s"${Step.Tool}:$n:0"
      case Some(Loop.Gate(n)) => Round.at(n).step
      case Some(Loop.Ask(n, j)) => s"${Step.Wait}:$n:$j"
      case Some(Loop.Wait(n, j)) => s"${Step.Tool}:$n:$j"
      case Some(Loop.Tool(n, _)) => s"${Step.CallModel}:${n + 1}"
      case Some(Loop.Sent(n)) => s"${Step.Tool}:$n:0"
      case Some(Loop.Reached(n)) => s"${Step.Tool}:$n:0"
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
        d.transact(Step.PinModels, Subject.Turn(turn))(pinModels(env, turn))
          .flatMap(profileOf(env, _)) match {
          case Left(failure) => failedEarly(env, turn, failure)
          case Right(profile) =>
            // Taken by every turn, whatever the recipe, so a recovery under another
            // deployment's configuration replays the same steps.
            val weighed = d.step(Step.Weigh)(() => weigh(env, tooling, turn))
            // Taken as weigh is, so a recovery under another deployment's clearances replays it.
            d.step(Step.Route)(() => routeOf(env, turn, profile)) match {
              case Left(failure) => failedEarly(env, turn, failure)
              case Right(r) => offered(env, tooling, turn, weighed, r)(using Picked.of(r), d)
            }
        }
    }

  /** The `offer` step, its transaction bounded as `pins` says, then the rest of the turn under
    * `pins`.
    */
  private def offered[C^](
      env: TurnEnv^,
      tooling: TurnTooling[C]^,
      turn: TurnRef,
      weighed: Option[TurnWeighing.Weighed],
      routed: Route
  )(using pins: Picked, d: Durable^): String = {
    import TurnJournal.recordedOffer
    val hosting = env.hosting
    val ledger = env.records.ledger
    val speech = env.speech.store
    d.transact(Step.Offer, Subject.Turn(turn))(
      pins.known
        .flatMap(_ => speech.answering(turn))
        .left
        .map(storeFailure)
        .flatMap(answering =>
          TurnOffer.decide(
            hosting,
            env.records.entries,
            tooling,
            turn,
            weighed.flatMap(_.answers),
            answering,
            pins.at
          )
        )
        .flatMap(offered =>
          weighCost(ledger, env.classifier.upstream, turn, weighed)
            .flatMap(_ => taskCost(ledger, env.classifier.upstream, turn, routed))
            .map(_ => offered)
        )
    ).flatMap(TurnOffer.load(hosting, pins.reads(env.db).as(Subject.Turn(turn)), _)) match {
      case Left(failure) => failedEarly(env, turn, failure)
      case Right(offer) => pinned(env, tooling, turn)(using pins, offer, d)
    }
  }

  /** The `record-failure` step, for `turn`, which ended in `failure` before its offer was
    * made or read: `Failed` kept as what became of it when it answers a heard message
    * ([[grit.core.speech.SpeechStore.drafted]]), so its decision never stands drafting; for a
    * message said to grit, [[Uncleared.reply]] kept as its reply when `failure` is
    * `Uncleared`, and nothing otherwise. Taken by every turn that fails there, whatever it
    * answers, since which it answers is read inside the step.
    */
  private def failedEarly(env: TurnEnv^, turn: TurnRef, failure: TurnFailure)(using
      d: Durable^
  ): String = {
    import TurnJournal.given
    val (entries, store) = (env.records.entries, env.speech.store)
    val at = env.clock.now()
    val why = failure.toString
    if (!d.patch(Patches.RecordFailure)) s"failed: $failure"
    else
      d.transact(Step.RecordFailure, Subject.Turn(turn))(
        entries
          .ofTurn(turn)
          .flatMap(own =>
            own.minByOption(_.seq).map(_.payload) match {
              case Some(Payload.Heard(_)) => store.drafted(turn, Outcome.Failed(why), None, at)
              case _ =>
                failure match {
                  case TurnFailure.Uncleared(c) => replyUncleared(entries, turn, c, at)
                  case _ => Right(false)
                }
            }
          )
          .left
          .map(storeFailure)
      ) match {
        case Left(f) => s"failed: $failure; not recorded: $f"
        case Right(_) => s"failed: $failure"
      }
  }

  /** [[Uncleared.reply]] kept as `turn`'s reply, dated `at`: what a turn said to grit is
    * answered when no turn pin is cleared for `classification`. No call was made, so no cost.
    */
  private def replyUncleared(
      entries: EntryStore,
      turn: TurnRef,
      classification: Label,
      at: Instant
  )(using Tx^): Either[StoreError, Boolean] = {
    val reply = Message.Assistant(
      Vector(AssistantBlock.Text(Uncleared.reply(Uncleared(classification)))),
      StopReason.EndTurn,
      Usage.Zero,
      "grit" // no model wrote it
    )
    for {
      next <- entries.lockNext(turn.conversationId)
      level <- entries.believed(turn)
      _ <- entries.insert(
        Entry(
          turn.replyId,
          turn.conversationId,
          turn.turnSeq,
          None,
          next.seq,
          Payload.Message(reply),
          at,
          level
        )
      )
    } yield true
  }

  /** The `route` step: what a read for `turn` knows and the day's headroom, then each role's
    * pins cleared for that; the turn's, at `Low` headroom, at most [[Router.ceiling]]
    * ([[Routing.atMost]]). When those are of more than one grade the root is asked the task
    * questions, outside any transaction, and [[Router.allot]] says what grade the turn wants and
    * what it lays over its calls; the turn is the pin [[Routing.pick]] gives that grade, or,
    * with none wanted, the one a turn asked nothing takes ([[unasked]]), raised to the room's
    * baseline among the same pins ([[Routing.floored]]). Summary and query take
    * their first cleared pin. `Uncleared` when no turn pin is cleared; `Store` when the read
    * fails.
    */
  private def routeOf(
      env: TurnEnv^,
      turn: TurnRef,
      profile: TurnProfile
  ): Either[TurnFailure, Route] = {
    val (routing, now) = (env.routing, env.clock.now())
    env.db
      .read(Subject.Turn(turn)) {
        // Routed first: the spend read cannot raise what the turn is routed for.
        val routed = Routed.of
        // The turn's own room is its conversation's (Subject.Turn); a gone one has none.
        val baseline = Tx.clearance(summon[Tx^]).own.flatMap(o => Tx.baseline(o.room))
        routing.spending
          .on(routing.budget.today(now))
          .map(spent => (routed, Router.headroom(spent, routing.budget.cap), baseline))
      }
      .left
      .map(storeFailure)
      .flatMap { (routed, headroom, baseline) =>
        def cleared(pins: grit.core.model.Pins) = routed.cleared(pins)
        val label = routed.classification.label
        cleared(profile.turn)
          .toRight(TurnFailure.Uncleared(label))
          .map { all =>
            val pins = Router.ceiling(headroom).fold(all)(Routing.atMost(all, _))
            // Outside the read: no transaction is open while the root is asked.
            val task =
              if (pins.all.map(_.grade).distinct.sizeIs > 1)
                routing.tasks.task(turn).fold(Route.Task.Unanswered(_), Route.Task.Read(_))
              else Route.Task.Skipped
            val allotment = Router.allot(
              task match {
                case Route.Task.Read(asked) => Some(asked.features)
                case Route.Task.Skipped | Route.Task.Unanswered(_) => None
              },
              headroom
            )
            val picked = allotment.grade.fold(unasked(pins))(Routing.pick(pins, _))
            Route(
              label,
              baseline.fold(picked)(Routing.floored(pins, picked, _)),
              cleared(profile.summary).map(_.first),
              cleared(profile.query).map(_.first),
              headroom,
              task,
              allotment,
              baseline
            )
          }
      }
  }

  /** The pin a turn asked nothing, or wanting no grade, is routed to among `pins`, the turn
    * pins it may be routed to: the first.
    */
  private def unasked(pins: grit.core.model.Pins): Pinned = pins.first

  /** The cost of the task call `routed` made, when its root was asked and answered, recorded
    * in `ledger` under [[Route.id]] with its estimate, served by `upstream`; nothing otherwise.
    */
  private def taskCost(
      ledger: UsageLedger,
      upstream: Option[String],
      turn: TurnRef,
      routed: Route
  )(using Tx^): Either[TurnFailure, Unit] =
    routed.task match {
      case Route.Task.Read(asked) =>
        ledger
          .record(
            Route.id(turn),
            turn,
            turn.workflowId,
            asked.model,
            upstream,
            None,
            asked.usage,
            asked.estimate
          )
          .left
          .map(storeFailure)
      case Route.Task.Skipped | Route.Task.Unanswered(_) => Right(())
    }

  /** The `weigh` step: the tags live triage kept for the heard message `turn` answers
    * ([[grit.core.triage.TriageStore.of]]); for a message said to grit, when its offer would
    * read the answers ([[TurnOffer.weighs]]), what live triage's set makes of it
    * ([[TurnWeighing.said]]), or why asking it failed; [[Weighing.Unweighed.Unread]] when the
    * turn's own entries, its conversation or triage's tags cannot be read. `None` when nothing
    * is kept or asked. Never fails the turn: unweighed or failed, its offer withholds nothing
    * ([[grit.core.recipe.ServiceOffer.Verdict.Unweighed]]).
    */
  private def weigh[C^](
      env: TurnEnv^,
      tooling: TurnTooling[C]^,
      turn: TurnRef
  ): Option[TurnWeighing.Weighed] = {
    val (entries, conversations) = (env.records.entries, env.hosting.conversations)
    val (triage, said) = (env.weighing.triage, env.weighing.said)
    env.db
      .read(Subject.Turn(turn)) {
        for {
          own <- entries.ofTurn(turn)
          conversation <- conversations.get(turn.conversationId)
          kept <- own.minByOption(_.seq) match {
            case Some(root @ Entry(_, _, _, _, _, Payload.Heard(_), _, _)) =>
              triage.of(Vector(root.id)).map(_.get(root.id).map(TurnWeighing.Weighed.Kept(_)))
            case _ => Right(None)
          }
          addressed = own
            .minByOption(_.seq)
            .exists(_.payload match {
              case Payload.Message(Message.User(_)) => true
              case _ => false
            })
        } yield (kept, addressed && conversation.exists(c => TurnOffer.weighs(tooling, c.origin)))
      }
      .fold(
        _ => Some(TurnWeighing.Weighed.Failed(Weighing.Unweighed.Unread)),
        {
          case (Some(kept), _) => Some(kept)
          case (None, true) =>
            Some(
              said
                .weigh(turn)
                .fold(TurnWeighing.Weighed.Failed(_), TurnWeighing.Weighed.Asked(_))
            )
          case (None, false) => None
        }
      )
  }

  /** The cost of the classifier call `weighed` made, when the turn asked it, recorded in
    * `ledger` under [[TurnWeighing.id]] with its estimate, served by `upstream`; nothing
    * otherwise.
    */
  private def weighCost(
      ledger: UsageLedger,
      upstream: Option[String],
      turn: TurnRef,
      weighed: Option[TurnWeighing.Weighed]
  )(using Tx^): Either[TurnFailure, Unit] =
    weighed match {
      case Some(TurnWeighing.Weighed.Asked(Weighing.Weighed(tags, estimate))) =>
        ledger
          .record(
            TurnWeighing.id(turn),
            turn,
            turn.workflowId,
            tags.model,
            upstream,
            None,
            tags.usage,
            estimate
          )
          .left
          .map(storeFailure)
      case Some(TurnWeighing.Weighed.Kept(_) | TurnWeighing.Weighed.Failed(_)) | None =>
        Right(())
    }

  /** The `stitched` step: what the placement of `turn`'s message did, once it has ended, when
    * the message is an [[Opening]]; `None` when it is not, or is gone. `TurnFailure.Store`
    * when the conversation cannot be read or the placement failed.
    */
  private def placed(env: TurnEnv^, turn: TurnRef): Either[TurnFailure, Option[String]] = {
    val c = turn.conversationId
    val (entries, conversations) = (env.records.entries, env.hosting.conversations)
    env.db
      .read(Subject.Turn(turn)) {
        for {
          all <- entries.list(c)
          conversation <- conversations.get(c)
        } yield conversation.flatMap(Opening.of(_, all, turn))
      }
      .left
      .map(storeFailure)
      .flatMap {
        case None => Right(None)
        case Some(opening) =>
          env.stitching.placements
            .awaited(opening)
            .left
            .map(why => TurnFailure.Store(s"placement: $why"))
            .map(Some(_))
      }
  }

  /** The catalog in force, pinned as `turn`'s profile: kept by the store, and its id the
    * step's output, so a replay makes every call under the profile the turn started with.
    */
  private def pinModels(env: TurnEnv^, turn: TurnRef)(using
      Tx^
  ): Either[TurnFailure, TurnProfileId] =
    for {
      catalog <- env.models.catalog().left.map(why => TurnFailure.Model(s"no model catalog: $why"))
      profile = catalog.pin
      _ <- env.records.profiles.pin(turn.workflowId, profile).left.map(storeFailure)
    } yield profile.id

  /** The profile kept under `id`, read through `env`'s `db`; `TurnFailure.Store` when none
    * is.
    */
  private def profileOf(env: TurnEnv^, id: TurnProfileId): Either[TurnFailure, TurnProfile] =
    env.db
      .read(Subject.Public)(env.records.profiles.get(id))
      .left
      .map(storeFailure)
      .flatMap(_.toRight(TurnFailure.Store(s"no profile kept under ${TurnProfileId.value(id)}")))

  /** [[body]] once `turn`'s models are pinned and its offer is made. */
  private def pinned[C^](env: TurnEnv^, tooling: TurnTooling[C]^, turn: TurnRef)(using
      pins: Picked,
      offer: TurnOffer,
      d: Durable^
  ): String = {
    import TurnJournal.given
    val records = env.records
    val stitched = d.step(Step.Stitched)(() => placed(env, turn)) match {
      case Right(_) => ""
      case Left(failure) => s"; not placed: $failure"
    }
    val placing = d.step(Step.Classify) { () =>
      TurnTopics.classify(env.classifier, records.entries, env.db, records.estimator, turn)
    }
    val topicFailure =
      d.transact(Step.RecordTopic, Subject.Turn(turn))(
        TurnTopics.record(
          records.entries,
          records.ledger,
          env.classifier.upstream,
          turn,
          placing,
          env.clock.now()
        )
      ).left
        .toOption
    val ran = run(turn, placing, tooling)(using env, pins, offer, d)
    val topics = stitched + topicFailure.fold("")(f => s"; topics not recorded: $f") +
      ran.verdictUnrecorded.fold("")(f => s"; verdict not recorded: $f")
    (ran.result, offer.root) match {
      case (Left(failure), TurnOffer.Root.Addressed) => s"failed: $failure$topics"
      case (Left(failure), TurnOffer.Root.Heard | TurnOffer.Root.Named | TurnOffer.Root.ByName) =>
        val at = env.clock.now()
        val speech = env.speech
        val settled = d.transact(Step.RecordSpeech, Subject.Turn(turn))(
          speech.store
            .drafted(turn, Outcome.Failed(failure.toString), None, at)
            .left
            .map(storeFailure)
            .map(_ => Outcome.Failed(failure.toString))
        )
        s"failed: $failure$topics${settled.fold(f => s"; not recorded: $f", _ => "")}"
      case (Right(draft), root @ (TurnOffer.Root.Heard | TurnOffer.Root.Named)) =>
        val settling = root match {
          case TurnOffer.Root.Named => Settling.Named
          case _ => Settling.Judged(d.step(Step.Judge)(() => judgeDraft(env, turn)))
        }
        val at = env.clock.now()
        d.transact(Step.RecordSpeech, Subject.Turn(turn))(
          recordSpeech(env, turn, settling, ran.level, at)
        ) match {
          case Left(failure) => s"drafted: ${EntryId.value(draft)}; not settled: $failure$topics"
          case Right(Outcome.Posted(_)) =>
            val summary =
              summarise(turn, turn.replyId, ran.level)(using env, pins, d) match {
                case Right(id) => s"summarised: ${EntryId.value(id)}"
                case Left(failure) => s"no summary: $failure"
              }
            s"posted: ${EntryId.value(turn.replyId)}; $summary$topics"
          case Right(other) =>
            s"drafted: ${EntryId.value(draft)}; ${SpeechJson.outcomeName(other)}$topics"
        }
      case (Right(reply), TurnOffer.Root.Addressed | TurnOffer.Root.ByName) =>
        val summary =
          summarise(turn, reply, ran.level)(using env, pins, d) match {
            case Right(id) => s"summarised: ${EntryId.value(id)}"
            case Left(failure) => s"no summary: $failure"
          }
        s"replied: ${EntryId.value(reply)}; $summary$topics"
    }
  }

  /** The `judge` step: what the classifier makes of `turn`'s draft against its thread and
    * what its recorded window recalled ([[TurnJudge]]); `Passed`, asking nothing, when the
    * draft passes; `Unjudged` when the store cannot be read or the draft is not there.
    */
  private def judgeDraft(env: TurnEnv^, turn: TurnRef): TurnJudge.Judgement = {
    val records = env.records
    val stitching = env.stitching
    val conversations = env.hosting.conversations
    val now = env.clock.now()
    env.db
      .read(Subject.Turn(turn)) { (tx: Tx^) ?=>
        for {
          all <- records.entries.list(turn.conversationId)
          window = all.collectFirst {
            case Entry(id, _, _, _, _, Payload.Window(entries, _, nearby, _), _, _)
                if id == windowId(turn) =>
              Window(entries, Vector.empty, nearby)
          }
          near <- window.fold[Either[StoreError, Vector[Entry]]](Right(Vector.empty))(w =>
            Nearby.read(w.nearby, records.entries)
          )
          strand <- strandOf(stitching, conversations, turn, all, now)
          named <- records.principals.speakers((all ++ strand.shown).map(_.id))
          // Routed by the read that built the state: the classifier is asked only when its
          // service is cleared for it.
        } yield (Routed.of, Judging(all, window, near, strand, named))
      } match {
      case Left(e) => TurnJudge.Judgement.Unjudged(s"store: ${describe(e)}")
      case Right((routed, Judging(all, window, near, strand, named))) =>
        (
          all.filter(_.turnSeq == turn.turnSeq).minByOption(_.seq),
          all.collectFirst {
            case Entry(id, _, _, _, _, Payload.Draft(m), _, _) if id == turn.draftId => m
          }
        ) match {
          case (Some(_), Some(draft)) =>
            TurnJudge.said(draft) match {
              case None => TurnJudge.Judgement.Passed
              case Some(text) =>
                TurnJudge.judge(
                  env.classifier.asking(routed),
                  records.estimator,
                  TurnJudge.state(
                    all,
                    window.getOrElse(Window(Vector.empty)),
                    near,
                    strand,
                    stitching.tuning.strandChars,
                    named,
                    text
                  )
                )
            }
          case _ => TurnJudge.Judgement.Unjudged("the draft is not there")
        }
    }
  }

  /** What the `judge` step reads. */
  private final case class Judging(
      all: Vector[Entry],
      window: Option[Window],
      near: Vector[Entry],
      strand: Strand.Read,
      named: grit.core.store.Speakers
  )

  /** What `turn`'s conversation's strand said from the horizon before its first entry
    * (`all`'s) until `until`, in the scope in force ([[Along.read]]); nothing for a
    * conversation gone.
    */
  private def strandOf(
      stitching: TurnStitching^,
      conversations: grit.core.store.ConversationStore,
      turn: TurnRef,
      all: Vector[Entry],
      until: Instant
  )(using Tx^): Either[StoreError, Strand.Read] =
    for {
      conversation <- conversations.get(turn.conversationId)
      settings <- stitching.lifecycle.current()
      read <- conversation.fold[Either[StoreError, Strand.Read]](Right(Strand.Read.empty)) { c =>
        val began = all.minByOption(_.seq).fold(until)(_.createdAt)
        Along.read(
          stitching.stitches,
          c,
          settings.locality.scope,
          began.minusNanos(stitching.tuning.horizon.toNanos),
          until
        )
      }
    } yield read

  /** How a heard-rooted turn's draft is settled: an unprompted one by its `judgement`; a named
    * one unjudged.
    */
  private enum Settling {
    case Judged(judgement: TurnJudge.Judgement)
    case Named
  }

  /** The `record-speech` step: what becomes of `turn`'s draft, as `settling` and the
    * speaking in force say (an unprompted draft by [[Speech.post]], a named one, unless it
    * passes, by [[Speech.postNamed]]), unless the assistant already replied after its root
    * ([[Speech.spoken]]); the judge's cost in the ledger; a posted draft written as the
    * turn's reply and awaited at the address its heard message was heard with; and the
    * outcome kept ([[grit.core.speech.SpeechStore.drafted]]), dated `at`. A post with no
    * address to go to is `Failed`.
    */
  private def recordSpeech(
      env: TurnEnv^,
      turn: TurnRef,
      settling: Settling,
      believed: Option[Integrity],
      at: Instant
  )(using Tx^): Either[TurnFailure, Outcome] = {
    val TurnRecords(entries, ledger, _, _, _, _) = env.records
    val TurnSpeech(speaking, store, deliveries) = env.speech
    for {
      all <- entries.list(turn.conversationId).left.map(storeFailure)
      root <- all
        .filter(_.turnSeq == turn.turnSeq)
        .minByOption(_.seq)
        .toRight(TurnFailure.Store(s"${WorkflowId.value(turn.workflowId)} has no entries"))
      draft <- all
        .collectFirst {
          case Entry(id, _, _, _, _, Payload.Draft(m), _, _) if id == turn.draftId => m
        }
        .toRight(TurnFailure.Store(s"${WorkflowId.value(turn.workflowId)} has no draft"))
      reach <- store.reach(turn).left.map(storeFailure)
      // The strand after the root, to now: the assistant's reply there holds the draft too.
      strand <- strandOf(
        env.stitching,
        env.hosting.conversations,
        turn,
        all,
        at.plusSeconds(1)
      ).left
        .map(storeFailure)
      judged = Speech
        .spoken(root, all, strand.said.map(_.entry))
        .getOrElse(settling match {
          case Settling.Judged(TurnJudge.Judgement.Passed) => Outcome.Passed
          case Settling.Judged(TurnJudge.Judgement.NothingRecalled) => Outcome.NothingRecalled
          case Settling.Judged(TurnJudge.Judgement.Unjudged(why)) =>
            Speech.post(speaking, Left(why))
          case Settling.Judged(TurnJudge.Judgement.Scored(j, _)) =>
            Speech.post(speaking, Right(j))
          case Settling.Named =>
            TurnJudge.said(draft).fold(Outcome.Passed)(_ => Speech.postNamed(speaking))
        })
      outcome = (judged, reach.flatMap(_.replyTo)) match {
        case (Outcome.Posted(_), None) => Outcome.Failed("no address to reply to")
        case (o, _) => o
      }
      called = settling match {
        case Settling.Judged(TurnJudge.Judgement.Scored(j, estimated)) =>
          Some(TurnJudge.Call(j.model, j.usage, estimated))
        case Settling.Judged(_) | Settling.Named => None
      }
      _ <- called.fold[Either[TurnFailure, Unit]](Right(()))(c =>
        ledger
          .record(
            TurnJudge.id(turn),
            turn,
            turn.workflowId,
            c.model,
            env.classifier.upstream,
            None,
            c.usage,
            c.estimated
          )
          .left
          .map(storeFailure)
      )
      _ <- (outcome, reach.flatMap(_.replyTo)) match {
        case (Outcome.Posted(_), Some(to)) =>
          for {
            next <- entries.lockNext(turn.conversationId).left.map(storeFailure)
            level <- believedAt(entries, turn, believed)
            _ <- entries
              .insert(
                Entry(
                  turn.replyId,
                  turn.conversationId,
                  turn.turnSeq,
                  None,
                  next.seq,
                  Payload.Message(draft),
                  at,
                  level
                )
              )
              .left
              .map(storeFailure)
            _ <- deliveries.await(turn, to).left.map(storeFailure)
          } yield ()
        case _ => Right(())
      }
      _ <- store.drafted(turn, outcome, TurnJudge.said(draft), at).left.map(storeFailure)
    } yield outcome
  }

  /** The id of the `i`-th search query assembly wrote for `turn`, from 0. */
  def queryId(turn: TurnRef, i: Int): EntryId = {
    val base = s"query:${WorkflowId.value(turn.workflowId)}"
    EntryId(if (i == 0) base else s"$base:$i")
  }

  /** The id of the record of `turn`'s window. */
  def windowId(turn: TurnRef): EntryId = EntryId(s"window:${WorkflowId.value(turn.workflowId)}")

  /** The pin each role's calls are made under, the classification each read whose content a
    * model is sent is bounded by, and the turn loop's calls: a routed turn's [[Route]], its
    * allotment laid over its turn pin's effort and its calls, a summary uncleared failing
    * `Uncleared`.
    */
  private[turn] final case class Picked(
      turn: Pinned,
      summary: Either[TurnFailure, Pinned],
      query: Option[Pinned],
      bound: Option[Label],
      calls: Allotment.Calls
  ) {

    /** `db`, its reads bounded as this says ([[Db.within]]). */
    def reads(db: Db^): Db^ = bound match {
      case Some(b) => Db.within(db, b)
      case None => db
    }

    /** Whether a transaction knows no more than this is bounded by ([[Tx.within]]). */
    def known(using Tx^): Either[StoreError, Unit] = bound.fold(Right(()))(Tx.within(_))

    /** What the turn sends and writes out at in a transaction: a routed turn's classification
      * held while it runs ([[Tx.locked]]), else what the transaction knows.
      */
    def at(using Tx^): Classification = bound.fold(Tx.classification)(Tx.locked(_))
  }

  private[turn] object Picked {
    def of(r: Route): Picked =
      Picked(
        r.turn.copy(assignment =
          r.turn.assignment.copy(effort = r.allotment.reasoning.over(r.turn.assignment.effort))
        ),
        r.summary.toRight(TurnFailure.Uncleared(r.classification)),
        r.query,
        Some(r.classification),
        r.allotment.calls
      )
  }

  /** `c`, when the classifier was unsure: the classification the model is asked about. */
  private def asked(c: TurnTopics.Classification): Option[TurnTopics.Classification] =
    Option.when(c.uncertain)(c)

  /** What steps came to, `result`; the verdict's record when it failed without failing
    * the turn; and the turn's level at its end, `None` when it is ungated ([[believedAt]]).
    */
  private final case class Ran[A](
      result: Either[TurnFailure, A],
      verdictUnrecorded: Option[TurnFailure],
      level: Option[Integrity]
  )

  /** The steps from `assemble` to `append`: `turn`'s reply entry, or why it has none, its
    * model offered `tooling`'s tools.
    */
  private def run[C^](
      turn: TurnRef,
      placing: TurnTopics.Classification,
      tooling: TurnTooling[C]^
  )(using env: TurnEnv^, pins: Picked, offer: TurnOffer, d: Durable^): Ran[EntryId] = {
    import TurnJournal.given
    val heard = d.stream(TurnStream.Key)
    val prepared = for {
      window <- d.step(Step.Assemble) { () =>
        env.assembler
          .assemble(AssemblyRequest(turn, pins.query, offer.width))(using pins.reads(env.db))
          .left
          .map { case AssemblyError.Store(error) =>
            TurnFailure.Assembly(describe(error))
          }
      }
      _ <- d.transact(Step.RecordWindow, Subject.Turn(turn))(
        recordWindow(env.records, turn, window, env.clock.now())
      )
    } yield window
    prepared match {
      case Left(failure) => Ran(Left(failure), None, None)
      case Right(window) => loop(heard, turn, window, asked(placing), tooling)
    }
  }

  /** The steps from the first model call to `append`, the tool loop: each call of the loop,
    * `call-model` then `call-model:n`, offered the turn's tools, and `topic` too when `asked` holds the classification the model is
    * asked about; each reply that called tools kept by `record-call:n`, and each of its
    * calls settled by `tool:n:j` ([[TurnTools]]); then `record-verdict` when `asked`, and
    * the answer appended as `turn`'s reply entry. The tools are `tooling`'s.
    */
  private def loop[C^](
      heard: StreamWriter^,
      turn: TurnRef,
      seen: Window,
      asked: Option[TurnTopics.Classification],
      tooling: TurnTooling[C]^
  )(using env: TurnEnv^, pins: Picked, offer: TurnOffer, d: Durable^): Ran[EntryId] =
    looping(
      heard,
      turn,
      seen,
      asked,
      TurnOffer.toolbox(tooling, offer),
      tooling.jot,
      TurnLoop.Budget.allotted(tooling.budget, pins.calls),
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
      asked: Option[TurnTopics.Classification],
      own: Toolbox[C],
      jot: Jot^,
      budget: TurnLoop.Budget,
      strict: Boolean,
      answerWithin: FiniteDuration
  )(using env: TurnEnv^, pins: Picked, offer: TurnOffer, d: Durable^): Ran[EntryId] = {
    import TurnJournal.given
    offered(own, asked) match {
      case Left(failure) => Ran(Left(failure), None, None)
      case Right(tools) =>
        val schemas = tools.schemas(strict)
        // How this turn's pair is read and told: the pin's, fixed for the turn.
        val settings = pins.turn.settings
        val repairs = Repairs(settings.names, settings.repairs)
        val after = settings.afterResult
        val guidance = settings.guidance
        val hosting = env.hosting
        val workspace = offer.workspace
        // Whether each round's free hosted calls to each place were sent to a serving edge, as
        // its dispatch or reach step recorded: read by the round's settles, after its record.
        val dispatched = scala.collection.mutable.Map.empty[(Int, Place), Boolean]
        // Each round's gate as its gate:n step recorded it, by round: what its request offers
        // and says, read by every step that rebuilds that request.
        val gates = scala.collection.mutable.Map.empty[Int, Gated]
        val offering = tools.set
        def gate(round: Round, prior: Option[Integrity]): Either[TurnFailure, Gated] = {
          // The name each item hidden so far was given, kept for as long as it stays hidden.
          val named = gates.values.flatMap(_.hidden.map((item, h) => item.ref -> h.name)).toMap
          d.step(Step.gate(round)) { () =>
            gateOf(env, turn, seen, offer.root, offering, prior, named)
          }.map { g =>
            gates.update(round.index, g)
            g
          }
        }
        // How extract puts its questions: to the summary's pin, over what the window shows.
        val extracting = extraction(env, jot, turn, seen)
        // What a call of `round` binds against: `tools`, after read_hidden and extract over the
        // items the round's gate hid when the round offers them. Their names are free: offered
        // checked them.
        def boxAt(round: Round): Toolbox[caps.CapSet^{C, extracting}] =
          if (!hidSince(gates.toMap, round)) tools
          else {
            val g = gates.get(round.index)
            Toolbox
              .of[caps.CapSet^{extracting}](ReadHidden.tool(g), Extract.tool(g, extracting))
              .flatMap(hidden => Toolbox.joined[caps.CapSet^{C, extracting}](hidden, tools))
              .getOrElse(tools)
          }
        def shapeAs(round: Round, use: ToolUse): ModelRequest -> ModelRequest = {
          val (shown, line) = gatedOffer(gates.toMap, round, offering, schemas, strict)
          loopShape(asked, round, use, shown, after, guidance, line)
        }
        def shape(round: Round): ModelRequest -> ModelRequest =
          shapeAs(round, TurnLoop.use(budget, round))
        val gating = d.patch(Patches.Integrity)
        val first: Either[TurnFailure, Option[Gated]] =
          if (gating) gate(Round.First, None).map(Some(_)) else Right(None)
        // The turn's level: from its first gate's, lowered by each later gate's after its step.
        // An ungated turn's stays at Authority, so its calls bind to any tool whose requirement
        // names no compartment, as every turn's did before calls were stamped. A turn whose
        // first gate failed ends before anything is stamped.
        val cell = Mint.taint(first.toOption.flatten.fold(Integrity.Authority)(_.level))
        def believed: Option[Integrity] = Option.when(gating)(cell.level)
        val moves = new TurnLoop.Moves {
          def call(round: Round, use: ToolUse): Either[TurnFailure, Message.Assistant] = {
            val gated =
              if (!gating || round.index == Round.First.index) Right(())
              else gate(round, Some(cell.level)).map(g => cell.lower(g.level))
            gated.flatMap { _ =>
              val shaped = shapeAs(round, use)
              d.step(round.step) { () =>
                callShaped(heard, turn, seen, stubsAt(gates.toMap, round, offering), shaped)
              }
            }
          }

          def record(
              round: Round,
              reply: Message.Assistant,
              calls: Vector[Pending]
          ): Either[TurnFailure, Unit] = {
            val shaped = shape(round)
            d.transact(Step.recordCall(round), Subject.Turn(turn))(
              recordCall(
                offer.system,
                env.records,
                turn,
                seen,
                stubsAt(gates.toMap, round, offering),
                round,
                reply,
                pins.turn.grade,
                shaped,
                believed,
                env.clock.now()
              )
            ).flatMap { _ =>
              // Every free hosted call of the round goes to the edge at once, in one step.
              val free = calls.zipWithIndex.foldLeft[Either[TurnFailure, Vector[
                (TurnTools.Slot, ToolCallId, Bound.Hosted)
              ]]](Right(Vector.empty)) { case (acc, (pending, index)) =>
                TurnTools.read(
                  boxAt(round),
                  pending,
                  repairs,
                  pins.reads(env.db).as(Subject.Turn(turn)),
                  cell.level
                ) match {
                  case Right(h: Bound.Hosted) if h.ask.isEmpty =>
                    for {
                      done <- acc
                      slot <- TurnTools.Slot.of(turn, round, index)
                    } yield done :+ ((slot, pending.call.id, h))
                  case _ => acc
                }
              }
              // The workspace's go in `dispatch:n`, each reached place's in its `reach:n:place`.
              free.flatMap { free =>
                val groups = TurnHosted.requests(free, offer)
                groups.foldLeft[Either[TurnFailure, Unit]](Right(())) { case (acc, (place, qs)) =>
                  acc.flatMap { _ =>
                    val step =
                      if (workspace.contains(place)) Step.dispatch(round)
                      else Step.reach(round, place)
                    d.transact(step, Subject.Turn(turn))(
                      TurnHosted.dispatch(hosting, turn, place, qs, Permit.Free, pins)
                    ).map { sent =>
                      dispatched.update((round.index, place), sent)
                    }
                  }
                }
              }
            }
          }

          def settle(round: Round, index: Int, pending: Pending): Either[TurnFailure, Unit] = {
            TurnTools.Slot.of(turn, round, index).flatMap { slot =>
              val call = pending.call.id
              val clock = env.clock
              val settling = new TurnTools.Settling(jot, env.records.entries)
              val settled = TurnTools.read(
                boxAt(round),
                pending,
                repairs,
                pins.reads(env.db).as(Subject.Turn(turn)),
                cell.level
              ) match {
                case Left(outcome) =>
                  // A refusal is grit's text and what the call sent: believed as the call was.
                  val (named, level) = (pending.call.name, cell.level)
                  d.step(slot.step) { () =>
                    settling.answer(slot, call, named, outcome, Left(level), clock.now())
                  }
                case Right(free: Bound.Free) =>
                  val level = cell.level
                  d.step(slot.step) { () => settling.run(slot, call, free, level, clock.now()) }
                case Right(gated: Bound.Gated) =>
                  val (entries, shown, level) = (env.records.entries, gated.ask, cell.level)
                  d.transact(slot.askStep, Subject.Turn(turn))(
                    TurnTools.ask(entries, slot, call, shown, clock.now())
                  ).flatMap { _ =>
                    val approval = Calling.approval(call, answerWithin)(using d)
                    d.step(slot.step) { () =>
                      settling.decide(slot, call, gated, approval, level, clock.now())
                    }
                  }
                case Right(hosted: Bound.Hosted) =>
                  val place = offer.placeOf(hosted.tool)
                  val sent = place.exists(p => dispatched.getOrElse((round.index, p), false))
                  TurnHosted.settle(
                    hosting,
                    slot,
                    call,
                    hosted,
                    place,
                    sent,
                    answerWithin,
                    settling,
                    clock,
                    pins,
                    cell.level
                  )
              }
              settled.map(_ => ())
            }
          }
        }
        first.flatMap(_ => moves.call(Round.First, TurnLoop.use(budget, Round.First))) match {
          case Left(failure) => Ran(Left(failure), None, None)
          case Right(first) =>
            val looped = TurnLoop.from(budget, first, moves)
            val replies = looped.fold(_ => Vector(first), _.replies)
            val verdictUnrecorded = asked.flatMap { c =>
              d.transact(Step.RecordVerdict, Subject.Turn(turn))(
                TurnTopics.writeEvents(
                  env.records.entries,
                  env.records.ledger,
                  turn,
                  TurnVerdict.verdictId(turn),
                  TurnVerdict
                    .events(TurnVerdict.of(c, first), TurnVerdict.anomaly(replies), c, turn),
                  None,
                  None,
                  env.clock.now()
                )
              ).left
                .toOption
            }
            val appended = looped.flatMap { l =>
              val shaped = shape(l.round)
              d.transact(Step.Append, Subject.Turn(turn))(
                append(
                  offer,
                  env.records,
                  env.speech.store,
                  turn,
                  seen,
                  stubsAt(gates.toMap, l.round, offering),
                  l.answer,
                  pins.turn.grade,
                  shaped,
                  believed,
                  env.clock.now()
                )
              )
            }
            Ran(appended, verdictUnrecorded, believed)
        }
    }
  }

  /** The tools each call of the loop offers: `own`, after `topic` for the classification
    * `asked` holds. `topic` stays on after the first call, which alone asks for it
    * ([[loopShape]]): a request whose messages hold a call names the tool it called.
    * read_hidden and extract are not among them: a round binds and offers them apart
    * ([[gatedOffer]]). `TurnFailure.Model` when `own` already names one `topic`, read_hidden or
    * extract.
    */
  private def offered[C^](
      own: Toolbox[C],
      asked: Option[TurnTopics.Classification]
  ): Either[TurnFailure, Toolbox[C]] = {
    val tools = asked match {
      case None => Right(own)
      case Some(c) => own.including(TurnVerdict.tool(c))
    }
    tools
      .flatMap(t => t.preceded(Vector(ReadHidden.tool(None), unasked)).map(_ => t))
      .left
      .map { case DuplicateName(name) =>
        TurnFailure.Model(s"two tools are named ${ToolName.value(name)}")
      }
  }

  /** What `round`'s request offers of `schemas`, and the line it adds, as `gates`, each
    * round's gate by its index, say: those its gate withholds left out, after read_hidden and
    * extract from the first round whose gate hid an item on ([[ReadHidden]], [[Extract]],
    * `strict` as `schemas` are), and
    * the withdrawal its gate makes against the round before's, its tools as `offered` says them
    * ([[TurnGate.withdrawn]]); all of `schemas` and no line for a round with no gate.
    */
  private def gatedOffer(
      gates: Map[Int, Gated],
      round: Round,
      offered: ToolSet,
      schemas: Vector[ToolSchema],
      strict: Boolean
  ): (Vector[ToolSchema], Option[String]) =
    gates.get(round.index) match {
      case None => (schemas, None)
      case Some(now) =>
        (
          Vector(ReadHidden.tool(None).schema(strict), unasked.schema(strict))
            .filter(_ => hidSince(gates, round)) ++
            schemas.filterNot(s => now.withheld.exists(w => ToolName.value(w.tool) == s.name)),
          gates.get(round.index - 1).flatMap(TurnGate.withdrawn(_, now, offered))
        )
    }

  /** extract as a request offers it, asking nothing: for its name and schema alone. */
  private val unasked: Tool.Offered = Extract.tool(None, (_, _, _, _) => Left("not asked"))

  /** How extract puts a question of an item a request over `window` shows of `turn`
    * ([[Extract.Answer]]): to the summary's pin as `pins` cleared it, a quarantined JSON ask
    * ([[Extract.request]]) shown the item as the model would be shown it in full ([[textOf]]),
    * read through `env`'s `db` as `pins` bound it, and the question; each of its model calls
    * kept in the usage ledger through `jot` under [[Extract.costId]] (one a rerun already kept
    * is left as kept). Why no answer: no summary pin cleared, the item no longer kept, the
    * store unreadable, the ask's provider failing or its reply not reading after its repair.
    */
  private def extraction(env: TurnEnv^, jot: Jot^, turn: TurnRef, window: Window)(using
      pins: Picked
  ): (Item, String, Shape, CallSlot) ->{env, jot} Either[String, Option[Extracted]] =
    (item, question, shape, slot) =>
      for {
        pin <- pins.summary.left.map(_ => "no summary model is cleared for this turn")
        showing <- pins
          .reads(env.db)
          .read(Subject.Turn(turn))(shown(env.records, turn, window))
          .left
          .map(describe)
        text <- textOf(showing, window, item.ref).toRight("the item is no longer kept")
        request <- Extract.request(text, question, shape, pin.settings.strict)
        typed <- Extract.reply(shape)
        shaped = Shaping.shaped(
          env.models.provider(pin),
          request,
          Posed.ReplyTool,
          typed,
          pin.settings.names,
          pin.settings.repairs,
          Posed.Json.Repairs,
          env.clock
        )
        calls = shaped match {
          case Shaped.Read(_, _, calls) => calls
          case Shaped.Unread(_, calls) => calls
          case Shaped.Failed(_, calls) => calls
        }
        _ <- calls.zipWithIndex.foldLeft[Either[String, Unit]](Right(())) { case (acc, (c, k)) =>
          acc.flatMap { _ =>
            val (ledger, estimate) = (env.records.ledger, env.records.estimator.request(c.sent))
            jot.write(Subject.Turn(turn))(
              ledger.record(
                Extract.costId(slot, k),
                turn,
                turn.workflowId,
                c.answer.model,
                c.answer.upstream,
                Some(pin.grade),
                c.answer.usage,
                estimate
              )
            ) match {
              case Right(()) | Left(StoreError.DuplicateId(_)) => Right(())
              case Left(error) => Left(describe(error))
            }
          }
        }
        answer <- shaped match {
          case Shaped.Read(_, value, _) => Right(value)
          case Shaped.Unread(why, _) => Left(why)
          case Shaped.Failed(why, _) => Left(why)
        }
      } yield answer

  /** What a request over `window` would show of the item at `ref` among `showing` in full, as
    * its one message's text: a tool's result as [[Shown.result]] shows it, an entry as
    * [[Shown.of]] does, a nearby one as its section alone shows it ([[Shown.section]]), a
    * document as [[Shown.document]] does; `None` when `showing` no longer holds it.
    */
  private def textOf(showing: Showing, window: Window, ref: ItemRef): Option[String] = {
    def text(m: Message): String = m match {
      case Message.User(t) => t
      case Message.Assistant(blocks, _, _, _, _) =>
        blocks.collect { case AssistantBlock.Text(t) => t }.mkString
      case Message.ToolResult(_, content, _) => content
    }
    val own = showing.window ++ showing.mine
    ref match {
      case ItemRef.Result(call) =>
        own.map(_.payload).collectFirst {
          case Payload.Result(r, _) if r.callId == call => Shown.result(r).content
        }
      case ItemRef.Own(seq) =>
        own.find(_.seq == seq).flatMap(Shown.of(_, showing.named)).map(text)
      case ItemRef.Nearby(conversation, seq) =>
        for {
          section <- window.nearby.find(s =>
            s.conversation == conversation && s.names.contains(seq)
          )
          e <- showing.near.find(e => e.conversationId == conversation && e.seq == seq)
          m <- Shown.section(only(section, seq), Vector(e), showing.named)
        } yield text(m)
      case ItemRef.Document(version) =>
        showing.documents.collectFirst {
          case (d, label) if d.version == version => text(Shown.document(d, label))
        }
    }
  }

  /** Whether a gate of `gates`, each round's by its index, hid an item by `round`'s: from then
    * on, each round offers read_hidden and extract.
    */
  private def hidSince(gates: Map[Int, Gated], round: Round): Boolean =
    gates.exists((index, g) => index <= round.index && g.hidden.nonEmpty)

  /** How the loop's call `round` is built from the plain request: tagged for `asked` on the
    * first call, offering `tools` as `use` says, guided as [[TurnLoop.guided]] says under
    * `guidance`, told as [[TurnLoop.told]] says under `after`, and its system prompt ending,
    * after a blank line, with `withdrawn` when there is one ([[TurnGate.withdrawn]]).
    */
  private def loopShape(
      asked: Option[TurnTopics.Classification],
      round: Round,
      use: ToolUse,
      tools: Vector[ToolSchema],
      after: AfterToolResult,
      guidance: ToolGuidance,
      withdrawn: Option[String]
  ): ModelRequest -> ModelRequest =
    base => {
      val tagged = asked.filter(_ => round == Round.First).fold(base)(TurnVerdict.tagged(base, _))
      val shaped =
        TurnLoop.told(use, TurnLoop.guided(tagged.copy(tools = tools, use = use), guidance), after)
      withdrawn.fold(shaped)(line => shaped.copy(system = s"${shaped.system}\n\n$line"))
    }

  /** The `record-call:n` step: `reply`, the reply to `round` that called tools, kept as
    * [[TurnTools.callId]] dated `at` after everything in the conversation, believed as
    * [[believedAt]] says of `believed`, and what it cost
    * in the ledger beside the estimate of its request, rebuilt by `shape` from the same
    * window and entries, each item `stubs` names shown as its stub.
    */
  private def recordCall(
      system: String,
      records: TurnRecords,
      turn: TurnRef,
      window: Window,
      stubs: Map[ItemRef, String],
      round: Round,
      reply: Message.Assistant,
      grade: Grade,
      shape: ModelRequest -> ModelRequest,
      believed: Option[Integrity],
      at: Instant
  )(using Tx^): Either[TurnFailure, EntryId] = {
    val TurnRecords(entries, ledger, estimator, _, _, _) = records
    val id = TurnTools.callId(turn, round)
    for {
      next <- entries.lockNext(turn.conversationId).left.map(storeFailure)
      level <- believedAt(entries, turn, believed)
      showing <- shown(records, turn, window).left.map(storeFailure)
      sent <- requestOf(system, showing, turn, window, stubs).map(_.shaped(shape))
      _ <- entries
        .insert(
          Entry(
            id,
            turn.conversationId,
            turn.turnSeq,
            None,
            next.seq,
            Payload.Exchange(reply),
            at,
            level
          )
        )
        .left
        .map(storeFailure)
      _ <- Asking
        .spent(ledger, id, turn, reply, Some(grade), estimator.request(sent))
        .left
        .map(storeFailure)
    } yield id
  }

  /** The request built from `turn`'s `window`, each item `stubs` names shown as its stub, and
    * shaped by `shape`, sent to the provider, whose reply is told to edges as it arrives, asked as [[Asking.reply]] says: each try a
    * fresh attempt on the stream ([[TurnStream.hearing]]), so a rerun's pieces follow a
    * crashed run's; its failure fails the call.
    */
  private def callShaped(
      heard: StreamWriter^,
      turn: TurnRef,
      window: Window,
      stubs: Map[ItemRef, String],
      shape: ModelRequest -> ModelRequest
  )(using
      env: TurnEnv^,
      pins: Picked,
      offer: TurnOffer
  ): Either[TurnFailure, Message.Assistant] =
    request(env, offer.system, turn, window, stubs).flatMap { req =>
      val hearing = TurnStream.hearing(heard, env.fresh, env.clock)
      Asking
        .reply(env.models.provider(pins.turn), req.shaped(shape), hearing, env.clock)
        .left
        .map(TurnFailure.Model(_))
    }

  /** The last two steps: the summary of `turn`, whose reply is `answered`, believed as
    * [[believedAt]] says of `believed`, with the name and description of its message's
    * topic, asked again while the summarizer is unavailable, as a reply is ([[Asking.reply]]).
    */
  private def summarise(
      turn: TurnRef,
      answered: EntryId,
      believed: Option[Integrity]
  )(using env: TurnEnv^, pins: Picked, d: Durable^): Either[TurnFailure, EntryId] = {
    import TurnJournal.given
    for {
      message <- d.step(Step.Summarise) { () =>
        pins.summary
          .flatMap { pin =>
            pins
              .reads(env.db)
              .read(Subject.Turn(turn))(env.records.entries.list(turn.conversationId))
              .left
              .map(storeFailure)
              .flatMap { all =>
                val request = summaryRequest(all, turn)
                Asking
                  .reply(env.models.provider(pin), request, Hearing.silent(), env.clock)
                  .left
                  .map(TurnFailure.Model(_))
              }
          }
          // Nothing reads a summary's reasoning, and it was most of what this step recorded.
          .map[Message.Assistant](m =>
            Message.Assistant(
              m.blocks.filter {
                case AssistantBlock.Reasoning(_, _) => false
                case _ => true
              },
              m.stop,
              m.usage,
              m.model,
              m.upstream
            )
          )
      }
      // Right whenever `message` is: the summarise step made its call under this pin.
      grade <- pins.summary.map(_.grade)
      appended <- d.transact(Step.AppendSummary, Subject.Turn(turn))(
        appendSummary(env.records, turn, answered, message, grade, believed, env.clock.now())
      )
    } yield appended
  }

  /** The summary request for `turn`, from `all` of its conversation's entries: asking after
    * its topic too when it has one.
    */
  private def summaryRequest(all: Vector[Entry], turn: TurnRef): ModelRequest =
    TurnSummary.request(own(all, turn), TurnTopics.topicOf(all, turn))

  /** Records the summary in `message` as `turn`'s summary, dated `at`, a child of its reply
    * `answered`, after everything already in the conversation, believed as [[believedAt]]
    * says of `believed`; and what it cost in the
    * ledger beside the estimate of the request that produced it, rebuilt as [[append]]
    * rebuilds its own.
    * What it says of the message's topic is recorded after it ([[TurnTopics.describedId]]).
    */
  private def appendSummary(
      records: TurnRecords,
      turn: TurnRef,
      answered: EntryId,
      message: Message.Assistant,
      grade: Grade,
      believed: Option[Integrity],
      at: Instant
  )(using Tx^): Either[TurnFailure, EntryId] = {
    val id = TurnSummary.id(turn)
    val TurnRecords(entries, ledger, estimator, _, _, _) = records
    for {
      read <- TurnSummary
        .read(message)
        .toRight(TurnFailure.Model(s"the summary has no text (stop: ${message.stop})"))
      text = read.summary
      next <- entries.lockNext(turn.conversationId).left.map(storeFailure)
      level <- believedAt(entries, turn, believed)
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
            at,
            level
          )
        )
        .left
        .map(storeFailure)
      estimate = estimator.request(summaryRequest(all, turn))
      _ <- Asking.spent(ledger, id, turn, message, Some(grade), estimate).left.map(storeFailure)
      described = TurnTopics.topicOf(all, turn).toVector.flatMap(TurnTopics.described(_, read))
      _ <- TurnTopics.writeEvents(
        entries,
        ledger,
        turn,
        TurnTopics.describedId(turn),
        described,
        None,
        None,
        at
      )
    } yield id
  }

  /** What a row `turn` writes after its first gate is believed at: `believed`, its level then,
    * or, for a turn ungated ([[Patches.Integrity]]), what its entries say
    * ([[EntryStore.believed]]).
    */
  private def believedAt(entries: EntryStore, turn: TurnRef, believed: Option[Integrity])(using
      Tx^
  ): Either[TurnFailure, Integrity] =
    believed.fold(entries.believed(turn).left.map(storeFailure))(Right(_))

  /** The `gate:n` step: what a request for `turn` over `window` shows, read through `env`'s
    * `db` as `pins` bound it ([[shown]]), each item as its row is believed ([[itemsOf]]),
    * decided by [[TurnGate.decide]] over `offered` from `prior`, else from the turn's start,
    * each hidden item keeping the name `named` gave it, and read as the turn's replies called
    * read_hidden ([[ReadHidden.read]]). The start is its root's integrity, met with
    * [[Integrity.Outside]] when `root` is a heard message grit was not addressed in ([[TurnOffer.Root.Heard]], [[TurnOffer.Root.Named]]).
    */
  private def gateOf(
      env: TurnEnv^,
      turn: TurnRef,
      window: Window,
      root: TurnOffer.Root,
      offered: ToolSet,
      prior: Option[Integrity],
      named: Map[ItemRef, HiddenName]
  )(using pins: Picked): Either[TurnFailure, Gated] =
    pins
      .reads(env.db)
      .read(Subject.Turn(turn))(shown(env.records, turn, window))
      .left
      .map(storeFailure)
      .map { showing =>
        val start = prior.getOrElse {
          val rooted =
            showing.mine
              .minByOption(e => EntrySeq.value(e.seq))
              .fold(Integrity.Untrusted)(_.integrity)
          root match {
            case TurnOffer.Root.Heard | TurnOffer.Root.Named => rooted.meet(Integrity.Outside)
            case TurnOffer.Root.Addressed | TurnOffer.Root.ByName => rooted
          }
        }
        TurnGate.decide(
          start,
          offered,
          itemsOf(showing, window),
          ReadHidden.read(showing.mine),
          named
        )
      }

  /** Each item a request over `window` shows the model of `showing`, in the order it shows
    * them: the entries `window`'s nearby sections show, section by section ([[Shown.section]]),
    * the documents, the window's entries [[Shown.own]] shows, then the turn's own entries
    * [[Shown.turn]] shows; an entry kept as a tool's result by the call it answers, named by
    * its tool, and any other by its position, a person's named as [[Showing.named]] names them
    * ("someone" when it does not), grit's as "an earlier reply" and a closing as "a closing
    * record". An entry the request does not show (an earlier turn's tool exchange or result, a
    * window, a query, a summary, a topic, an ask, a message with no text nearby) is none.
    */
  private def itemsOf(showing: Showing, window: Window): Vector[Item] = {
    val Showing(shownWindow, mine, near, named, documents) = showing
    val tools: Map[ToolCallId, String] = (shownWindow ++ mine)
      .flatMap(_.payload match {
        case Payload.Exchange(reply) =>
          reply.blocks.collect { case AssistantBlock.ToolCall(id, name, _) => id -> name }
        case _ => Vector.empty
      })
      .toMap
    def person(e: Entry): String = named.of(e.id).getOrElse("someone")
    def entry(e: Entry, ref: ItemRef): Option[Item] = {
      val source = e.payload match {
        case Payload.Result(r, _) => Some(tools.getOrElse(r.callId, "a tool"))
        case Payload.Message(Message.User(_)) | Payload.Heard(_) => Some(person(e))
        case Payload.Message(_) | Payload.Exchange(_) | Payload.Posted(_) =>
          Some("an earlier reply")
        case Payload.Closed(_, _, _) => Some("a closing record")
        case _ => None
      }
      val at = e.payload match {
        case Payload.Result(r, _) => ItemRef.Result(r.callId)
        case _ => ref
      }
      source.map(Item(at, e.integrity, _))
    }
    // Each entry as its section shows it alone: shown there exactly when it is shown alone.
    val nearby = window.nearby.flatMap { section =>
      section.names.flatMap { seq =>
        near
          .find(e => e.conversationId == section.conversation && e.seq == seq)
          .filter(e => Shown.section(only(section, seq), Vector(e), named).isDefined)
          .flatMap(e => entry(e, ItemRef.Nearby(e.conversationId, e.seq)))
      }
    }
    nearby ++
      documents.map((d, label) =>
        Item(ItemRef.Document(d.version), d.integrity, DocLabel.value(label))
      ) ++
      shownWindow
        .filter(e => Shown.of(e, named).isDefined)
        .flatMap(e => entry(e, ItemRef.Own(e.seq))) ++
      mine
        .filter(e => Shown.turn(Vector(e), named).nonEmpty)
        .flatMap(e => entry(e, ItemRef.Own(e.seq)))
  }

  /** `section` naming `seq` alone. */
  private def only(section: Nearby, seq: EntrySeq): Nearby = section match {
    case Nearby.Open(c, place, _) => Nearby.Open(c, place, Vector(seq))
    case Nearby.Along(c, place, _) => Nearby.Along(c, place, Vector(seq))
    case Nearby.Asked(c, place, _) => Nearby.Asked(c, place, Vector(seq))
    case Nearby.Closed(c, place, _) => Nearby.Closed(c, place, seq)
  }

  /** `showing` with each item `stubs` names, by the ref [[itemsOf]] gives it, in its kind's
    * place but holding its stub in place of what it says: a tool's result as a result that is
    * no error, grit's reply as its text, a heard message or grit's post as one, a closing as
    * its record's prose, a document as its text (dropped if the stub cannot be one), and any
    * other entry as a person's message.
    */
  private def hiding(showing: Showing, stubs: Map[ItemRef, String]): Showing =
    if (stubs.isEmpty) showing
    else {
      def entry(ref: Entry => ItemRef)(e: Entry): Entry = {
        val at = e.payload match {
          case Payload.Result(r, _) => ItemRef.Result(r.callId)
          case _ => ref(e)
        }
        stubs.get(at).fold(e)(stub => e.copy(payload = stubbed(e.payload, stub)))
      }
      val own = entry(e => ItemRef.Own(e.seq))
      showing.copy(
        window = showing.window.map(own),
        mine = showing.mine.map(own),
        near = showing.near.map(entry(e => ItemRef.Nearby(e.conversationId, e.seq))),
        documents = showing.documents.flatMap((document, label) =>
          stubs.get(ItemRef.Document(document.version)) match {
            case None => Some((document, label))
            case Some(stub) => DocText.of(stub).toOption.map(t => (document.copy(text = t), label))
          }
        )
      )
    }

  /** `payload` holding `stub` in place of what it says, as [[hiding]] says. */
  private def stubbed(payload: Payload, stub: String): Payload = payload match {
    case Payload.Result(r, shown) => Payload.Result(r.copy(content = stub, isError = false), shown)
    case Payload.Message(reply: Message.Assistant) =>
      Payload.Message(reply.copy(blocks = Vector(AssistantBlock.Text(stub))))
    // Its calls are kept so their results still pair, and their arguments, which may copy what
    // that turn read, never carry a hidden item here: no exchange is ever hidden. An earlier
    // turn's is not shown, so is no item (itemsOf); this turn's own is believed at the level
    // the turn had when it was kept (record-call), which only falls after, so it dominates
    // every later gate's level and is never listed.
    case Payload.Exchange(reply) =>
      Payload.Exchange(reply.copy(blocks = AssistantBlock.Text(stub) +: reply.blocks.collect {
        case call: AssistantBlock.ToolCall => call
      }))
    case Payload.Heard(_) => Payload.Heard(stub)
    case Payload.Posted(_) => Payload.Posted(stub)
    case Payload.Closed(period, reason, _) =>
      Flows
        .of(stub, None, Vector.empty)
        .fold(Payload.Message(Message.User(stub)))(f =>
          Payload.Closed(period, reason, Closing(f, Balance.empty))
        )
    case _ => Payload.Message(Message.User(stub))
  }

  /** The stubs `round`'s request shows, by the ref of the item each stands for, as its gate
    * in `gates` hid them, their tools as `offered` says them ([[TurnGate.stub]]); none for a
    * round with no gate.
    */
  private def stubsAt(
      gates: Map[Int, Gated],
      round: Round,
      offered: ToolSet
  ): Map[ItemRef, String] =
    gates
      .get(round.index)
      .fold(Map.empty)(
        _.hidden.map((item, h) => item.ref -> TurnGate.stub(item, h, offered)).toMap
      )

  /** `turn`'s own entries, from `all` of its conversation's. */
  private def own(all: Vector[Entry], turn: TurnRef): Vector[Entry] =
    all.filter(_.turnSeq == turn.turnSeq)

  /** The window's entries, then the turn's own, as one model request under `system`, each
    * item `stubs` names shown as its stub, to be shaped, read through `env`'s `db`.
    */
  private def request(
      env: TurnEnv^,
      system: String,
      turn: TurnRef,
      window: Window,
      stubs: Map[ItemRef, String]
  )(using pins: Picked): Either[TurnFailure, Unshaped] =
    pins
      .reads(env.db)
      .read(Subject.Turn(turn))(shown(env.records, turn, window))
      .left
      .map(storeFailure)
      .flatMap(requestOf(system, _, turn, window, stubs))

  /** What a request for `turn` over `window` can show, read from `records`: the entries of
    * its conversation at the window's seqs, the turn's own entries, the nearby sections'
    * entries that still exist, who said each, and the window's documents as it shows them
    * ([[grit.core.document.DocumentSearch.labelled]]); nothing else of the conversation.
    */
  private def shown(records: TurnRecords, turn: TurnRef, window: Window)(using
      Tx^
  ): Either[StoreError, Showing] =
    shown(records.entries, records.principals, records.documents, turn, window)

  /** [[shown]], read from the three stores it reads. */
  private def shown(
      entries: EntryStore,
      principals: Principals,
      documents: DocumentSearch,
      turn: TurnRef,
      window: Window
  )(using Tx^): Either[StoreError, Showing] =
    for {
      at <- entries.at(turn.conversationId, window.entries)
      mine <- entries.ofTurn(turn)
      near <- Nearby.read(window.nearby, entries)
      named <- principals.speakers((at ++ mine ++ near).map(_.id).distinct)
      labelled <- documents.labelled(window.documents)
    } yield Showing(at, mine, near, named, labelled)

  /** What [[shown]] read: `window`, the entries at the window's seqs that exist; `mine`,
    * the turn's own; `near`, the nearby entries that still exist; `named`, their speakers;
    * `documents`, the window's documents still kept whose plugin is enabled, in its order,
    * each with its plugin's label.
    */
  private final case class Showing(
      window: Vector[Entry],
      mine: Vector[Entry],
      near: Vector[Entry],
      named: Speakers,
      documents: Vector[(Document, DocLabel)]
  )

  /** The request [[request]] builds from `showing`, each item `stubs` names shown as its stub
    * there ([[hiding]]), to be shaped
    * ([[Unshaped]]): each nearby section as one user message ([[Shown.section]], a section with
    * none of its entries left dropped), then each document as one ([[Shown.document]]), then
    * what the model is shown of the window's entries ([[Shown.own]]: its messages, a closing
    * entry as one user message, and a gap line wherever turns are left out), then the turn's
    * own, its tool loop's exchange among them in order. A summary is not shown to the model
    * yet. A window seq with no entry is an `Assembly` failure naming every such seq.
    */
  private def requestOf(
      system: String,
      showing: Showing,
      turn: TurnRef,
      window: Window,
      stubs: Map[ItemRef, String]
  ): Either[TurnFailure, Unshaped] = {
    val Showing(at, mine, near, named, documents) = hiding(showing, stubs)
    val bySeq = at.map(e => e.seq -> e).toMap
    val sections = window.nearby.flatMap(Shown.section(_, near, named))
    window.entries.filterNot(bySeq.contains) match {
      case missing if missing.nonEmpty =>
        Left(
          TurnFailure.Assembly(
            s"window names unknown entries at seqs: ${missing.map(EntrySeq.value).mkString(", ")}"
          )
        )
      case _ =>
        val shown = Shown.own(window.entries.flatMap(bySeq.get), turn.turnSeq, named)
        Right(Unshaped(system, sections, documents, shown ++ Shown.turn(mine, named)))
    }
  }

  /** A request of `system`: `before`, then each of `documents` as one user message, then
    * `after`.
    */
  private final case class Unshaped(
      system: String,
      before: Vector[Message],
      documents: Vector[(Document, DocLabel)],
      after: Vector[Message]
  ) {

    /** This request shaped by `shape`, each document's pointer line shown only when the shaped
      * request offers the tool it names: a model offered no tool of that name could not follow
      * it. The tools offered are those `shape` gives the request with no pointer line shown.
      */
    def shaped(shape: ModelRequest -> ModelRequest): ModelRequest = {
      def built(offered: Set[String]): ModelRequest =
        ModelRequest(
          system,
          before ++ documents.map((document, label) =>
            Shown.document(
              document.copy(pointer =
                document.pointer.filter(p => offered.contains(ToolName.value(p.tool)))
              ),
              label
            )
          ) ++ after
        )
      shape(built(shape(built(Set.empty)).tools.map(_.name).toSet))
    }
  }

  /** Records `message` as `turn`'s reply entry ([[TurnRef.replyId]]), or, when `offer`'s
    * root is a heard message it drafts a reply to, as its draft ([[TurnRef.draftId]],
    * [[Payload.Draft]]), believed as [[believedAt]] says of `believed`; a reply to a heard message answered as said to grit is kept as its
    * outcome in `speech` ([[Outcome.Replied]]) with its words; dated `at`, after
    * everything already in the conversation, and what it cost in the ledger beside the
    * estimate of the request that produced it: rebuilt from the same window and the same
    * entries (the turn's own were all recorded before its call, and the reply is not yet
    * among them), each item `stubs` names shown as its stub, shaped by `shape`. When the
    * window is `recorded` with the reply, its queries and the window itself go in first
    * ([[writeWindow]]). Each of the window's
    * documents whose pointer one of the turn's calls followed, and that succeeded, is counted
    * as followed at `at` ([[Pointer.followedIn]]).
    */
  private def append(
      offer: TurnOffer,
      records: TurnRecords,
      speech: SpeechStore,
      turn: TurnRef,
      window: Window,
      stubs: Map[ItemRef, String],
      message: Message.Assistant,
      grade: Grade,
      shape: ModelRequest -> ModelRequest,
      believed: Option[Integrity],
      at: Instant
  )(using Tx^): Either[TurnFailure, EntryId] = {
    val (id, payload) = offer.root match {
      case TurnOffer.Root.Addressed | TurnOffer.Root.ByName =>
        (turn.replyId, Payload.Message(message))
      case TurnOffer.Root.Heard | TurnOffer.Root.Named => (turn.draftId, Payload.Draft(message))
    }
    val TurnRecords(entries, ledger, estimator, _, _, _) = records
    for {
      next <- entries.lockNext(turn.conversationId).left.map(storeFailure)
      level <- believedAt(entries, turn, believed)
      showing <- shown(records, turn, window).left.map(storeFailure)
      sent <- requestOf(offer.system, showing, turn, window, stubs).map(_.shaped(shape))
      _ <- entries
        .insert(
          Entry(
            id,
            turn.conversationId,
            turn.turnSeq,
            None,
            next.seq,
            payload,
            at,
            level
          )
        )
        .left
        .map(storeFailure)
      _ <- Asking
        .spent(ledger, id, turn, message, Some(grade), estimator.request(sent))
        .left
        .map(storeFailure)
      followed = Pointer.followedIn(showing.documents.map(_._1), callsOf(showing.mine))
      _ <- records.documents.followed(followed, at).left.map(storeFailure)
      _ <- offer.root match {
        case TurnOffer.Root.ByName =>
          speech
            .drafted(turn, Outcome.Replied, TurnJudge.said(message), at)
            .left
            .map(storeFailure)
        case TurnOffer.Root.Addressed | TurnOffer.Root.Heard | TurnOffer.Root.Named => Right(true)
      }
    } yield id
  }

  /** Each tool call among `own`, a turn's own entries, by the name it was made under, with
    * whether its result was kept and was not an error; a call under a name no tool could have
    * is left out.
    */
  private def callsOf(own: Vector[Entry]): Vector[Pointer.Call] = {
    val failed = own
      .map(_.payload)
      .collect { case Payload.Result(result, _) =>
        result.callId -> result.isError
      }
      .toMap
    own.map(_.payload).flatMap {
      case Payload.Exchange(reply) =>
        reply.blocks.flatMap {
          case AssistantBlock.ToolCall(id, name, arguments) =>
            ToolName
              .of(name)
              .toOption
              .map(Pointer.Call(_, arguments, failed.get(id).contains(false)))
          case _ => None
        }
      case _ => Vector.empty
    }
  }

  /** The `record-window` step: `turn`'s window and the queries that chose it, recorded
    * dated `at` before the model is called, so an edge sees them while the turn runs, and
    * each document the window holds counted as placed at `at`.
    */
  private def recordWindow(
      records: TurnRecords,
      turn: TurnRef,
      window: Window,
      at: Instant
  )(using Tx^): Either[TurnFailure, EntryId] =
    for {
      next <- records.entries.lockNext(turn.conversationId).left.map(storeFailure)
      level <- records.entries.believed(turn).left.map(storeFailure)
      _ <- writeWindow(records, turn, window, next.seq, level, at)
      _ <- records.documents.placed(window.documents, at).left.map(storeFailure)
    } yield windowId(turn)

  /** Writes each query `window`'s notes say assembly wrote, as [[queryId]] with its own
    * cost, then the window itself, as [[windowId]], from position `from`, dated `at`, believed
    * at `level`; the position after the last one written.
    */
  private def writeWindow(
      records: TurnRecords,
      turn: TurnRef,
      window: Window,
      from: EntrySeq,
      level: Integrity,
      at: Instant
  )(using Tx^): Either[TurnFailure, EntrySeq] = {
    val TurnRecords(entries, ledger, _, _, _, _) = records
    val queries = window.notes.collect { case q: AssemblyNote.Queried => q }
    val recalled = window.notes.flatMap {
      case AssemblyNote.Recalled(turns) => turns
      case _ => Vector.empty
    }
    for {
      windowSeq <- queries.zipWithIndex.foldLeft[Either[TurnFailure, EntrySeq]](Right(from)) {
        case (done, (q, i)) =>
          done.flatMap { seq =>
            val qid = queryId(turn, i)
            val entry = Entry(
              qid,
              turn.conversationId,
              turn.turnSeq,
              None,
              seq,
              Payload.Query(q.query),
              at,
              level
            )
            entries
              .insert(entry)
              .flatMap(_ =>
                ledger.record(
                  qid,
                  turn,
                  turn.workflowId,
                  q.model,
                  q.upstream,
                  None,
                  q.usage,
                  q.estimate
                )
              )
              .left
              .map(storeFailure)
              .map(_ => seq.next)
          }
      }
      _ <- entries
        .insert(
          Entry(
            windowId(turn),
            turn.conversationId,
            turn.turnSeq,
            None,
            windowSeq,
            Payload.Window(window.entries, recalled, window.nearby, window.documents),
            at,
            level
          )
        )
        .left
        .map(storeFailure)
    } yield windowSeq.next
  }

  /** [[TurnRecord.requests]]. */
  private[turn] def rebuilt(
      turn: TurnRef,
      steps: Vector[StepRecord],
      reads: TurnRecord.Reads
  )(using Tx^): Either[String, TurnRecord.Calls] = {
    import TurnJournal.given
    val workflow = WorkflowId.value(turn.workflowId)
    def output[A](name: String)(using journal: Journaled[A]): Either[String, Option[A]] =
      steps.find(_.name == name).flatMap(_.output) match {
        case None => Right(None)
        case Some(o) => journal.decode(o).map(Some(_)).left.map(why => s"$name: $why")
      }
    // A step's value, `None` when it is not recorded; `Left` when it recorded a failure.
    def recorded[A](name: String)(using
        Journaled[Either[TurnFailure, A]]
    ): Either[String, Option[A]] =
      output[Either[TurnFailure, A]](name).flatMap {
        case Some(Right(a)) => Right(Some(a))
        case Some(Left(failure)) => Left(s"$name recorded a failure: ${reason(failure)}")
        case None => Right(None)
      }
    def needed[A](name: String)(using Journaled[Either[TurnFailure, A]]): Either[String, A] =
      recorded[A](name).flatMap(_.toRight(s"$workflow recorded no $name step"))
    def kept[A](what: String, read: Either[StoreError, A]): Either[String, A] =
      read.left.map(e => s"$what of $workflow unread: ${describe(e)}")
    // The rounds whose replies called tools, by the record-call:n steps that kept them.
    val looped: Vector[(Round, String)] =
      steps.flatMap(s => Loop.of(s.name).collect { case Loop.Record(n) => Round.at(n) -> s.name })
    // Each round's gate, by the gate:n steps that recorded it: none for a turn ungated.
    val gated: Vector[(Int, String)] =
      steps.flatMap(s => Loop.of(s.name).collect { case Loop.Gate(n) => n -> s.name })
    for {
      offer <- needed[TurnOffer.Recorded](Step.Offer)
      profileId <- needed[TurnProfileId](Step.PinModels)
      window <- needed[Window](Step.Assemble)
      classified <- output[TurnTopics.Classification](Step.Classify)
      profile <- kept("profile", reads.profiles.get(profileId))
        .flatMap(_.toRight(s"no profile kept under ${TurnProfileId.value(profileId)}"))
      set <- kept("tool set", reads.toolSets.get(offer.tools))
      prompt <- kept("prompt", reads.prompts.prompt(offer.prompt))
      showing <- kept(
        "window",
        shown(reads.entries, reads.principals, reads.documents, turn, window)
      )
      calls <- looped.foldLeft[Either[String, Vector[(Round, EntryId)]]](Right(Vector.empty)) {
        case (acc, (round, name)) =>
          acc.flatMap(done => recorded[EntryId](name).map(id => done ++ id.map(round -> _)))
      }
      answer <- recorded[EntryId](Step.Append)
      gates <- gated.foldLeft[Either[String, Map[Int, Gated]]](Right(Map.empty)) {
        case (acc, (round, name)) =>
          acc.flatMap(done => recorded[Gated](name).map(g => done ++ g.map(round -> _)))
      }
      routed <- recorded[Route](Step.Route)
      settings = routed.fold(profile.turn.first)(_.turn).settings
      strict = settings.strict == StrictSchemas.Enforced
      asking = classified.flatMap(asked)
      // The set as recorded, each tool as a turn reads one its build no longer has: its schema
      // is what a typed tool sends when not strict (Tool.entry records args.schema(false)).
      own = Toolbox.of[{}](set.tools.map(Tool.gone)*).getOrElse(Toolbox.Empty)
      tools <- offered(own, asking).left.map(reason)
      schemas = tools.schemas(strict)
      // What the call whose reply is kept as `reply` was sent, under `use`. Its reply is kept
      // after every own entry the call was shown: recordCall and append read `shown` after
      // lockNext, which serialises the conversation, and before inserting the reply, so the
      // own entries it saw are exactly those below the reply's seq. Own entries Shown.turn
      // does not show (a window, a query, a topic event, an ask) change nothing either side.
      sent = (round: Round, use: ToolUse, reply: EntryId) =>
        showing.mine.find(_.id == reply) match {
          case None => Left(s"the reply ${EntryId.value(reply)} of $workflow is not kept")
          case Some(at) =>
            val before = showing.mine.filter(e => EntrySeq.value(e.seq) < EntrySeq.value(at.seq))
            val (shown, line) = gatedOffer(gates, round, tools.set, schemas, strict)
            requestOf(
              prompt.render,
              showing.copy(mine = before),
              turn,
              window,
              stubsAt(gates, round, tools.set)
            )
              .map(
                _.shaped(
                  loopShape(
                    asking,
                    round,
                    use,
                    shown,
                    settings.afterResult,
                    settings.guidance,
                    line
                  )
                )
              )
              .left
              .map(reason)
        }
      rounds <- calls.foldLeft[Either[String, Vector[TurnRecord.Call]]](Right(Vector.empty)) {
        case (acc, (round, reply)) =>
          acc.flatMap(done =>
            sent(round, ToolUse.Auto, reply).map(r =>
              done :+ TurnRecord.Call(round.index, reply, r)
            )
          )
      }
      last = Round.at(calls.size)
      answered <- answer.fold[Either[String, Option[TurnRecord.Answered]]](Right(None))(reply =>
        for {
          on <- sent(last, ToolUse.Auto, reply)
          off <- sent(last, ToolUse.Off, reply)
        } yield Some(TurnRecord.Answered(last.index, reply, on, off))
      )
    } yield TurnRecord.Calls(
      rounds,
      answered,
      if (strict) TurnRecord.Schemas.Recorded else TurnRecord.Schemas.Sent
    )
  }

  /** What `failure` says. */
  private def reason(failure: TurnFailure): String = failure match {
    case TurnFailure.Assembly(why) => why
    case TurnFailure.Model(why) => why
    case TurnFailure.Store(why) => why
    case TurnFailure.Uncleared(c) => Uncleared.reply(Uncleared(c))
  }

  private def storeFailure(error: StoreError): TurnFailure = TurnFailure.Store(describe(error))

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
