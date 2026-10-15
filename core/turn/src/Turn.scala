package grit.turn

import grit.core.allot.Allotment
import grit.core.context.Window
import grit.core.durable.{Durable, Flows, Journaled, StepRecord}
import grit.core.id.{EntryId, EntrySeq, TurnRef, WorkflowId}
import grit.core.model.{Pinned, StrictSchemas, TurnProfileId}
import grit.core.provider.ToolUse
import grit.core.store.{Db, StoreError, Tx}
import grit.core.tool.{Tool, Toolbox}
import grit.core.visibility.{Classification, Label}

import TurnFailure.{describe, reason}
import TurnLoop.Round
import TurnShown.{asked, gatedOffer, loopShape, offered, requestOf, shown, stubsAt}

/** The durable turn: one workflow per turn, run as [[TurnFlow]] describes it. Each step's
  * output is recorded, so a turn resumed after a crash never calls a model twice. A step that
  * fails returns a [[TurnFailure]], recorded like any other output, so a rerun ends the same
  * way without calling anything. A failure before the reply ends the turn; a failed summary
  * leaves the reply standing, and the turn without a summary. Topics never fail a turn.
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
    val PinModels: String = TurnSteps.Names.pinModels.family
    val Weigh: String = TurnSteps.Names.weigh.family
    val Route: String = TurnSteps.Names.route.family
    val Offer: String = TurnSteps.Names.offer.family
    val RecordFailure: String = TurnSteps.Names.recordFailure.family
    val Stitched: String = TurnSteps.Names.stitched.family
    val Classify: String = TurnSteps.Names.classify.family
    val RecordTopic: String = TurnSteps.Names.recordTopic.family
    val Assemble: String = TurnSteps.Names.assemble.family
    val RecordWindow: String = TurnSteps.Names.recordWindow.family
    val CallModel: String = TurnRound.Names.firstCall.family
    val RecordVerdict: String = TurnSteps.Names.recordVerdict.family
    val Append: String = TurnSteps.Names.append.family
    val Judge: String = TurnSteps.Names.judge.family
    val RecordSpeech: String = TurnSteps.Names.recordSpeech.family
    val Summarise: String = TurnSteps.Names.summarise.family
    val AppendSummary: String = TurnSteps.Names.appendSummary.family

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
    val RecordCall: String = TurnRound.Names.recordCall.family

    /** The family of `gate:n`, what round n's request shows and offers ([[Gated]]),
      * decided before its model call `call-model:n` (`call-model` for round 0).
      */
    val Gate: String = TurnRound.Names.gate.family

    /** The family of `tool:n:j`, the tool loop's settling of call j of round n's reply. */
    val Tool: String = TurnRound.Names.tool.family

    /** The family of `ask:n:j`, the tool loop's asking a person about call j of round n's
      * reply, a gated one, before it waits for their answer.
      */
    val Ask: String = TurnRound.Names.ask.family

    /** The family of `dispatch:n`, round n's free hosted calls sent to an edge together, and
      * of `dispatch:n:j`, call j's, a gated one, sent once a person approved it.
      */
    val Dispatch: String = TurnRound.Names.dispatch.family

    /** The family of `reach:n:{place}`, round n's free calls to the tools of a service the
      * turn reaches sent to the edge at its place (written), together.
      */
    val Reach: String = TurnRound.Names.reach.family

    /** The family of `expire:n:j`, call j of round n given up on when no edge claimed it in
      * time, or found claimed and waited on again.
      */
    val Expire: String = TurnRound.Names.expire.family

    /** The family of `abandon:n:j`, call j of round n given up on when its edge claimed it
      * and did not answer in time.
      */
    val Abandon: String = TurnRound.Names.abandon.family

    /** The family of `wait:n:j`, a person's time to answer about call j of round n's reply:
      * not a step the turn records, but its wait between `ask:n:j` and the answer, which
      * DBOS records as [[StepRecord.Received]] ([[named]] and [[running]] give it this name).
      */
    val Wait: String = TurnRound.Names.answer.family

    /** The step `name` stands for among [[all]] and the tool loop's families ([[Gate]],
      * [[RecordCall]], [[Ask]], [[Wait]], [[Tool]], [[Dispatch]], [[Reach]], [[Expire]],
      * [[Abandon]]): its
      * round, index or place dropped (`call-model:3` is [[CallModel]], `tool:1:0` is
      * [[Tool]]). `None` for a name that is not the turn's, such as a patch's marker or
      * DBOS's own records of a wait ([[StepRecord.Received]] before [[named]] renames it).
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
      * them: a [[StepRecord.Received]] that ends the wait after `ask:n:j` is `wait:n:j`; every other
      * name is as recorded.
      */
    def named(recorded: Vector[String]): Vector[String] =
      recorded
        .foldLeft((Vector.empty[String], Option.empty[Loop.Ask])) { case ((out, asked), name) =>
          (Loop.of(name), asked) match {
            case (Some(ask: Loop.Ask), _) => (out :+ name, Some(ask))
            case (_, Some(Loop.Ask(n, j))) if name == StepRecord.Received =>
              (out :+ s"$Wait:$n:$j", None)
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
      * `record-failure` ([[TurnFlow]]). Never retired: a rerun whose `pin-models` or `offer`
      * output is read back and then fails to load has recorded later steps at this point, does
      * not take it, and records nothing more.
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

  /** The turn workflow's body, for the turn whose workflow id is `workflowId`: its flow
    * ([[TurnFlow]]), its model offered the tools its `offer` step recorded from `tooling`'s.
    * Returns what the turn did, for logs: its reply and summary are in the store, never in
    * this string.
    */
  def body[C^](env: TurnEnv^, tooling: TurnTooling[C]^)(
      workflowId: WorkflowId
  )(using d: Durable^): String =
    TurnRef.fromWorkflowId(workflowId) match {
      case None => s"not a turn: ${WorkflowId.value(workflowId)}"
      case Some(turn) =>
        val turning = new TurnSteps.Turning(env, tooling, d.stream(TurnStream.Key))
        Flows.run(TurnFlow.flow, turning, turn).merge
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
          acc.flatMap(done =>
            recorded[TurnRound.Kept](name).map(kept => done ++ kept.map(round -> _.reply))
          )
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
            val (shown, lines) = gatedOffer(gates, round, tools.set, schemas, strict)
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
                    lines
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

}
