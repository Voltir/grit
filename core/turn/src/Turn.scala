package grit.turn

import grit.core.allot.Allotment
import grit.core.context.Window
import grit.core.durable.{Durable, Flows, Journaled, StepRecord}
import grit.core.id.{EntryId, EntrySeq, TurnRef, WorkflowId}
import grit.core.model.{Pinned, StrictSchemas, TurnProfile, TurnProfileId}
import grit.core.provider.ToolUse
import grit.core.store.{Db, ModelProfileStore, StoreError, Tx}
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
    * epoch, and only an engine of the same epoch, and the same deployment version
    * ([[under]]), resumes it. Change the steps compatibly
    * with `Durable.patch` and keep the epoch; change the epoch only for a break a patch
    * cannot carry, which strands the turns in flight under the old one. `TurnReplayTests`
    * replays the histories recorded under this epoch.
    */
  val Epoch = "2026-10-16"

  /** The application version an engine opens under for a deployment whose own version is
    * `deployment`: [[Epoch]], `+`, then `deployment`. Only an engine of the same one resumes
    * a workflow started under it.
    */
  def under(deployment: String): String = s"$Epoch+$deployment"

  /** The epoch the application version `version` names: what comes before its first `+`, or
    * all of it when it holds none.
    */
  def epochOf(version: String): String = version.takeWhile(_ != '+')

  /** The names of the turn's steps, as DBOS records them; which run, and in what order, is
    * [[TurnFlow.flow]]'s ([[all]]).
    */
  object Step {
    val PinModels: String = TurnSteps.Names.pinModels.family
    val Weigh: String = TurnSteps.Names.weigh.family
    val Route: String = TurnSteps.Names.route.family
    val Offer: String = TurnSteps.Names.offer.family
    val RecordFailure: String = TurnSteps.Names.recordFailure.family
    val Stitched: String = TurnSteps.Names.stitched.family
    val Classify: String = TurnSteps.Names.classify.family
    val RecordTopic: String = TurnSteps.Names.recordTopic.family
    val Search: String = TurnSteps.Names.search.family
    val Assemble: String = TurnSteps.Names.assemble.family
    val RecordWindow: String = TurnSteps.Names.recordWindow.family
    val Ask: String = TurnRound.Names.ask.family
    val RecordVerdict: String = TurnSteps.Names.recordVerdict.family
    val Append: String = TurnSteps.Names.append.family
    val Judge: String = TurnSteps.Names.judge.family
    val RecordSpeech: String = TurnSteps.Names.recordSpeech.family
    val Summarise: String = TurnSteps.Names.summarise.family
    val AppendSummary: String = TurnSteps.Names.appendSummary.family

    /** The families the turn's flow records, in the order a run first reaches each
      * ([[Flows.names]]): a tool loop's by their family alone (`gate`, `ask`, `call`).
      */
    val all: Vector[String] = Flows.names(TurnFlow.flow).map(_.name)

    /** The family of `ask:n:record`, the tool loop's record of round n's reply that called
      * tools.
      */
    val Record: String = "ask:record"

    /** The family of `gate:n`, what round n's request shows and offers ([[Gated]]),
      * decided before its model call `ask:n`.
      */
    val Gate: String = "gate"

    /** The turn's query written for its search, a move of its own. */
    val Query: String = "ask:query"

    /** The cost of the turn's query recorded, when its writer answered. */
    val QueryRecord: String = "ask:query:record"

    /** The family of `ask:relevance-n`, the turn's judging batch n of what its search found. */
    val Relevance: String = "ask:relevance"

    /** The family of `ask:relevance-n:record`, batch n's cost recorded, when its classifier
      * answered.
      */
    val RelevanceRecord: String = "ask:relevance:record"

    private val RelevanceBatch = "ask:relevance-\\d+".r
    private val RelevanceBatchRecord = "ask:relevance-\\d+:record".r

    /** The family of `call:n:j`, the tool loop's settling of call j of round n's reply. */
    val Call: String = "call"

    /** The family of `call:n:j:approve`, the tool loop's asking a person about call j of round
      * n's reply, a gated one, before it waits for their answer.
      */
    val Approve: String = "call:approve"

    /** The family of `call:n:send` and `call:n:send:{place}`, round n's free hosted calls sent
      * together to the edge serving the workspace, or a place the turn reaches (written).
      */
    val Send: String = "call:send"

    /** The family of `call:n:j:dispatch`, call j of round n, a gated hosted one, sent once a
      * person approved it.
      */
    val Dispatch: String = "call:dispatch"

    /** The family of `call:n:j:expire`, call j of round n given up on when no edge claimed it
      * in time, or found claimed and waited on again.
      */
    val Expire: String = "call:expire"

    /** The family of `call:n:j:abandon`, call j of round n given up on when its edge claimed
      * it and did not answer in time.
      */
    val Abandon: String = "call:abandon"

    /** The family of `call:n:j:wait`, a person's time to answer about call j of round n's
      * reply: not a step the turn records, but its wait between `call:n:j:approve` and the
      * answer, which DBOS records as [[StepRecord.Received]] ([[named]] and [[running]] give it
      * this name).
      */
    val Wait: String = "call:wait"

    /** The step `name` stands for among [[all]], the assembly's asks ([[Query]],
      * [[QueryRecord]], [[Relevance]], [[RelevanceRecord]]) and the tool loop's families
      * ([[Gate]], [[Record]], [[Approve]], [[Wait]], [[Call]], [[Send]], [[Dispatch]],
      * [[Expire]], [[Abandon]]): its round, batch, index or place dropped (`ask:3` is [[Ask]],
      * `ask:relevance-1` is [[Relevance]], `call:1:0` is [[Call]]). `None` for a name that is not the turn's, such as a patch's marker or
      * DBOS's own records of a wait ([[StepRecord.Received]] before [[named]] renames it).
      */
    def family(name: String): Option[String] =
      name match {
        case _ if name.startsWith(StepRecord.marker("")) => None
        case Query | QueryRecord => Some(name)
        case RelevanceBatch() => Some(Relevance)
        case RelevanceBatchRecord() => Some(RelevanceRecord)
        case _ => loopFamily(name)
      }

    /** `name`'s family when it is the tool loop's, or else one of [[all]]. */
    private def loopFamily(name: String): Option[String] =
      Loop.of(name) match {
        case Some(Loop.Ask(_)) => Some(Ask)
        case Some(Loop.Gate(_)) => Some(Gate)
        case Some(Loop.Record(_)) => Some(Record)
        case Some(Loop.Approve(_, _)) => Some(Approve)
        case Some(Loop.Wait(_, _)) => Some(Wait)
        case Some(Loop.Call(_, _)) => Some(Call)
        case Some(Loop.Send(_, _)) => Some(Send)
        case Some(Loop.Dispatch(_, _)) => Some(Dispatch)
        case Some(Loop.Expire(_, _)) => Some(Expire)
        case Some(Loop.Abandon(_, _)) => Some(Abandon)
        case None => Option.when(all.contains(name))(name)
      }

    /** The names of `recorded`, the steps a turn recorded in order, as a watcher is shown
      * them: a [[StepRecord.Received]] that ends the wait after `call:n:j:approve` is
      * `call:n:j:wait`; every other name is as recorded.
      */
    def named(recorded: Vector[String]): Vector[String] =
      recorded
        .foldLeft((Vector.empty[String], Option.empty[Loop.Approve])) { case ((out, asked), name) =>
          (Loop.of(name), asked) match {
            case (Some(ask: Loop.Approve), _) => (out :+ name, Some(ask))
            case (_, Some(Loop.Approve(n, j))) if name == StepRecord.Received =>
              (out :+ Loop.Wait(n, j).name, None)
            case _ if family(name).nonEmpty => (out :+ name, None)
            case _ => (out :+ name, asked)
          }
        }
        ._1
  }

  /** A step of the tool loop, by what its name says. */
  private val Loop = TurnRound.Names.Step

  /** The step `turn` is in, given what its workflow has `recorded` (a step is recorded when it
    * completes): the step [[Flows.read]] names next over the turn's flow, a person's wait by
    * its label (`call:n:j:wait`, [[Step.Wait]]); once every step is recorded, the last, named
    * as [[Step.named]] names it. A history that does not read as this build's turn (one of
    * another epoch) is in its last record, as recorded.
    */
  def running(turn: TurnRef, recorded: Vector[StepRecord]): String =
    Flows.read(TurnFlow.flow, turn, recorded).map(_.at) match {
      case Right(Flows.Read.Next(step, _)) => step
      case Right(Flows.Read.Ended(_, _)) =>
        Step.named(recorded.map(_.name)).lastOption.getOrElse(Step.PinModels)
      case Left(_) => recorded.lastOption.fold(Step.PinModels)(_.name)
    }

  /** The turn workflow's body, for the turn whose workflow id is `workflowId`: its flow
    * ([[TurnFlow]]), its model offered the tools its `offer` step recorded from `tooling`'s.
    * Returns what the turn did, for logs: its reply and summary are in the store, never in
    * this string.
    */
  def body[C^](env: TurnEnv^, tooling: TurnTooling[C]^)(
      workflowId: WorkflowId
  )(using d: Durable^): String =
    WorkflowId
      .as("turn", TurnRef.fromWorkflowId)(workflowId)
      .fold(
        identity,
        turn => {
          val turning = new TurnSteps.Turning(env, tooling, d.stream(TurnStream.Key))
          Flows.run(TurnFlow.flow, turning, turn).merge
        }
      )

  /** The id of the `i`-th search query assembly wrote for `turn`, from 0. */
  def queryId(turn: TurnRef, i: Int): EntryId = {
    val base = s"query:${WorkflowId.value(turn.workflowId)}"
    EntryId(if (i == 0) base else s"$base:$i")
  }

  /** The id of the record of `turn`'s window. */
  def windowId(turn: TurnRef): EntryId = EntryId(s"window:${WorkflowId.value(turn.workflowId)}")

  /** The pin each role's calls are made under, and the classification each read whose content
    * a model is sent is bounded by: a routed turn's [[Route]], its allotment laid over its turn
    * pin's effort, a summary uncleared failing `Uncleared`.
    */
  private[turn] final case class Picked(
      turn: Pinned,
      summary: Either[TurnFailure, Pinned],
      query: Option[Pinned],
      bound: Option[Label]
  ) extends Bounded

  private[turn] object Picked {
    def of(r: Route): Picked =
      Picked(
        r.turn.copy(assignment =
          r.turn.assignment.copy(effort = r.allotment.reasoning.over(r.turn.assignment.effort))
        ),
        r.summary.toRight(TurnFailure.Uncleared(r.classification)),
        r.query,
        Some(r.classification)
      )
  }

  /** The classification each read of a turn whose content a model is sent is bounded by;
    * `None`: unbounded.
    */
  private[turn] trait Bounded {
    def bound: Option[Label]

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

  /** A turn's route as its steps carry it: `route`, its pins named by their place in the
    * profile kept under `profile`, which the steps that make calls read back ([[picked]]).
    */
  private[turn] final case class Routed(profile: TurnProfileId, route: Route.Recorded)
      extends Bounded {
    def bound: Option[Label] = Some(route.classification)

    /** The calls the turn's loop is allotted. */
    def calls: Allotment.Calls = route.allotment.calls

    /** The pins this names, the profile read through `env`'s `db` by id
      * ([[TurnSteps.profileOf]], which says why every run reads the same one unrecorded):
      * `TurnFailure.Store` when the profile is not kept or has no pin where `route` names one.
      */
    def picked(env: TurnEnv^): Either[TurnFailure, Picked] =
      TurnSteps.profileOf(env, profile).flatMap(in)

    /** [[picked]], the profile read from `profiles` in this transaction. */
    def pickedIn(profiles: ModelProfileStore)(using Tx^): Either[TurnFailure, Picked] =
      TurnSteps.profileIn(profiles, profile).flatMap(in)

    private def in(p: TurnProfile): Either[TurnFailure, Picked] =
      route.in(p).left.map(TurnFailure.Store(_)).map(Picked.of)
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
    for {
      // The rounds asked, as each model call's input shows them, in order.
      rounds <- Flows
        .read(TurnFlow.flow, turn, steps)
        .left
        .map(why => s"$workflow does not read as this build's turn: $why")
        .map(_.observed.collect { case TurnRound.AskRecorded(n) => n })
      // Each asked round's ask:n:record, which kept its reply when it called tools, and its
      // gate:n, recorded when the round was gated.
      looped = rounds.map(n => Round.at(n) -> Loop.Record(n).name)
      gated = rounds.map(n => n -> Loop.Gate(n).name)
      byRef <- needed[TurnOffer.Recorded](Step.Offer)
      profileId <- needed[TurnProfileId](Step.PinModels)
      window <- needed[Window](Step.Assemble)
      classified <- output[TurnTopics.Classification](Step.Classify)
      profile <- kept("profile", reads.profiles.get(profileId))
        .flatMap(_.toRight(s"no profile kept under ${TurnProfileId.value(profileId)}"))
      offer <- kept("offer", TurnOffer.load(reads.toolSets, reads.prompts, byRef))
      (set, prompt) = (offer.tools, offer.prompt)
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
      answer <- recorded[TurnSteps.Appended](Step.Append).map(_.map(_.reply))
      gates <- gated.foldLeft[Either[String, Map[Int, Gated]]](Right(Map.empty)) {
        case (acc, (round, name)) =>
          acc.flatMap(done => recorded[Gated](name).map(g => done ++ g.map(round -> _)))
      }
      recordedRoute <- recorded[Route.Recorded](Step.Route)
      routed <- recordedRoute.fold(Right(None))(_.in(profile).map(Some(_)))
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
