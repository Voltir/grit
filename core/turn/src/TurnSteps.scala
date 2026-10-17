package grit.turn

import java.time.Instant

import grit.act.phase.{Asking, Hearing, Placing}
import grit.core.allot.Router
import grit.core.context.{AssemblyNote, Window}
import grit.core.document.Pointer
import grit.core.durable.{Flow, Step, StepName, StreamWriter}
import grit.core.id.{DocumentVersion, EntryId, EntrySeq, TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message, StopReason, Usage}
import grit.core.model.{Grade, Pinned, TurnProfile, TurnProfileId}
import grit.core.provider.ModelRequest
import grit.core.route.{Routed, Routing, Uncleared}
import grit.core.speech.{Outcome, Speech, SpeechStore}
import grit.core.stitch.{Along, Strand}
import grit.core.store.{
  Entry,
  EntryStore,
  ModelProfileStore,
  Nearby,
  Payload,
  StoreError,
  Tx,
  UsageLedger
}
import grit.core.tool.ToolName
import grit.core.triage.Weighing
import grit.core.visibility.{Integrity, Label, Subject}

import TurnRound.believedAt
import TurnShown.{own, requestOf, shown}
import TurnJournal.given

/** The turn's steps besides its loop's ([[TurnRound]]), each a value: what it is recorded as,
  * what it reads off the wire, and its body, acting through an [[Env]]. [[TurnFlow]] puts them
  * in order. A body that fails returns a [[TurnFailure]], recorded like any other output.
  */
private[turn] object TurnSteps {

  /** What the turn's steps act through: `env`, and each body that reads the turn's tools. It
    * has no capture-set parameter, and everything it hands out is pure or captures it alone,
    * as [[TurnRound.Env]]'s does.
    */
  trait Env {

    /** What the turn works with besides its tools. */
    def env: TurnEnv^{this}

    /** The `weigh` step's body: the answers `turn`'s root is weighed with. Never fails: unread
      * or unasked is a [[TurnWeighing.Weighed.Failed]]; nothing kept or asked, `None`.
      */
    def weigh(turn: TurnRef): Option[TurnWeighing.Weighed]

    /** The `offer` step's body: what `turn` offers its model, decided as `routed` bounds it
      * ([[TurnOffer.decide]]), its transaction knowing no more than that; and the costs of the
      * classifier call `weighed` made and of `routed`'s task call, in the ledger.
      */
    def offer(turn: TurnRef, weighed: Option[TurnWeighing.Weighed], routed: Turn.Routed)(using
        Tx^
    ): Either[TurnFailure, TurnOffer.Recorded]

    /** The limits a loop offered what `offer` names ([[TurnOffer.load]]), its calls allotted as
      * `routed` says, its model asked about `asked`, runs under: `TurnFailure.Store` when the
      * offer's tool set or prompt is not kept, `TurnFailure.Model` when its tools do not make
      * one toolbox ([[TurnShown.offered]]).
      */
    def limits(
        offer: TurnOffer.Recorded,
        routed: Turn.Routed,
        asked: Option[TurnTopics.Classification]
    )(using Tx^): Either[TurnFailure, TurnRound.Limits]

    /** What the turn's rounds act through. */
    def rounds: TurnRound.Env^{this}

    /** What the wait for the turn's message's placement acts through. */
    def placing: Placing.Env^{this}

    /** What round `at.round`'s request, offered as `offer` under `pins`, was sent: the stub of
      * each item its gate hid, and how it is shaped from the plain request. Fails as
      * [[TurnRound.Env.record]] does.
      */
    def sent(
        at: TurnRound.Progress,
        offer: TurnOffer,
        pins: Turn.Picked
    ): Either[TurnFailure, (Map[ItemRef, String], ModelRequest -> ModelRequest)]
  }

  /** The [[Env]] of a turn with `env`, offered `tooling`'s tools, its model told through
    * `heard`.
    */
  final class Turning[C^](turnEnv: TurnEnv^, tooling: TurnTooling[C]^, heard: StreamWriter^)
      extends Env {
    private val rounding = new Rounding(heard, turnEnv, tooling)

    def env: TurnEnv^{this} = turnEnv

    def placing: Placing.Env^{this} = turnEnv.stitching.placing

    def weigh(turn: TurnRef): Option[TurnWeighing.Weighed] = weighOf(turnEnv, tooling, turn)

    def offer(turn: TurnRef, weighed: Option[TurnWeighing.Weighed], routed: Turn.Routed)(using
        Tx^
    ): Either[TurnFailure, TurnOffer.Recorded] = {
      val (ledger, upstream) = (turnEnv.records.ledger, turnEnv.classifier.upstream)
      routed.known
        .flatMap(_ => turnEnv.speech.store.answering(turn))
        .left
        .map(TurnFailure.ofStore)
        .flatMap(answering =>
          TurnOffer.decide(
            turnEnv.hosting,
            turnEnv.records.entries,
            tooling,
            turn,
            weighed.flatMap(_.answers),
            answering,
            routed.at
          )
        )
        .flatMap(offered =>
          weighCost(ledger, upstream, turn, weighed)
            .flatMap(_ => taskCost(ledger, upstream, turn, routed.route))
            .map(_ => offered)
        )
    }

    def limits(
        offer: TurnOffer.Recorded,
        routed: Turn.Routed,
        asked: Option[TurnTopics.Classification]
    )(using Tx^): Either[TurnFailure, TurnRound.Limits] =
      turnEnv.hosting
        .offered(offer)
        .flatMap(o => TurnShown.offered(TurnOffer.toolbox(tooling, o), asked))
        .map(_ =>
          TurnRound.Limits(
            TurnLoop.Rounds.allotted(tooling.budget, routed.calls),
            tooling.answerWithin
          )
        )

    def rounds: TurnRound.Env^{this} = rounding

    def sent(
        at: TurnRound.Progress,
        offer: TurnOffer,
        pins: Turn.Picked
    ): Either[TurnFailure, (Map[ItemRef, String], ModelRequest -> ModelRequest)] =
      Rounding.sent(tooling, at, offer, pins)
  }

  /** The names of the turn's steps besides its loop's: the one home of each. */
  object Names {
    val pinModels: StepName[Any] = StepName("pin-models")
    val weigh: StepName[Any] = StepName("weigh")
    val route: StepName[Any] = StepName("route")
    val offer: StepName[Any] = StepName("offer")
    val recordFailure: StepName[Any] = StepName("record-failure")
    val stitched: StepName[Any] = StepName("stitched")
    val classify: StepName[Any] = StepName("classify")
    val recordTopic: StepName[Any] = StepName("record-topic")
    val search: StepName[Any] = StepName("search")
    val assemble: StepName[Any] = StepName("assemble")
    val recordWindow: StepName[Any] = StepName("record-window")
    val recordVerdict: StepName[Any] = StepName("record-verdict")
    val append: StepName[Any] = StepName("append")
    val judge: StepName[Any] = StepName("judge")
    val recordSpeech: StepName[Any] = StepName("record-speech")
    val summarise: StepName[Any] = StepName("summarise")
    val appendSummary: StepName[Any] = StepName("append-summary")
  }

  /** A step's input, of `turn`. */
  trait OfTurn {
    def turn: TurnRef
  }

  /** `turn`, ended for `failure` before its offer was made or read. */
  final case class Failed(turn: TurnRef, failure: TurnFailure) extends OfTurn

  /** `turn`, its models pinned as the profile kept under `profile`, its root weighed as
    * `weighed`.
    */
  final case class Weighed(
      turn: TurnRef,
      profile: TurnProfileId,
      weighed: Option[TurnWeighing.Weighed]
  ) extends OfTurn

  /** `turn`, its root weighed as `weighed`, routed as `route`: what its offer is made from. */
  final case class Offering(
      turn: TurnRef,
      weighed: Option[TurnWeighing.Weighed],
      route: Turn.Routed
  ) extends OfTurn

  /** `turn`, its calls made as `routed` says, offering what `offer` names; `notes`, what its
    * steps that never fail it said went wrong, for its log line.
    */
  final case class Offered(
      turn: TurnRef,
      routed: Turn.Routed,
      offer: TurnOffer.Recorded,
      notes: String
  ) extends OfTurn

  /** `at`, its message placed as `placing` says. */
  final case class Placed(at: Offered, placing: TurnTopics.Classification) extends OfTurn {
    def turn: TurnRef = at.turn
  }

  /** `at`, over the window `seen`. */
  final case class Assembled(at: Offered, placing: TurnTopics.Classification, seen: Window)
      extends OfTurn {
    def turn: TurnRef = at.turn
  }

  /** `turn`'s model asked about `asked`, its verdict read from `first` against `replies`. */
  final case class Verdict(
      turn: TurnRef,
      asked: TurnTopics.Classification,
      first: Message.Assistant,
      replies: Vector[Message.Assistant]
  ) extends OfTurn

  /** `at`'s loop answered as `looped` says. */
  final case class Answered(at: Assembled, looped: TurnRound.Looped) extends OfTurn {
    def turn: TurnRef = at.turn
  }

  /** What `append` kept: the reply or draft, and the window's documents whose pointer its
    * calls followed, each counted as followed.
    */
  final case class Appended(reply: EntryId, followed: Vector[DocumentVersion])

  /** What `at`'s steps from `assemble` to `append` came to: its reply entry, or why it has
    * none; the verdict's record when it failed without failing the turn; and the turn's level
    * at its end, `None` when it ended before its loop had a reply ([[TurnRound.Progress.level]]).
    */
  final case class Ended(
      at: Offered,
      result: Either[TurnFailure, EntryId],
      verdictUnrecorded: Option[TurnFailure],
      level: Option[Integrity]
  ) extends OfTurn {
    def turn: TurnRef = at.turn

    /** What the turn's log line adds after what it did. */
    def notes: String = at.notes + verdictUnrecorded.fold("")(f => s"; verdict not recorded: $f")
  }

  /** How a heard-rooted turn's draft is settled: an unprompted one by its `judgement`; a named
    * one unjudged.
    */
  enum Settling {
    case Judged(judgement: TurnJudge.Judgement)
    case Named
  }

  /** `at`'s draft, settled as `settling` says. */
  final case class DraftSettled(at: Ended, settling: Settling) extends OfTurn {
    def turn: TurnRef = at.turn
  }

  /** `turn`'s reply `answered` summarised, routed as `routed`, its rows believed at
    * `believed`.
    */
  final case class Summing(
      turn: TurnRef,
      routed: Turn.Routed,
      answered: EntryId,
      believed: Option[Integrity]
  ) extends OfTurn

  /** `at`'s summary, `message`. */
  final case class Summary(at: Summing, message: Message.Assistant) extends OfTurn {
    def turn: TurnRef = at.turn
  }

  private def onTurn(in: OfTurn): Subject = Subject.Turn(in.turn)

  /** `pin-models`: the catalog in force, pinned as the turn's profile: kept by the store, and
    * its id the step's output, so a replay makes every call under the profile the turn started
    * with.
    */
  val pinModels: Step[Env, TurnRef, Either[TurnFailure, TurnProfileId]] =
    Step.transact[Env, TurnRef, Either[TurnFailure, TurnProfileId]](
      Names.pinModels,
      Subject.Turn(_)
    )(r =>
      turn =>
        (tx: Tx^) ?=> {
          val env = r.env
          for {
            catalog <- env.models
              .catalog()
              .left
              .map(why => TurnFailure.Model(s"no model catalog: $why"))
            profile = catalog.pin
            _ <- env.records.profiles.pin(turn.workflowId, profile).left.map(TurnFailure.ofStore)
          } yield profile.id
        }
    )

  /** The profile kept under `id`, read through `env`'s `db`: profiles are kept by content id,
    * written once and never changed, so every run reads the same profile and the read needs
    * no recording. `TurnFailure.Store` when none is kept.
    */
  def profileOf(env: TurnEnv^, id: TurnProfileId): Either[TurnFailure, TurnProfile] =
    env.db
      .read(Subject.Public)(env.records.profiles.get(id))
      .left
      .map(TurnFailure.ofStore)
      .flatMap(kept(id, _))

  /** [[profileOf]], read from `profiles` in this transaction. */
  def profileIn(profiles: ModelProfileStore, id: TurnProfileId)(using
      Tx^
  ): Either[TurnFailure, TurnProfile] =
    profiles.get(id).left.map(TurnFailure.ofStore).flatMap(kept(id, _))

  private def kept(
      id: TurnProfileId,
      found: Option[TurnProfile]
  ): Either[TurnFailure, TurnProfile] =
    found.toRight(TurnFailure.Store(s"no profile kept under ${TurnProfileId.value(id)}"))

  /** `weigh` ([[Env.weigh]]). Taken by every turn, whatever the recipe, so a recovery under
    * another deployment's configuration replays the same steps.
    */
  val weigh: Step[Env, OfTurn, Option[TurnWeighing.Weighed]] =
    Step[Env, OfTurn, Option[TurnWeighing.Weighed]](Names.weigh)(r => in => r.weigh(in.turn))

  /** `route` ([[routeOf]]), under the profile `pin-models` kept ([[profileOf]], whose failure
    * it records). Taken as weigh is, so a recovery under another deployment's clearances
    * replays it.
    */
  val route: Step[Env, Weighed, Either[TurnFailure, Route.Recorded]] =
    Step[Env, Weighed, Either[TurnFailure, Route.Recorded]](Names.route)(r =>
      in =>
        profileOf(r.env, in.profile).flatMap(profile =>
          routeOf(r.env, in.turn, profile).flatMap(
            Route.recorded(_, profile).left.map(TurnFailure.Store(_))
          )
        )
    )

  /** `offer` ([[Env.offer]]), its transaction bounded as the turn was routed. */
  val offer: Step[Env, Offering, Either[TurnFailure, TurnOffer.Recorded]] =
    Step.transact[Env, Offering, Either[TurnFailure, TurnOffer.Recorded]](Names.offer, onTurn)(r =>
      in => (tx: Tx^) ?=> r.offer(in.turn, in.weighed, in.route)
    )

  /** `record-failure`: for a turn that ended before its offer was made or read, `Failed` kept
    * as what became of it when it answers a heard message
    * ([[grit.core.speech.SpeechStore.drafted]]), so its decision never stands drafting; for a
    * message said to grit, [[Uncleared.reply]] kept as its reply when its failure is
    * `Uncleared`, and nothing otherwise. Taken by every turn that fails there, whatever it
    * answers, since which it answers is read inside the step.
    */
  val recordFailure: Step[Env, Failed, Either[TurnFailure, Boolean]] =
    Step.transact[Env, Failed, Either[TurnFailure, Boolean]](Names.recordFailure, onTurn)(r =>
      in =>
        (tx: Tx^) ?=> {
          val env = r.env
          val (entries, store, turn) = (env.records.entries, env.speech.store, in.turn)
          val at = env.clock.now()
          entries
            .ofTurn(turn)
            .flatMap(own =>
              own.minByOption(_.seq).map(_.payload) match {
                case Some(Payload.Heard(_)) =>
                  store.drafted(turn, Outcome.Failed(in.failure.toString), None, at)
                case _ =>
                  in.failure match {
                    case TurnFailure.Uncleared(c) => replyUncleared(entries, turn, c, at)
                    case _ => Right(false)
                  }
              }
            )
            .left
            .map(TurnFailure.ofStore)
        }
    )

  /** `stitched`: [[Placing.placed]] of the turn's message, read for the turn;
    * `TurnFailure.Store` when the conversation cannot be read or the placement failed. Never
    * fails the turn.
    */
  val stitched: Flow[Env, OfTurn, Nothing, Either[TurnFailure, Option[String]]] =
    Placing
      .placed[OfTurn, TurnFailure](
        Names.stitched,
        _.turn,
        in => Subject.Turn(in.turn),
        {
          case Placing.Unplaced.Unread(e) => TurnFailure.ofStore(e)
          case Placing.Unplaced.Failed(why) => TurnFailure.Store(s"placement: $why")
        }
      )
      .over[Env](r => r.placing)

  /** `classify`: the turn's message placed among its conversation's topics
    * ([[TurnTopics.classify]]).
    */
  val classify: Step[Env, OfTurn, TurnTopics.Classification] =
    Step[Env, OfTurn, TurnTopics.Classification](Names.classify)(r =>
      in => {
        val (env, records) = (r.env, r.env.records)
        TurnTopics.classify(env.classifier, records.entries, env.db, records.estimator, in.turn)
      }
    )

  /** `record-topic`: the placement recorded ([[TurnTopics.record]]). */
  val recordTopic: Step[Env, Placed, Either[TurnFailure, EntryId]] =
    Step.transact[Env, Placed, Either[TurnFailure, EntryId]](Names.recordTopic, onTurn)(r =>
      in =>
        (tx: Tx^) ?=> {
          val (env, records) = (r.env, r.env.records)
          TurnTopics.record(
            records.entries,
            records.ledger,
            env.classifier.upstream,
            in.turn,
            in.placing,
            env.clock.now()
          )
        }
    )

  /** `record-window`: the turn's window and the queries that chose it, recorded before the
    * model is called, so an edge sees them while the turn runs, and each document the window
    * holds counted as placed; and the limits its loop runs under ([[Env.limits]]), checked
    * first, so a turn whose tools do not make one toolbox records no window. A query's cost is
    * not kept here: its move, `ask:query`, kept it ([[TurnAssembly]]).
    */
  val recordWindow: Step[Env, Assembled, Either[TurnFailure, TurnRound.Limits]] =
    Step.transact[Env, Assembled, Either[TurnFailure, TurnRound.Limits]](
      Names.recordWindow,
      onTurn
    )(r =>
      in =>
        (tx: Tx^) ?=>
          r.limits(in.at.offer, in.at.routed, TurnShown.asked(in.placing))
            .flatMap(limits =>
              windowRecorded(r.env.records, in.turn, in.seen, r.env.clock.now()).map(_ => limits)
            )
    )

  /** `record-verdict`: the model's verdict on the turn's topic, when the classifier was unsure
    * ([[TurnVerdict]]).
    */
  val recordVerdict: Step[Env, Verdict, Either[TurnFailure, EntryId]] =
    Step.transact[Env, Verdict, Either[TurnFailure, EntryId]](Names.recordVerdict, onTurn)(r =>
      in =>
        (tx: Tx^) ?=> {
          val (env, turn) = (r.env, in.turn)
          TurnTopics.writeEvents(
            env.records.entries,
            env.records.ledger,
            turn,
            TurnVerdict.verdictId(turn),
            TurnVerdict.events(
              TurnVerdict.of(in.asked, in.first),
              TurnVerdict.anomaly(in.replies),
              in.asked,
              turn
            ),
            None,
            None,
            env.clock.now()
          )
        }
    )

  /** `append` ([[appended]]): the loop's answer kept as the turn's reply, or its draft, under
    * the offer its `offer` step names and the pins its `route` step names
    * ([[TurnHosting.offered]], [[Turn.Routed.pickedIn]], whose failures it records).
    */
  val append: Step[Env, Answered, Either[TurnFailure, Appended]] =
    Step.transact[Env, Answered, Either[TurnFailure, Appended]](Names.append, onTurn)(r =>
      in =>
        (tx: Tx^) ?=> {
          val (env, rounds, a) = (r.env, in.looped.at, in.at.at)
          (for {
            offer <- env.hosting.offered(a.offer)
            pins <- a.routed.pickedIn(env.records.profiles)
            (stubs, shape) <- r.sent(rounds, offer, pins)
          } yield (offer, pins, stubs, shape))
            .flatMap((offer, pins, stubs, shape) =>
              appended(
                offer,
                env.records,
                env.speech.store,
                in.turn,
                in.at.seen,
                stubs,
                in.looped.answer,
                pins.turn.grade,
                shape,
                Some(rounds.level),
                env.clock.now()
              )
            )
        }
    )

  /** `judge` ([[judgeDraft]]). */
  val judge: Step[Env, OfTurn, TurnJudge.Judgement] =
    Step[Env, OfTurn, TurnJudge.Judgement](Names.judge)(r => in => judgeDraft(r.env, in.turn))

  /** `record-speech` for a heard-rooted turn that failed: `Failed` kept as what became of its
    * draft.
    */
  val speechFailed: Step[Env, Failed, Either[TurnFailure, Outcome]] =
    Step.transact[Env, Failed, Either[TurnFailure, Outcome]](Names.recordSpeech, onTurn)(r =>
      in =>
        (tx: Tx^) ?=> {
          val failed = Outcome.Failed(in.failure.toString)
          r.env.speech.store
            .drafted(in.turn, failed, None, r.env.clock.now())
            .left
            .map(TurnFailure.ofStore)
            .map(_ => failed)
        }
    )

  /** `record-speech` for a heard-rooted turn's draft ([[recordSpeech]]). */
  val speechSettled: Step[Env, DraftSettled, Either[TurnFailure, Outcome]] =
    Step.transact[Env, DraftSettled, Either[TurnFailure, Outcome]](Names.recordSpeech, onTurn)(r =>
      in => (tx: Tx^) ?=> recordSpeech(r.env, in.turn, in.settling, in.at.level, r.env.clock.now())
    )

  /** `summarise`: the summary of the turn whose reply is `answered`, asked of the summary's
    * pin, again while the summarizer is unavailable, as a reply is ([[Asking.reply]]), with
    * the name and description of its message's topic; its reasoning dropped.
    * `Uncleared` when no summary pin is cleared; `Store` when its route's pins do not read
    * back ([[Turn.Routed.picked]]).
    */
  val summarise: Step[Env, Summing, Either[TurnFailure, Message.Assistant]] =
    Step[Env, Summing, Either[TurnFailure, Message.Assistant]](Names.summarise)(r =>
      in => {
        val (env, turn) = (r.env, in.turn)
        in.routed
          .picked(env)
          .flatMap(pins => pins.summary.map((pins, _)))
          .flatMap { (pins, pin) =>
            pins
              .reads(env.db)
              .read(Subject.Turn(turn))(env.records.entries.list(turn.conversationId))
              .left
              .map(TurnFailure.ofStore)
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
    )

  /** `append-summary` ([[appendSummary]]), at the grade of the summary pin its route names
    * ([[Turn.Routed.pickedIn]], whose failure it records).
    */
  val appendSummary: Step[Env, Summary, Either[TurnFailure, EntryId]] =
    Step.transact[Env, Summary, Either[TurnFailure, EntryId]](Names.appendSummary, onTurn)(r =>
      in =>
        (tx: Tx^) ?=> {
          val s = in.at
          s.routed
            .pickedIn(r.env.records.profiles)
            .flatMap(_.summary)
            .flatMap(pin =>
              appendSummaryOf(
                r.env.records,
                s.turn,
                s.answered,
                in.message,
                pin.grade,
                s.believed,
                r.env.clock.now()
              )
            )
        }
    )

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
      .map(TurnFailure.ofStore)
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
      routed: Route.Recorded
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
          .map(TurnFailure.ofStore)
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
  private def weighOf[C^](
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
          .map(TurnFailure.ofStore)
      case Some(TurnWeighing.Weighed.Kept(_) | TurnWeighing.Weighed.Failed(_)) | None =>
        Right(())
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
          window = all
            .collectFirst {
              case Entry(id, _, _, _, _, Payload.Window(entries, _, nearby, documents), _, _)
                  if id == Turn.windowId(turn) =>
                Window(entries = entries, nearby = nearby, documents = documents)
            }
            .getOrElse(Window(Vector.empty))
          // What the model was shown, read as its own request reads it.
          showing <- TurnShown.shown(records, turn, window)
          strand <- strandOf(stitching, conversations, turn, all, now)
          named <- records.principals.speakers((all ++ strand.shown).map(_.id))
          // Routed by the read that built the state, documents included: the classifier is
          // asked only when its service is cleared for it.
        } yield (Routed.of, Judging(all, window, showing, strand, named))
      } match {
      case Left(e) => TurnJudge.Judgement.Unjudged(s"store: ${TurnFailure.describe(e)}")
      case Right((routed, Judging(all, window, showing, strand, named))) =>
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
                    showing,
                    window,
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
      window: Window,
      showing: TurnShown.Showing,
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
      all <- entries.list(turn.conversationId).left.map(TurnFailure.ofStore)
      root <- all
        .filter(_.turnSeq == turn.turnSeq)
        .minByOption(_.seq)
        .toRight(TurnFailure.Store(s"${WorkflowId.value(turn.workflowId)} has no entries"))
      draft <- all
        .collectFirst {
          case Entry(id, _, _, _, _, Payload.Draft(m), _, _) if id == turn.draftId => m
        }
        .toRight(TurnFailure.Store(s"${WorkflowId.value(turn.workflowId)} has no draft"))
      reach <- store.reach(turn).left.map(TurnFailure.ofStore)
      // The strand after the root, to now: the assistant's reply there holds the draft too.
      strand <- strandOf(
        env.stitching,
        env.hosting.conversations,
        turn,
        all,
        at.plusSeconds(1)
      ).left
        .map(TurnFailure.ofStore)
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
          .map(TurnFailure.ofStore)
      )
      _ <- (outcome, reach.flatMap(_.replyTo)) match {
        case (Outcome.Posted(_), Some(to)) =>
          for {
            next <- entries.lockNext(turn.conversationId).left.map(TurnFailure.ofStore)
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
              .map(TurnFailure.ofStore)
            _ <- deliveries.await(turn, to).left.map(TurnFailure.ofStore)
          } yield ()
        case _ => Right(())
      }
      _ <- store.drafted(turn, outcome, TurnJudge.said(draft), at).left.map(TurnFailure.ofStore)
    } yield outcome
  }

  /** The summary request for `turn`, from `all` of its conversation's entries: asking after
    * its topic too when it has one.
    */
  private def summaryRequest(all: Vector[Entry], turn: TurnRef): ModelRequest =
    TurnSummary.request(own(all, turn), TurnTopics.topicOf(all, turn))

  /** Records the summary in `message` as `turn`'s summary, dated `at`, a child of its reply
    * `answered`, after everything already in the conversation, believed as
    * [[TurnRound.believedAt]] says of `believed`; and what it cost in the ledger beside the
    * estimate of the request that produced it, rebuilt as [[appended]] rebuilds its own.
    * What it says of the message's topic is recorded after it ([[TurnTopics.describedId]]).
    */
  private def appendSummaryOf(
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
      next <- entries.lockNext(turn.conversationId).left.map(TurnFailure.ofStore)
      level <- believedAt(entries, turn, believed)
      all <- entries.list(turn.conversationId).left.map(TurnFailure.ofStore)
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
        .map(TurnFailure.ofStore)
      estimate = estimator.request(summaryRequest(all, turn))
      _ <- Asking
        .spent(ledger, id, turn, message, Some(grade), estimate)
        .left
        .map(TurnFailure.ofStore)
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

  /** Records `message` as `turn`'s reply entry ([[TurnRef.replyId]]), or, when `offer`'s
    * root is a heard message it drafts a reply to, as its draft ([[TurnRef.draftId]],
    * [[Payload.Draft]]), believed as [[TurnRound.believedAt]] says of `believed`; a reply to a
    * heard message answered as said to grit is kept as its outcome in `speech`
    * ([[Outcome.Replied]]) with its words; dated `at`, after everything already in the
    * conversation, and what it cost in the ledger beside the estimate of the request that
    * produced it: rebuilt from the same window and the same entries (the turn's own were all
    * recorded before its call, and the reply is not yet among them), each item `stubs` names
    * shown as its stub, shaped by `shape`. Each of the window's documents whose pointer one of
    * the turn's calls followed, and that succeeded, is counted as followed at `at`
    * ([[Pointer.followedIn]]).
    */
  private def appended(
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
  )(using Tx^): Either[TurnFailure, Appended] = {
    val (id, payload) = offer.root match {
      case TurnOffer.Root.Addressed | TurnOffer.Root.ByName =>
        (turn.replyId, Payload.Message(message))
      case TurnOffer.Root.Heard | TurnOffer.Root.Named => (turn.draftId, Payload.Draft(message))
    }
    val TurnRecords(entries, ledger, estimator, _, _, _) = records
    for {
      next <- entries.lockNext(turn.conversationId).left.map(TurnFailure.ofStore)
      level <- believedAt(entries, turn, believed)
      showing <- shown(records, turn, window).left.map(TurnFailure.ofStore)
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
        .map(TurnFailure.ofStore)
      _ <- Asking
        .spent(ledger, id, turn, message, Some(grade), estimator.request(sent))
        .left
        .map(TurnFailure.ofStore)
      followed = Pointer.followedIn(showing.documents.map(_._1), callsOf(showing.mine))
      _ <- records.documents.followed(followed, at).left.map(TurnFailure.ofStore)
      _ <- offer.root match {
        case TurnOffer.Root.ByName =>
          speech
            .drafted(turn, Outcome.Replied, TurnJudge.said(message), at)
            .left
            .map(TurnFailure.ofStore)
        case TurnOffer.Root.Addressed | TurnOffer.Root.Heard | TurnOffer.Root.Named => Right(true)
      }
    } yield Appended(id, followed)
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
    * dated `at`, and each document the window holds counted as placed at `at`.
    */
  private def windowRecorded(
      records: TurnRecords,
      turn: TurnRef,
      window: Window,
      at: Instant
  )(using Tx^): Either[TurnFailure, EntryId] =
    for {
      next <- records.entries.lockNext(turn.conversationId).left.map(TurnFailure.ofStore)
      level <- records.entries.believed(turn).left.map(TurnFailure.ofStore)
      _ <- writeWindow(records.entries, turn, window, next.seq, level, at)
      _ <- records.documents.placed(window.documents, at).left.map(TurnFailure.ofStore)
    } yield Turn.windowId(turn)

  /** Writes each query `window`'s notes say assembly wrote, as [[Turn.queryId]], then the
    * window itself, as [[Turn.windowId]], from position `from`, dated `at`, believed at
    * `level`; the position after the last one written.
    */
  private def writeWindow(
      entries: EntryStore,
      turn: TurnRef,
      window: Window,
      from: EntrySeq,
      level: Integrity,
      at: Instant
  )(using Tx^): Either[TurnFailure, EntrySeq] = {
    val queries = window.notes.collect { case q: AssemblyNote.Queried => q }
    val recalled = window.notes.flatMap {
      case AssemblyNote.Recalled(turns) => turns
      case _ => Vector.empty
    }
    for {
      windowSeq <- queries.zipWithIndex.foldLeft[Either[TurnFailure, EntrySeq]](Right(from)) {
        case (done, (q, i)) =>
          done.flatMap { seq =>
            val qid = Turn.queryId(turn, i)
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
              .left
              .map(TurnFailure.ofStore)
              .map(_ => seq.next)
          }
      }
      _ <- entries
        .insert(
          Entry(
            Turn.windowId(turn),
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
        .map(TurnFailure.ofStore)
    } yield windowSeq.next
  }
}
