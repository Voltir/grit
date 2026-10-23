package grit.turn

import scala.concurrent.duration.FiniteDuration

import grit.act.moves.{MoveRecords, MovesEnv}
import grit.act.phase.Placing
import grit.core.allot.Tasking
import grit.core.classify.ClearedClassifier
import grit.core.clock.{Clock, Fresh}
import grit.core.context.ContextAssembler
import grit.core.document.DocumentStore
import grit.core.files.PieceStore
import grit.core.edge.{Deliveries, EdgeDirectory, ToolRequests}
import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.persona.Persona
import grit.core.place.{Reaches, WorksIn}
import grit.core.provider.{Models, TokenEstimator}
import grit.core.recipe.TurnRecipe
import grit.core.speech.{Speaking, SpeechStore}
import grit.core.spend.{Budget, Spending}
import grit.core.stitch.{StitchStore, Tuning}
import grit.core.store.{
  Askers,
  ConversationStore,
  Db,
  EntrySearch,
  EntryStore,
  Jot,
  ModelProfileStore,
  Principals,
  PromptStore,
  Tx,
  UsageLedger,
  VoiceStore
}
import grit.core.tool.{Gate, Tool, ToolSets, Toolbox}
import grit.core.triage.{Corpora, Tags, TriageStore, Weighing}

/** What a turn works with besides its `Durable` and its [[TurnTooling]]: its [[TurnRecords]]
  * and [[TurnHosting]], then the capabilities it calls. `assembler` builds its window,
  * `classifier` places its message on the agenda, `models` holds the catalog the turn
  * pins and makes each role's calls under its pin, `db` reads the store outside a
  * transaction, `clock` dates entries and paces the reply's stream, and `fresh` tags each
  * attempt at a model call ([[TurnStream]]); `classifier` also judges an unprompted turn's
  * draft, settled as [[TurnSpeech]] says, and stitches a conversation's first message as
  * [[TurnStitching]] says. None of them writes the store but `stitching`'s `placements`,
  * which queues an opening's placement and waits for it, so a step body that captures this
  * can otherwise only read it. `weighing` is what its root's answers are read from, or
  * asked of, before its offer ([[TurnWeighing]]); `routing`, what its `route` step reads
  * besides its clearances ([[TurnRouting]]). `moveRecords` keeps the costs of the asks it makes
  * as moves ([[moves]]).
  */
final case class TurnEnv(
    records: TurnRecords,
    hosting: TurnHosting,
    moveRecords: MoveRecords,
    assembler: ContextAssembler,
    classifier: ClearedClassifier^,
    models: Models^,
    db: Db^,
    clock: Clock^,
    fresh: Fresh^,
    speech: TurnSpeech,
    stitching: TurnStitching^,
    weighing: TurnWeighing^,
    routing: TurnRouting^
) {

  /** What the turn's moves are made with: its own models, classifier, db and clock, their costs
    * kept by `moveRecords`.
    */
  def moves: MovesEnv^{this} =
    MovesEnv(moveRecords, models, classifier, db, clock)
}

/** What a turn's `route` step reads besides its clearances: the task questions it asks its root
  * when the pins it may be routed to are of more than one grade (`tasks`), and the daily budget
  * and the spend its headroom is read from.
  */
final case class TurnRouting(tasks: Tasking^, budget: Budget, spending: Spending)

/** What a turn's root is weighed by before its offer (ADR 0025): the tags live triage kept
  * for a heard message (`triage`), or, for a message said to grit whose answers its recipe
  * reads, live triage's set asked of it (`said`), which keeps nothing.
  */
final case class TurnWeighing(triage: TriageStore, said: Weighing^)

object TurnWeighing {

  /** What a turn's root was weighed with, by where it came from, or why asking failed. */
  enum Weighed {

    /** The tags live triage kept for the heard message the turn answers. */
    case Kept(kept: Tags)

    /** Live triage's set asked of the message said to grit the turn answers: a classifier
      * call the turn made, whose cost its `offer` step records under [[id]].
      */
    case Asked(asked: Weighing.Weighed)

    /** The turn's root was to be weighed, and was not, for `why`: its stores did not read
      * ([[Weighing.Unweighed.Unread]]), or live triage's set was to be asked of the message
      * said to grit it answers and was not. Its offer withholds nothing.
      */
    case Failed(why: Weighing.Unweighed)

    /** The answers its offer reads; none when asking failed. */
    def answers: Option[Tags] = this match {
      case Kept(kept) => Some(kept)
      case Asked(asked) => Some(asked.tags)
      case Failed(_) => None
    }
  }

  /** The id `turn`'s asking of its root is kept under in the usage ledger, in its `offer`
    * step's transaction; no entry has it.
    */
  def id(turn: TurnRef): EntryId = EntryId(s"weigh:${WorkflowId.value(turn.workflowId)}")
}

/** How a conversation's first message is stitched to an exchange in its room, and its strand
  * read (ADR 0023): the placements kept (`stitches`), the room's `search`, the `tuning`, and
  * what its opening's placement is waited on through (`placing`).
  */
final case class TurnStitching(
    stitches: StitchStore,
    search: EntrySearch,
    tuning: Tuning,
    placing: Placing.Env^
)

/** What a turn rooted on a heard message settles its draft against (ADR 0022): whether and
  * within what grit speaks (`speaking`, read when the draft is settled), where each decision
  * is kept (`store`), and the replies edges await (`deliveries`), where a posted one is sent.
  */
final case class TurnSpeech(speaking: Speaking, store: SpeechStore, deliveries: Deliveries)

/** Where a turn's entries, their costs and its model profile are written, how a request is
  * priced, whose names its messages are shown under (`principals`), and the plugins'
  * documents and pieces its window shows and counts as placed (`documents`, `pieces`).
  */
final case class TurnRecords(
    entries: EntryStore,
    ledger: UsageLedger,
    estimator: TokenEstimator,
    profiles: ModelProfileStore,
    principals: Principals,
    documents: DocumentStore,
    pieces: PieceStore
)

/** What a turn offers, and where its hosted calls go (ADR 0017): each conversation's origin,
  * the system prompt and tool set each turn was offered (by content id), the requests its
  * hosted calls become, which edge serves a place, `voices`, the voice the turn's prompt
  * speaks in, and `askers`, who each turn's requests are made for.
  */
final case class TurnHosting(
    conversations: ConversationStore,
    prompts: PromptStore,
    toolSets: ToolSets,
    requests: ToolRequests,
    edges: EdgeDirectory,
    voices: VoiceStore,
    askers: Askers
) {

  /** The offer `recorded` names, read from `toolSets` and `prompts` by id ([[TurnOffer.load]],
    * which says why a step's body may read it on every run unrecorded); `TurnFailure.Store`
    * naming the set or fragment not kept.
    */
  def offered(recorded: TurnOffer.Recorded)(using Tx^): Either[TurnFailure, TurnOffer] =
    TurnOffer.load(toolSets, prompts, recorded).left.map(TurnFailure.ofStore)
}

/** The tools a turn's model may call in its loop ([[TurnLoop]]), and how the loop runs.
  * `tools` run in the turn's own steps and act only through the capabilities `C`;
  * `operator`, too, but are offered only in a conversation with the person running grit
  * ([[grit.core.store.Audience.operator]]), after `tools`; `hosted` are run by an edge serving the conversation's workspace, and act through nothing here.
  * `jot` keeps each call's result from inside its step ([[TurnTools.Settling]]); `budget`
  * bounds its model calls, the last made with tools off; a call a person approves first
  * waits its tool's own time for their answer ([[grit.core.tool.Gate.Ask]]), at most
  * `answerWithin`. `worksIn` gives a conversation with no directory
  * its workspace ([[WorksIn.of]]); `reaches` gives an addressed turn the services it reaches
  * besides it ([[Reaches.of]]). `recipe` shapes each turn by what it answers: its window's
  * width, and which of those services' tools it is offered, by the corpora
  * `knowledge` says supply each ([[grit.core.triage.Corpora.supplied]]). `persona`
  * is who the turn is told it is ([[TurnPrompt.called]]). `administering`, after `operator`,
  * are offered only on a turn a person put to grit (its root `Addressed` or `ByName`) in a room
  * an edge holds with colleagues ([[grit.core.store.Audience.Colleagues]]), never a heard or
  * unprompted turn, nor one in a direct message, a job's run or a directory's conversation;
  * whatever tools the deployment's `Offered` chose, which governs only the coding and operator
  * tools.
  */
final case class TurnTooling[C^](
    tools: Toolbox[C],
    operator: Toolbox[C],
    hosted: Vector[Tool.Offered],
    jot: Jot^,
    budget: TurnLoop.Rounds,
    answerWithin: FiniteDuration = Gate.AnswerWithin,
    worksIn: Vector[WorksIn] = Vector.empty,
    reaches: Vector[Reaches] = Vector.empty,
    recipe: TurnRecipe = TurnRecipe.Shipped,
    knowledge: Corpora = Corpora.Empty,
    persona: Persona = Persona.Grit,
    administering: Toolbox[C] = Toolbox.Empty
)
