package grit.turn

import scala.concurrent.duration.FiniteDuration

import grit.core.classify.Classifier
import grit.core.clock.{Clock, Fresh}
import grit.core.context.ContextAssembler
import grit.core.edge.{Deliveries, EdgeDirectory, ToolRequests}
import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.persona.Persona
import grit.core.place.{Reaches, WorksIn}
import grit.core.provider.{Models, TokenEstimator}
import grit.core.recipe.TurnRecipe
import grit.core.speech.{Speaking, SpeechStore}
import grit.core.stitch.{Placements, StitchStore, Tuning}
import grit.core.store.{
  ConversationStore,
  Db,
  EntrySearch,
  EntryStore,
  Jot,
  LifecycleStore,
  ModelProfileStore,
  Principals,
  PromptStore,
  UsageLedger,
  VoiceStore
}
import grit.core.tool.{Tool, ToolSets, Toolbox}
import grit.core.triage.{KnowledgeSources, Tags, TriageStore, Weighing}

/** What a turn works with besides its `Durable` and its [[TurnTooling]]: its [[TurnRecords]]
  * and [[TurnHosting]], then the capabilities it calls. `assembler` builds its window,
  * `classifier` places its message among the topics, `models` holds the catalog the turn
  * pins and makes each role's calls under its pin, `db` reads the store outside a
  * transaction, `clock` dates entries and paces the reply's stream, and `fresh` tags each
  * attempt at a model call ([[TurnStream]]); `classifier` also judges an unprompted turn's
  * draft, settled as [[TurnSpeech]] says, and stitches a conversation's first message as
  * [[TurnStitching]] says. None of them writes the store but `stitching`'s `placements`,
  * which queues an opening's placement and waits for it, so a step body that captures this
  * can otherwise only read it. `weighing` is what its root's answers are read from, or
  * asked of, before its offer ([[TurnWeighing]]).
  */
final case class TurnEnv(
    records: TurnRecords,
    hosting: TurnHosting,
    assembler: ContextAssembler^,
    classifier: Classifier^,
    models: Models^,
    db: Db^,
    clock: Clock^,
    fresh: Fresh^,
    speech: TurnSpeech,
    stitching: TurnStitching^,
    weighing: TurnWeighing^
)

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

  /** The id `turn`'s asking of its root is kept under in the usage ledger; no entry has it. */
  def id(turn: TurnRef): EntryId = EntryId(s"weigh:${WorkflowId.value(turn.workflowId)}")
}

/** How a conversation's first message is stitched to an exchange in its room, and its strand
  * read (ADR 0023): the placements kept (`stitches`), the room's `search`, the scope in force
  * (`lifecycle`), the `tuning`, and where its opening is placed (`placements`), which a turn
  * begun before openings were placed on their own does itself.
  */
final case class TurnStitching(
    stitches: StitchStore,
    search: EntrySearch,
    lifecycle: LifecycleStore,
    tuning: Tuning,
    placements: Placements^
)

/** What a turn rooted on a heard message settles its draft against (ADR 0022): whether and
  * within what grit speaks (`speaking`, read when the draft is settled), where each decision
  * is kept (`store`), and the replies edges await (`deliveries`), where a posted one is sent.
  */
final case class TurnSpeech(speaking: Speaking, store: SpeechStore, deliveries: Deliveries)

/** Where a turn's entries, their costs and its model profile are written, how a request is
  * priced, and whose names its messages are shown under (`principals`).
  */
final case class TurnRecords(
    entries: EntryStore,
    ledger: UsageLedger,
    estimator: TokenEstimator,
    profiles: ModelProfileStore,
    principals: Principals
)

/** What a turn offers, and where its hosted calls go (ADR 0017): each conversation's origin,
  * the system prompt and tool set each turn was offered (by content id), the requests its
  * hosted calls become, which edge serves a place, and `voices`, the voice the turn's prompt
  * speaks in.
  */
final case class TurnHosting(
    conversations: ConversationStore,
    prompts: PromptStore,
    toolSets: ToolSets,
    requests: ToolRequests,
    edges: EdgeDirectory,
    voices: VoiceStore
)

/** The tools a turn's model may call in its loop ([[TurnLoop]]), and how the loop runs.
  * `tools` run in the turn's own steps and act only through the capabilities `C`;
  * `operator`, too, but are offered only in a conversation with the person running grit
  * ([[grit.core.store.Audience.operator]]), after `tools`; `hosted` are run by an edge serving the conversation's workspace, and act through nothing here.
  * `jot` keeps each call's result from inside its step ([[TurnTools.Settling]]); `budget`
  * bounds its model calls, the last made with tools off; a call a person approves first
  * waits `answerWithin` for their answer. `worksIn` gives a conversation with no directory
  * its workspace ([[WorksIn.of]]); `reaches` gives an addressed turn the services it reaches
  * besides it ([[Reaches.of]]). `recipe` shapes each turn by what it answers: its window's
  * width, and which of those services' tools it is offered, by the knowledge sources
  * `knowledge` says supply each ([[grit.core.triage.KnowledgeSources.supplied]]). `persona`
  * is who the turn is told it is ([[TurnPrompt.called]]).
  */
final case class TurnTooling[C^](
    tools: Toolbox[C],
    operator: Toolbox[C],
    hosted: Vector[Tool.Offered],
    jot: Jot^,
    budget: TurnLoop.Budget,
    answerWithin: FiniteDuration = TurnTools.AnswerWithin,
    worksIn: Vector[WorksIn] = Vector.empty,
    reaches: Vector[Reaches] = Vector.empty,
    recipe: TurnRecipe = TurnRecipe.Shipped,
    knowledge: KnowledgeSources = KnowledgeSources.Empty,
    persona: Persona = Persona.Grit
)
