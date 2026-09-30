package grit.turn

import scala.concurrent.duration.FiniteDuration

import grit.core.classify.Classifier
import grit.core.clock.{Clock, Fresh}
import grit.core.context.ContextAssembler
import grit.core.edge.{Deliveries, EdgeDirectory, ToolRequests}
import grit.core.provider.{Models, TokenEstimator}
import grit.core.speech.{Speaking, SpeechStore}
import grit.core.stitch.{StitchStore, Tuning}
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

/** What a turn works with besides its `Durable` and its [[TurnTooling]]: its [[TurnRecords]]
  * and [[TurnHosting]], then the capabilities it calls. `assembler` builds its window,
  * `classifier` places its message among the topics, `models` holds the catalog the turn
  * pins and makes each role's calls under its pin, `db` reads the store outside a
  * transaction, `clock` dates entries and paces the reply's stream, and `fresh` tags each
  * attempt at a model call ([[TurnStream]]); `classifier` also judges an unprompted turn's
  * draft, settled as [[TurnSpeech]] says, and stitches a conversation's first message as
  * [[TurnStitching]] says. None of them writes the store, so a step body that captures this
  * can only read it.
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
    stitching: TurnStitching
)

/** How a conversation's first message is stitched to an exchange in its room, and its strand
  * read (ADR 0023): the placements kept (`stitches`), the room's `search`, the scope in force
  * (`lifecycle`), and the `tuning`.
  */
final case class TurnStitching(
    stitches: StitchStore,
    search: EntrySearch,
    lifecycle: LifecycleStore,
    tuning: Tuning
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
  * hosted calls become, which edge serves a place, `voices`, the voice the turn's prompt
  * speaks in, and `principals`, what the assistant is called where the turn is.
  */
final case class TurnHosting(
    conversations: ConversationStore,
    prompts: PromptStore,
    toolSets: ToolSets,
    requests: ToolRequests,
    edges: EdgeDirectory,
    voices: VoiceStore,
    principals: Principals
)

/** The tools a turn's model may call in its loop ([[TurnLoop]]), and how the loop runs.
  * `tools` run in the turn's own steps and act only through the capabilities `C`;
  * `operator`, too, but are offered only in a conversation with the person running grit
  * ([[grit.core.store.Audience.operator]]), after `tools`; `hosted` are run by an edge serving the conversation's workspace, and act through nothing here.
  * `jot` keeps each call's result from inside its step ([[TurnTools.Settling]]); `budget`
  * bounds its model calls, the last made with tools off; a call a person approves first
  * waits `answerWithin` for their answer.
  */
final case class TurnTooling[C^](
    tools: Toolbox[C],
    operator: Toolbox[C],
    hosted: Vector[Tool.Offered],
    jot: Jot^,
    budget: TurnLoop.Budget,
    answerWithin: FiniteDuration = TurnTools.AnswerWithin
)
