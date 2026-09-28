package grit.turn

import scala.concurrent.duration.FiniteDuration

import grit.core.classify.Classifier
import grit.core.clock.{Clock, Fresh}
import grit.core.context.ContextAssembler
import grit.core.edge.{EdgeDirectory, ToolRequests}
import grit.core.provider.{Models, TokenEstimator}
import grit.core.store.{
  ConversationStore,
  Db,
  EntryStore,
  Jot,
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
  * attempt at a model call ([[TurnStream]]). None of them writes the store, so a step body
  * that captures this can only read it.
  */
final case class TurnEnv(
    records: TurnRecords,
    hosting: TurnHosting,
    assembler: ContextAssembler^,
    classifier: Classifier^,
    models: Models^,
    db: Db^,
    clock: Clock^,
    fresh: Fresh^
)

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
  * ([[grit.core.store.Origin.operator]]), after `tools`; `hosted` are run by an edge serving the conversation's workspace, and act through nothing here.
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
