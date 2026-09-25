package grit.turn

import scala.concurrent.duration.FiniteDuration

import grit.core.classify.Classifier
import grit.core.clock.{Clock, Fresh}
import grit.core.context.ContextAssembler
import grit.core.host.{Edits, Shell, Workspace}
import grit.core.provider.{Provider, TokenEstimator}
import grit.core.store.{Db, EntryStore, Jot, UsageLedger}
import grit.core.tool.Toolbox

/** What a turn works with besides its `Durable`: its system prompt and [[TurnRecords]],
  * then the capabilities it calls. `assembler` builds its window, `classifier` places its
  * message among the topics, `provider` answers, `summarizer` summarises, `db` reads the
  * store outside a transaction, `jot` records each tool call's result as it settles
  * ([[TurnTools]]), `clock` dates entries and paces the reply's stream, `fresh` tags each
  * attempt at a model call ([[TurnStream]]), and [[TurnTooling]] says which tools its
  * model is offered.
  */
final case class TurnEnv(
    system: String,
    records: TurnRecords,
    assembler: ContextAssembler^,
    classifier: Classifier^,
    provider: Provider^,
    summarizer: Provider^,
    db: Db^,
    jot: Jot^,
    clock: Clock^,
    fresh: Fresh^,
    tooling: TurnTooling^
)

/** Where a turn's entries and their costs are written, and how a request is priced. */
final case class TurnRecords(entries: EntryStore, ledger: UsageLedger, estimator: TokenEstimator)

/** The tools a turn's model may call in its loop ([[TurnLoop]]): `tools`, which act only
  * through `workspace`, `edits` and `shell`, and may be offered fewer than all three allow.
  * `budget` bounds the loop's model calls, the last made with tools off; under `strict` each
  * tool's schema asks the provider to hold the model's arguments to it. A call a person
  * approves first waits `answerWithin` for their answer.
  */
final case class TurnTooling(
    workspace: Workspace^,
    edits: Edits^,
    shell: Shell^,
    tools: Toolbox[{workspace, edits, shell}],
    budget: TurnLoop.Budget,
    strict: Boolean,
    answerWithin: FiniteDuration = TurnTools.AnswerWithin
)
