package grit.turn

import scala.concurrent.duration.FiniteDuration

import grit.core.classify.Classifier
import grit.core.clock.{Clock, Fresh}
import grit.core.context.ContextAssembler
import grit.core.host.{Edits, Shell, Workspace}
import grit.core.model.FactBook
import grit.core.provider.{Models, TokenEstimator}
import grit.core.store.{Db, EntryStore, Jot, ModelProfileStore, UsageLedger}
import grit.core.tool.Toolbox

/** What a turn works with besides its `Durable` and its [[TurnTooling]]: its system prompt
  * and [[TurnRecords]], then the capabilities it calls. `assembler` builds its window,
  * `classifier` places its message among the topics, `models` holds the catalog the turn
  * pins and makes each role's calls under its pin, `db` reads the store outside a transaction, `clock` dates entries and paces
  * the reply's stream, and `fresh` tags each attempt at a model call ([[TurnStream]]). None
  * of them writes the store, so a step body that captures this can only read it.
  */
final case class TurnEnv(
    system: String,
    records: TurnRecords,
    assembler: ContextAssembler^,
    classifier: Classifier^,
    models: Models^,
    db: Db^,
    clock: Clock^,
    fresh: Fresh^
)

/** Where a turn's entries, their costs and its model profile are written, and how a request
  * is priced.
  */
final case class TurnRecords(
    entries: EntryStore,
    ledger: UsageLedger,
    estimator: TokenEstimator,
    profiles: ModelProfileStore
)

/** The tools a turn's model may call in its loop ([[TurnLoop]]), and how the loop runs:
  * each case's `jot` keeps each call's result from inside its step ([[TurnTools.Settling]]);
  * `budget` bounds its model calls, the last made with tools off; a call a person approves
  * first waits `answerWithin` for their answer. What the tools may act through is the case.
  */
sealed trait TurnTooling {
  def budget: TurnLoop.Budget
  def answerWithin: FiniteDuration
}

object TurnTooling {

  /** Tools that act only through `workspace`: the turn cannot change the checkout. */
  final case class ReadOnly(
      workspace: Workspace^,
      tools: Toolbox[{workspace}],
      jot: Jot^,
      budget: TurnLoop.Budget,
      answerWithin: FiniteDuration = TurnTools.AnswerWithin
  ) extends TurnTooling

  /** Tools that act through `workspace`, `edits`, `shell`, `facts` and `models` (to probe a
    * pair); they may be fewer than all five allow.
    */
  final case class Full(
      workspace: Workspace^,
      edits: Edits^,
      shell: Shell^,
      facts: FactBook^,
      models: Models^,
      tools: Toolbox[{workspace, edits, shell, facts, models}],
      jot: Jot^,
      budget: TurnLoop.Budget,
      answerWithin: FiniteDuration = TurnTools.AnswerWithin
  ) extends TurnTooling
}
