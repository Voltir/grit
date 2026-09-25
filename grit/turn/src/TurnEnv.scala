package grit.turn

import scala.concurrent.duration.FiniteDuration

import grit.core.classify.Classifier
import grit.core.clock.{Clock, Fresh}
import grit.core.context.ContextAssembler
import grit.core.host.{Edits, Shell, Workspace}
import grit.core.provider.{Provider, TokenEstimator}
import grit.core.store.{Db, EntryStore, Jot, UsageLedger}
import grit.core.tool.Toolbox

/** What a turn works with besides its `Durable` and its [[TurnTooling]]: its system prompt
  * and [[TurnRecords]], then the capabilities it calls. `assembler` builds its window,
  * `classifier` places its message among the topics, `provider` answers, `summarizer`
  * summarises, `db` reads the store outside a transaction, `clock` dates entries and paces
  * the reply's stream, and `fresh` tags each attempt at a model call ([[TurnStream]]). None
  * of them writes the store, so a step body that captures this can only read it.
  */
final case class TurnEnv(
    system: String,
    records: TurnRecords,
    assembler: ContextAssembler^,
    classifier: Classifier^,
    provider: Provider^,
    summarizer: Provider^,
    db: Db^,
    clock: Clock^,
    fresh: Fresh^
)

/** Where a turn's entries and their costs are written, and how a request is priced. */
final case class TurnRecords(entries: EntryStore, ledger: UsageLedger, estimator: TokenEstimator)

/** The tools a turn's model may call in its loop ([[TurnLoop]]), and how the loop runs:
  * each case's `jot` keeps each call's result from inside its step ([[TurnTools.Settling]]);
  * `budget` bounds its model calls, the last made with tools off; under `strict` each tool's
  * schema asks the provider to hold the model's arguments to it; a call a person approves
  * first waits `answerWithin` for their answer. What the tools may act through is the case.
  */
sealed trait TurnTooling {
  def budget: TurnLoop.Budget
  def strict: Boolean
  def answerWithin: FiniteDuration
}

object TurnTooling {

  /** Tools that act only through `workspace`: the turn cannot change the checkout. */
  final case class ReadOnly(
      workspace: Workspace^,
      tools: Toolbox[{workspace}],
      jot: Jot^,
      budget: TurnLoop.Budget,
      strict: Boolean,
      answerWithin: FiniteDuration = TurnTools.AnswerWithin
  ) extends TurnTooling

  /** Tools that act through `workspace`, `edits` and `shell`; they may be fewer than all
    * three allow.
    */
  final case class Full(
      workspace: Workspace^,
      edits: Edits^,
      shell: Shell^,
      tools: Toolbox[{workspace, edits, shell}],
      jot: Jot^,
      budget: TurnLoop.Budget,
      strict: Boolean,
      answerWithin: FiniteDuration = TurnTools.AnswerWithin
  ) extends TurnTooling
}
