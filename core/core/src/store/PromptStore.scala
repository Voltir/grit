package grit.core.store

import grit.core.id.WorkflowId
import grit.core.prompt.{Fragment, FragmentId, SystemPrompt}

/** The system prompt each turn was sent. A fragment is kept once, under its id, however many
  * turns share it, and never deleted; a turn's prompt is recorded once and never changed.
  */
trait PromptStore {

  /** Keeps each of `fragments` that is new. */
  def keep(fragments: Vector[Fragment])(using Tx^): Either[StoreError, Unit]

  /** Records that `workflow` is sent `prompt`, keeping each fragment that is new. A workflow
    * already recorded keeps what it had.
    */
  def record(workflow: WorkflowId, prompt: SystemPrompt)(using Tx^): Either[StoreError, Unit]

  /** The prompt `workflow` was sent; `None` when none was recorded for it. */
  def of(workflow: WorkflowId)(using Tx^): Either[StoreError, Option[SystemPrompt]]

  /** The prompt of the fragments kept under `ids`. `StoreError.Invalid` naming the first id
    * with no fragment kept.
    */
  def prompt(ids: Vector[FragmentId])(using Tx^): Either[StoreError, SystemPrompt]

  /** Forgets which prompt each of `workflows` was sent; the fragments are kept. */
  def forget(workflows: Vector[WorkflowId])(using Tx^): Either[StoreError, Unit]
}
