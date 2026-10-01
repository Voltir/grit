package grit.core.store

import grit.core.id.WorkflowId
import grit.core.model.{TurnProfile, TurnProfileId}

/** Which [[TurnProfile]] each turn ran under. A profile is kept once, under its content id,
  * however many turns share it; a turn's is recorded once and never changed.
  */
trait ModelProfileStore {

  /** Records that `workflow` runs under `profile`, keeping `profile` if it is new. A
    * workflow already recorded keeps what it had: pinning it again changes nothing.
    */
  def pin(workflow: WorkflowId, profile: TurnProfile)(using Tx^): Either[StoreError, Unit]

  /** The profile `workflow` runs under, or `None` when it was never pinned. */
  def of(workflow: WorkflowId)(using Tx^): Either[StoreError, Option[TurnProfile]]

  /** The profile kept under `id`, or `None` when there is none. */
  def get(id: TurnProfileId)(using Tx^): Either[StoreError, Option[TurnProfile]]

  /** Forgets which profile each of `workflows` ran under; the profiles themselves are kept. */
  def forget(workflows: Vector[WorkflowId])(using Tx^): Either[StoreError, Unit]
}
