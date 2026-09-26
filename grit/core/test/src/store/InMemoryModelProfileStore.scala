package grit.core.store

import grit.core.id.WorkflowId
import grit.core.model.{TurnProfile, TurnProfileId}

/** An in-memory [[ModelProfileStore]] for tests, keeping [[StoreContract]]. It ignores the
  * `Tx`.
  */
final class InMemoryModelProfileStore extends ModelProfileStore {

  @caps.unsafe.untrackedCaptures
  var profiles = Vector.empty[TurnProfile]

  @caps.unsafe.untrackedCaptures
  var turns = Vector.empty[(WorkflowId, TurnProfileId)]

  def pin(workflow: WorkflowId, profile: TurnProfile)(using Tx^): Either[StoreError, Unit] = {
    if (!profiles.exists(_.id == profile.id)) profiles = profiles :+ profile
    if (!turns.exists(_._1 == workflow)) turns = turns :+ (workflow, profile.id)
    Right(())
  }

  def of(workflow: WorkflowId)(using Tx^): Either[StoreError, Option[TurnProfile]] =
    Right(turns.find(_._1 == workflow).flatMap((_, id) => profiles.find(_.id == id)))

  def get(id: TurnProfileId)(using Tx^): Either[StoreError, Option[TurnProfile]] =
    Right(profiles.find(_.id == id))
}
