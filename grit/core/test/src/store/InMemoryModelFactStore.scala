package grit.core.store

import java.time.Instant

import grit.core.model.Profile

/** An in-memory [[ModelFactStore]] for tests, keeping [[StoreContract]]. It ignores the `Tx`. */
final class InMemoryModelFactStore extends ModelFactStore {

  @caps.unsafe.untrackedCaptures
  var kept = Vector.empty[ModelFactStore.Kept]

  def keep(facts: Profile, approvedBy: String, at: Instant)(using Tx^): Either[StoreError, Unit] = {
    kept = kept :+ ModelFactStore.Kept(facts, approvedBy, at)
    Right(())
  }

  def all()(using Tx^): Either[StoreError, Vector[ModelFactStore.Kept]] = Right(kept)
}
