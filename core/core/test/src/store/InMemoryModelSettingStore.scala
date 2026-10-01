package grit.core.store

import java.time.Instant

import grit.core.model.Profile

/** An in-memory [[ModelSettingStore]] for tests, keeping [[StoreContract]]. It ignores the `Tx`. */
final class InMemoryModelSettingStore extends ModelSettingStore {

  @caps.unsafe.untrackedCaptures
  var kept = Vector.empty[ModelSettingStore.Kept]

  def keep(settings: Profile, approvedBy: String, at: Instant)(using
      Tx^
  ): Either[StoreError, Unit] = {
    kept = kept :+ ModelSettingStore.Kept(settings, approvedBy, at)
    Right(())
  }

  def all()(using Tx^): Either[StoreError, Vector[ModelSettingStore.Kept]] = Right(kept)
}
