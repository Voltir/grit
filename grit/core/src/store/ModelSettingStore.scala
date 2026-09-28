package grit.core.store

import java.time.Instant

import grit.core.model.Profile

/** Settings of pairs learned while grit runs, each approved by a person before it is kept:
  * the database's layer over the checked-in seed. Append-only; read in the order kept.
  */
trait ModelSettingStore {

  /** Keeps `settings`, what it knows of its pair, approved by `approvedBy` at `at`. */
  def keep(settings: Profile, approvedBy: String, at: Instant)(using Tx^): Either[StoreError, Unit]

  /** Every profile kept, oldest first. */
  def all()(using Tx^): Either[StoreError, Vector[ModelSettingStore.Kept]]
}

object ModelSettingStore {

  /** `settings` as kept: who approved them, and when. */
  final case class Kept(settings: Profile, approvedBy: String, at: Instant)
}
