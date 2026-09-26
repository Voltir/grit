package grit.core.store

import java.time.Instant

import grit.core.model.Profile

/** Facts about pairs learned while grit runs, each approved by a person before it is kept:
  * the database's layer over the checked-in seed. Append-only; read in the order kept.
  */
trait ModelFactStore {

  /** Keeps `facts`, the settings it knows for its pair, approved by `approvedBy` at `at`. */
  def keep(facts: Profile, approvedBy: String, at: Instant)(using Tx^): Either[StoreError, Unit]

  /** Every fact kept, oldest first. */
  def all()(using Tx^): Either[StoreError, Vector[ModelFactStore.Kept]]
}

object ModelFactStore {

  /** `facts` as kept: who approved them, and when. */
  final case class Kept(facts: Profile, approvedBy: String, at: Instant)
}
