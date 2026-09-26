package grit.core.store

import java.time.Instant

import grit.core.retention.{Target, Tombstone}

/** What grit has decided to delete, kept until the collector has deleted it: nothing in grit
  * deletes a row or a workflow history that no tombstone names.
  */
trait Tombstones {

  /** Marks `target` for deletion as of `at`. A pending tombstone on it keeps its time; one
    * collected or spared is pending again from `at`.
    */
  def write(target: Target, at: Instant)(using Tx^): Either[StoreError, Unit]

  /** Spares `target`'s pending tombstone at `at`, if it has one; a later [[write]] arms it
    * again.
    */
  def spare(target: Target, at: Instant)(using Tx^): Either[StoreError, Unit]

  /** Pending tombstones of `kind` written before `before`, at most `n`: those never deferred
    * oldest written first, each deferred one placed at the time it was last deferred.
    */
  def due(kind: Target.Kind, before: Instant, n: Int)(using
      Tx^
  ): Either[StoreError, Vector[Tombstone]]

  /** Records that `target`'s pending tombstone could not be collected at `at`, so [[due]]
    * places it behind the ones due since.
    */
  def deferred(target: Target, at: Instant)(using Tx^): Either[StoreError, Unit]

  /** Records `target`'s pending tombstone collected at `at`; nothing when none is pending. */
  def collected(target: Target, at: Instant)(using Tx^): Either[StoreError, Unit]

  /** Deletes the tombstones collected or spared before `before`; how many. The one deletion no
    * tombstone names.
    */
  def forget(before: Instant)(using Tx^): Either[StoreError, Int]
}
