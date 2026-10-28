package grit.core.store

import java.time.Instant

import grit.core.id.PluginName
import grit.core.retention.{Target, Tombstone}

/** What grit has decided to delete, kept until the collector has deleted it: nothing in grit
  * deletes a record (a row with a history, or a workflow history) that no tombstone names. Live
  * state with no history (a lock such as a claim, a cursor, a counter) is removed by the write
  * that ends it instead.
  */
trait Tombstones {

  /** Marks `target` for deletion as of `at`. A pending tombstone on it keeps its time; one
    * collected or spared is pending again from `at`. Whether it armed one: `false` when one
    * was pending already.
    */
  def write(target: Target, at: Instant)(using Tx^): Either[StoreError, Boolean]

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

  /** Pending tombstones of `kind` written before `before` on `plugin`'s rows, at most `n`, in
    * [[due]]'s order: of a kind whose retention its plugin declares
    * ([[grit.core.retention.Retention.Declared]]), a [[Target.Document]]'s or a
    * [[Target.PieceSet]]'s; none of any other kind. Which plugin's a target is, its row says: one
    * whose row is gone is never among them (its plugin's Disabled collection ends it).
    */
  def declaredDue(kind: Target.Kind, plugin: PluginName, before: Instant, n: Int)(using
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
