package grit.core.store

import java.time.Instant

import grit.core.id.PluginName
import grit.core.retention.{Target, Tombstone}

/** What grit has decided to delete, kept until the collector has deleted it: nothing in grit
  * deletes a row or a workflow history that no tombstone names.
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

  /** Pending [[Target.Document]] tombstones written before `before` on `plugin`'s versions, at
    * most `n`, in [[due]]'s order. Which plugin's a version is, its document row says: one whose
    * row is gone is never among them (its plugin's Disabled collection ends it).
    */
  def documentsDue(plugin: PluginName, before: Instant, n: Int)(using
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
