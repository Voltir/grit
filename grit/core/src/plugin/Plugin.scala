package grit.core.plugin

import java.time.Instant

import grit.core.id.PluginName
import grit.core.period.CloseOrdinal
import grit.core.store.{ClosedPeriod, StoreError, Tx}

/** A feature a deployment turns on, built from closed periods alone: it never sees a raw
  * entry.
  */
trait Plugin {
  def name: PluginName

  /** What `post` writes is of this version. Changing it leaves the plugin's documents unread,
    * and posts every closed period again.
    */
  def version: Int

  /** Keeps what the plugin makes of `closed` in `docs`, in the transaction that moves its
    * cursor past `closed`: both commit, or neither. A `Left` leaves the cursor before
    * `closed`, which is posted again on the next sweep.
    */
  def post(closed: ClosedPeriod, docs: CacheDocs)(using Tx^): Either[StoreError, Unit]
}

/** Where a plugin keeps what it makes of one closed period: every document it puts here is
  * that period's, deleted when its closing is.
  */
trait CacheDocs {

  /** Keeps `doc` under `key`, replacing any there, as the period's. */
  def put(key: String, doc: ujson.Value)(using Tx^): Either[StoreError, Unit]
}

/** One plugin's documents as its surfaces read them. No other plugin's are reachable. */
trait PluginDocs {

  /** The document under `key`, or `None`. */
  def get(key: String)(using Tx^): Either[StoreError, Option[ujson.Value]]

  /** Its documents whose keys start with `prefix`, greatest key first, at most `n`. */
  def newest(prefix: String, n: Int)(using Tx^): Either[StoreError, Vector[(String, ujson.Value)]]
}

/** How far each plugin has posted, in close order, and what is deleted of its documents. */
trait PluginCursors {

  /** `plugin`'s cursor at `version`: the close ordinal it has posted through. When none is
    * stored, or the one stored is of another version, it starts again at
    * [[grit.core.period.CloseOrdinal.Start]], and the documents it wrote before are no longer
    * read, marked for deletion at `at` ([[grit.core.retention.Target.Restarted]]).
    */
  def start(plugin: PluginName, version: Int, at: Instant)(using
      Tx^
  ): Either[StoreError, CloseOrdinal]

  /** Moves `plugin`'s cursor at `version` to `to`. */
  def advance(plugin: PluginName, version: Int, to: CloseOrdinal)(using
      Tx^
  ): Either[StoreError, Unit]

  /** Every plugin with a stored cursor, and the version it is at. */
  def stored()(using Tx^): Either[StoreError, Vector[(PluginName, Int)]]

  /** Deletes every plugin's documents posted from the closed period `order`. */
  def forgetPosted(order: CloseOrdinal)(using Tx^): Either[StoreError, Unit]

  /** Deletes `plugin`'s documents from before its cursor last started again. */
  def retire(plugin: PluginName)(using Tx^): Either[StoreError, Unit]

  /** Deletes `plugin`'s documents and cursor: its next start is at
    * [[grit.core.period.CloseOrdinal.Start]].
    */
  def remove(plugin: PluginName)(using Tx^): Either[StoreError, Unit]
}
