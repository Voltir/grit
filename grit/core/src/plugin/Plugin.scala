package grit.core.plugin

import grit.core.id.PluginName
import grit.core.store.{ClosedPeriod, StoreError, Tx}

/** A feature a deployment turns on, built from closed periods alone: it never sees a raw
  * entry.
  */
trait Plugin {
  def name: PluginName

  /** What `post` writes is of this version. Changing it clears the plugin's documents and
    * posts every closed period again.
    */
  def version: Int

  /** Keeps what the plugin wants of `closed` in `docs`, in the transaction that moves its
    * cursor past `closed`: both commit, or neither. A `Left` leaves the cursor before
    * `closed`, which is posted again on the next sweep.
    */
  def post(closed: ClosedPeriod, docs: PluginDocs)(using Tx^): Either[StoreError, Unit]
}

/** One plugin's documents, under keys it chooses. No other plugin's are reachable. */
trait PluginDocs {

  /** Keeps `doc` under `key`, replacing any there. */
  def put(key: String, doc: ujson.Value)(using Tx^): Either[StoreError, Unit]

  /** The document under `key`, or `None`. */
  def get(key: String)(using Tx^): Either[StoreError, Option[ujson.Value]]

  /** Its documents whose keys start with `prefix`, greatest key first, at most `n`. */
  def newest(prefix: String, n: Int)(using Tx^): Either[StoreError, Vector[(String, ujson.Value)]]
}

/** How far each plugin has posted, in close order. */
trait PluginCursors {

  /** `plugin`'s cursor at `version`: the close ordinal it has posted through. When none is
    * stored, or the one stored is of another version, the plugin's documents are deleted and
    * its cursor starts again at [[grit.core.period.CloseOrdinal.Start]]: a rebuild from the
    * closing entries.
    */
  def start(plugin: PluginName, version: Int)(using
      Tx^
  ): Either[StoreError, grit.core.period.CloseOrdinal]

  /** Moves `plugin`'s cursor at `version` to `to`. */
  def advance(plugin: PluginName, version: Int, to: grit.core.period.CloseOrdinal)(using
      Tx^
  ): Either[StoreError, Unit]
}
