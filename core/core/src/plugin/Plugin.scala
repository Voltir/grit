package grit.core.plugin

import java.time.Instant

import grit.core.document.{DocumentKeeper, DocumentTerms}
import grit.core.id.PluginName
import grit.core.job.{Declared, Job}
import grit.core.period.CloseOrdinal
import grit.core.store.{ClosedPeriod, StoreError, Tx}
import grit.core.visibility.Compartment

/** A feature a deployment turns on (ADR 0027): a name, a version, the plugins it reads, and
  * the contributions it makes to core's points, each none unless it says otherwise. Built
  * from closed periods alone: it never sees a raw entry.
  */
trait Plugin extends caps.Pure {
  def name: PluginName

  /** What its posting writes ([[cache]], [[documents]]) is of this version. Changing it posts
    * every kept closed period again and leaves its earlier cache documents unread; its
    * documents are kept.
    */
  def version: Int

  /** The plugins its tools read, each through its service ([[Needs]]). A deployment is
    * refused unless a plugin of each one's name is among its plugins.
    */
  def needs: Vector[Exports[?]] = Vector.empty

  /** What it keeps of each closed period as cache documents (ADR 0011). */
  def cache: Option[CachePosting] = None

  /** Its documents (ADR 0028). */
  def documents: Option[Documents] = None

  /** The tools every turn is offered that it runs itself over grit's store. A deployment is
    * refused when two of every plugin's tools and grit's own share a name.
    */
  def tools: Vector[PluginTool[?]] = Vector.empty

  /** Its jobs (ADR 0029), which its tools book ([[PluginTool.bind]]) and its schedules run. A
    * deployment is refused when two of every plugin's jobs and its own share a name.
    */
  def jobs: Vector[Job[?]] = Vector.empty

  /** The schedules it declares (ADR 0029), reconciled with the stored ones at every start. A
    * deployment is refused when one is of a job not among [[jobs]], or two share a key.
    */
  def schedules: Vector[Declared[?]] = Vector.empty

  /** The compartments it names, in a label it keeps documents at or labels anything with. A
    * deployment is refused unless its visibility declares each.
    */
  def compartments: Vector[Compartment] = Vector.empty

  /** Whether it is posted closed periods: it has a cache or documents. */
  final def posts: Boolean = cache.nonEmpty || documents.nonEmpty
}

/** A plugin's posting to cache documents. */
trait CachePosting extends caps.Pure {

  /** Keeps what the plugin makes of `closed` in `docs`, in the transaction that moves its
    * cursor past `closed`: both commit, or neither. A `Left` leaves the cursor before
    * `closed`, which is posted again on the next sweep.
    */
  def post(closed: ClosedPeriod, docs: CacheDocs)(using Tx^): Either[StoreError, Unit]
}

/** A plugin's documents: the terms they are kept and drawn on under, and its posting to them. */
trait Documents extends caps.Pure {
  def terms: DocumentTerms

  /** Keeps what the plugin makes of `closed` in `keeper`, in the transaction that moves its
    * cursor past `closed`, after its cache's post when it has one: all commit, or none. A
    * `Left` leaves the cursor before `closed`.
    */
  def post(closed: ClosedPeriod, keeper: DocumentKeeper)(using Tx^): Either[StoreError, Unit]
}

/** Where a plugin keeps what it makes of one closed period: every document it puts here is
  * that period's, deleted when its closing is.
  */
trait CacheDocs {

  /** Keeps `doc` under `key`, replacing any there, as the period's; a `DatabaseError` when the
    * plugin's cursor was never started.
    */
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
