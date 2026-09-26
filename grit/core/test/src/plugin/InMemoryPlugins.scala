package grit.core.plugin

import java.time.Instant

import grit.core.id.PluginName
import grit.core.period.CloseOrdinal
import grit.core.retention.Target
import grit.core.store.{ClosedPeriod, InMemoryTombstones, StoreError, Tombstones, Tx}

/** In-memory [[PluginDocs]], [[CacheDocs]] and [[PluginCursors]] for tests, keeping
  * [[PluginContract]], marking restarts in `tombstones`. They ignore the `Tx`: nothing is
  * rolled back.
  */
final class InMemoryPlugins(tombstones: Tombstones = new InMemoryTombstones) {
  import InMemoryPlugins.{Cursor, Doc}

  // Only ever replaced by new immutable maps, as the store's tables would be.
  @caps.unsafe.untrackedCaptures
  private var stored = Map.empty[(PluginName, Long, String), Doc]

  // As `stored`: only ever replaced by a new immutable map.
  @caps.unsafe.untrackedCaptures
  private var positions = Map.empty[PluginName, Cursor]

  private def generation(plugin: PluginName): Option[Long] = positions.get(plugin).map(_.generation)

  /** `plugin`'s documents as posted from the closed period `source`. */
  def cache(plugin: PluginName, source: CloseOrdinal): CacheDocs = new CacheDocs {
    def put(key: String, doc: ujson.Value)(using Tx^): Either[StoreError, Unit] =
      generation(plugin) match {
        // As the SQL store's insert fails: its generation, read from the cursor, is null.
        case None => Left(StoreError.DatabaseError(s"no cursor for ${PluginName.value(plugin)}"))
        case Some(g) =>
          stored = stored.updated((plugin, g, key), Doc(doc, source))
          Right(())
      }
  }

  /** How many documents `plugin` has in every generation, current or not. */
  def rows(plugin: PluginName): Int = stored.keys.count(_._1 == plugin)

  /** Given a plugin and the closed period it is posting, where it keeps what it makes of it. */
  val posting: (PluginName, ClosedPeriod) -> CacheDocs = (p, closed) => cache(p, closed.order)

  def docs(plugin: PluginName): PluginDocs = new PluginDocs {
    private def current: Vector[(String, ujson.Value)] =
      stored.toVector.collect {
        case ((p, g, k), d) if p == plugin && generation(plugin).contains(g) => k -> d.doc
      }
    def get(key: String)(using Tx^): Either[StoreError, Option[ujson.Value]] =
      Right(current.find(_._1 == key).map(_._2))
    def newest(prefix: String, n: Int)(using
        Tx^
    ): Either[StoreError, Vector[(String, ujson.Value)]] =
      Right(current.filter(_._1.startsWith(prefix)).sortBy(_._1).reverse.take(n max 0))
  }

  val cursors: PluginCursors = new PluginCursors {
    def start(plugin: PluginName, version: Int, at: Instant)(using
        Tx^
    ): Either[StoreError, CloseOrdinal] =
      positions.get(plugin) match {
        case Some(c) if c.version == version => Right(c.at)
        case Some(c) =>
          positions =
            positions.updated(plugin, Cursor(version, CloseOrdinal.Start, c.generation + 1))
          tombstones.write(Target.Restarted(plugin), at).map(_ => CloseOrdinal.Start)
        case None =>
          positions = positions.updated(plugin, Cursor(version, CloseOrdinal.Start, 1))
          Right(CloseOrdinal.Start)
      }
    def advance(plugin: PluginName, version: Int, to: CloseOrdinal)(using
        Tx^
    ): Either[StoreError, Unit] = {
      val g = generation(plugin).getOrElse(1L)
      positions = positions.updated(plugin, Cursor(version, to, g))
      Right(())
    }
    def stored()(using Tx^): Either[StoreError, Vector[(PluginName, Int)]] =
      Right(positions.toVector.map((p, c) => (p, c.version)).sortBy((p, _) => PluginName.value(p)))
    def forgetPosted(order: CloseOrdinal)(using Tx^): Either[StoreError, Unit] = {
      InMemoryPlugins.this.stored = InMemoryPlugins.this.stored.filterNot(_._2.source == order)
      Right(())
    }
    def retire(plugin: PluginName)(using Tx^): Either[StoreError, Unit] = {
      generation(plugin).foreach { g =>
        InMemoryPlugins.this.stored =
          InMemoryPlugins.this.stored.filterNot((k, _) => k._1 == plugin && k._2 < g)
      }
      Right(())
    }
    def remove(plugin: PluginName)(using Tx^): Either[StoreError, Unit] = {
      InMemoryPlugins.this.stored = InMemoryPlugins.this.stored.filterNot((k, _) => k._1 == plugin)
      positions = positions - plugin
      Right(())
    }
  }
}

object InMemoryPlugins {
  private final case class Doc(doc: ujson.Value, source: CloseOrdinal)
  private final case class Cursor(version: Int, at: CloseOrdinal, generation: Long)
}
