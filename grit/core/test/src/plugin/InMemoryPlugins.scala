package grit.core.plugin

import grit.core.period.CloseOrdinal
import grit.core.store.{StoreError, Tx}

/** In-memory [[PluginDocs]] and [[PluginCursors]] for tests, keeping [[PluginContract]]. They
  * ignore the `Tx`: nothing is rolled back.
  */
final class InMemoryPlugins {

  // Only ever replaced by new immutable maps, as the store's tables would be.
  @caps.unsafe.untrackedCaptures
  private var stored = Map.empty[(PluginName, String), ujson.Value]

  @caps.unsafe.untrackedCaptures
  private var positions = Map.empty[PluginName, (Int, CloseOrdinal)]

  def docs(plugin: PluginName): PluginDocs = new PluginDocs {
    def put(key: String, doc: ujson.Value)(using Tx^): Either[StoreError, Unit] = {
      stored = stored.updated((plugin, key), doc)
      Right(())
    }
    def get(key: String)(using Tx^): Either[StoreError, Option[ujson.Value]] =
      Right(stored.get((plugin, key)))
    def newest(prefix: String, n: Int)(using
        Tx^
    ): Either[StoreError, Vector[(String, ujson.Value)]] =
      Right(
        stored.toVector
          .collect { case ((p, k), v) if p == plugin && k.startsWith(prefix) => k -> v }
          .sortBy(_._1)
          .reverse
          .take(n max 0)
      )
  }

  val cursors: PluginCursors = new PluginCursors {
    def start(plugin: PluginName, version: Int)(using Tx^): Either[StoreError, CloseOrdinal] =
      positions.get(plugin) match {
        case Some((v, at)) if v == version => Right(at)
        case _ =>
          stored = stored.filterNot(_._1._1 == plugin)
          positions = positions.updated(plugin, (version, CloseOrdinal.Start))
          Right(CloseOrdinal.Start)
      }
    def advance(plugin: PluginName, version: Int, to: CloseOrdinal)(using
        Tx^
    ): Either[StoreError, Unit] = {
      positions = positions.updated(plugin, (version, to))
      Right(())
    }
  }
}
