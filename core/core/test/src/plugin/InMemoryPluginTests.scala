package grit.core.plugin

import grit.core.id.PluginName
import grit.core.period.CloseOrdinal
import grit.core.store.{InMemoryTombstones, Tombstones, Tx}
import grit.dbos.sql.TestTx

/** The plugin contract, kept by the in-memory fakes. */
object InMemoryPluginTests extends PluginContract {

  protected val tombstones: Tombstones = new InMemoryTombstones

  private val plugins = new InMemoryPlugins(tombstones)

  protected def docs(plugin: PluginName): PluginDocs = plugins.docs(plugin)

  protected def cache(plugin: PluginName, source: CloseOrdinal): CacheDocs =
    plugins.cache(plugin, source)

  protected val cursors: PluginCursors = plugins.cursors

  protected def docRows(plugin: PluginName): Int = plugins.rows(plugin)

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)
}
