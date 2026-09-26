package grit.core.plugin

import grit.core.store.Tx
import grit.dbos.sql.TestTx

/** The plugin contract, kept by the in-memory fakes. */
object InMemoryPluginTests extends PluginContract {

  private val plugins = new InMemoryPlugins

  protected def docs(plugin: PluginName): PluginDocs = plugins.docs(plugin)

  protected val cursors: PluginCursors = plugins.cursors

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)
}
