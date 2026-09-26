package grit.dbos.engine

import grit.core.id.PluginName
import grit.core.period.CloseOrdinal
import grit.core.plugin.{CacheDocs, PluginContract, PluginCursors, PluginDocs}
import grit.core.store.{Tombstones, Tx}
import grit.dbos.sql.{
  LiveDb,
  SqlCacheDocs,
  SqlPluginCursors,
  SqlPluginDocs,
  SqlTombstones,
  TestPostgres
}

/** The plugin contract, kept by the SQL stores against a real Postgres. */
object SqlPluginTests extends PluginContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_plugin")
    Engine.open(c, "test").close()
    c
  }

  protected def docs(plugin: PluginName): PluginDocs = new SqlPluginDocs(plugin)

  protected def cache(plugin: PluginName, source: CloseOrdinal): CacheDocs =
    new SqlCacheDocs(plugin, source)

  protected val tombstones: Tombstones = new SqlTombstones

  protected val cursors: PluginCursors = new SqlPluginCursors(tombstones)

  protected def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)
}
