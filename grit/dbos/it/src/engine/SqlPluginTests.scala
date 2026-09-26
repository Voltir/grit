package grit.dbos.engine

import grit.core.plugin.{PluginContract, PluginCursors, PluginDocs, PluginName}
import grit.core.store.Tx
import grit.dbos.sql.{LiveDb, SqlPluginCursors, SqlPluginDocs, TestPostgres}

/** The plugin contract, kept by the SQL stores against a real Postgres. */
object SqlPluginTests extends PluginContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_plugin")
    Engine.open(c, "test").close()
    c
  }

  protected def docs(plugin: PluginName): PluginDocs = new SqlPluginDocs(plugin)

  protected val cursors: PluginCursors = new SqlPluginCursors

  protected def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)
}
