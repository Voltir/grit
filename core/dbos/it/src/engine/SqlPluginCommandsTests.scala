package grit.dbos.engine

import grit.core.edge.{PluginCommands, PluginCommandsContract}
import grit.dbos.sql.{LiveDb, SqlIdentities, TestPostgres}

/** The commands contract, kept by the SQL commands of an engine on a real Postgres, one engine
  * opened for each loan on a database whose reader is enrolled once.
  */
object SqlPluginCommandsTests extends PluginCommandsContract {

  import grit.core.edge.EdgeContracts.Reader

  import PluginCommandsContract.*

  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_plugin_commands")
    LiveEngine.open(c, "test").close()
    LiveDb
      .transaction(c)(SqlIdentities.enroll(Set(Reader)))
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)
    c
  }

  protected def commanding[A](body: PluginCommands^ => A): A = {
    val engine = LiveEngine.open(config, "test")
    try body(engine.commands(Vector(Commands)))
    finally engine.close()
  }
}
