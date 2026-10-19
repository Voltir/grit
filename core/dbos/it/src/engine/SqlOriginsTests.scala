package grit.dbos.engine

import grit.core.edge.{Origins, OriginsContract}
import grit.core.id.ConversationId
import grit.core.store.{Origin, Tx}
import grit.core.visibility.{Clearance, Label}
import grit.dbos.sql.{LiveDb, SqlOrigins, TestPostgres}

/** The origins contract, kept by the SQL store against a real Postgres, on one database: each
  * test reads conversations of origins of its own.
  */
object SqlOriginsTests extends OriginsContract {

  private lazy val config = {
    val c =
      TestPostgres.freshDatabase(s"sql_origins_${java.util.UUID.randomUUID().toString.take(8)}")
    LiveEngine.open(c, "test").close()
    c
  }

  protected val origins: Origins = new SqlOrigins

  protected def conversation(origin: Origin, label: Label): ConversationId =
    LiveDb.conversation(config, origin, label).id

  protected def opened[A](clearance: Clearance)(body: (Tx^) ?=> A): A =
    LiveDb.transaction(config, clearance)(body)
}
