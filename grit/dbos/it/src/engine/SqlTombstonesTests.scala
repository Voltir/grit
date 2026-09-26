package grit.dbos.engine

import grit.core.store.{Tombstones, TombstonesContract, Tx}
import grit.dbos.sql.{LiveDb, SqlTombstones, TestPostgres}

/** The tombstones contract, kept by the SQL store against a real Postgres. */
object SqlTombstonesTests extends TombstonesContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_tombstones")
    Engine.open(c, "test").close()
    c
  }

  protected val tombstones: Tombstones = new SqlTombstones
  protected def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)
}
