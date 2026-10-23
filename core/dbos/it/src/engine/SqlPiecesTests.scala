package grit.dbos.engine

import grit.core.files.{PieceContract, PieceKeeper, PieceStore}
import grit.core.id.PluginName
import grit.core.store.{Tombstones, Tx}
import grit.core.visibility.Clearance
import grit.dbos.sql.{LiveDb, SqlPieces, SqlTombstones, TestPostgres}

/** The pieces contract, kept by the SQL store against a real Postgres. */
object SqlPiecesTests extends PieceContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_pieces")
    LiveEngine.open(c, "test").close()
    c
  }

  protected val tombstones: Tombstones = new SqlTombstones

  private val pieces = new SqlPieces(tombstones)

  protected val store: PieceStore = pieces

  protected def keeper(plugin: PluginName, bound: Long): PieceKeeper = pieces.keeper(plugin, bound)

  protected def opened[A](clearance: Clearance)(body: (Tx^) ?=> A): A =
    LiveDb.transaction(config, clearance)(body)
}
