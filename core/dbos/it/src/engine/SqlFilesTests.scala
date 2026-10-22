package grit.dbos.engine

import grit.core.files.{FileCatalog, FilesContract}
import grit.core.store.{Jot, Tombstones, Tx}
import grit.core.visibility.TestLabels
import grit.dbos.sql.{
  DbConfig,
  LiveDb,
  Opener,
  SqlFileCatalog,
  SqlJot,
  SqlStatements,
  SqlTombstones,
  TestPostgres
}

import org.postgresql.ds.PGSimpleDataSource

/** The file area's contract, kept over the SQL catalog against a real Postgres. */
object SqlFilesTests extends FilesContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_files")
    LiveEngine.open(c, "test").close()
    c
  }

  protected val catalog: FileCatalog = new SqlFileCatalog
  protected def jot: Jot = SqlFilesTests.jot(config)
  protected def reading[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)
  protected val tombstones: Tombstones = new SqlTombstones
  protected def emptied(): Unit = {
    val _ = LiveDb.transaction(config)(SqlStatements.update("DELETE FROM grit.files")(_ => ()))
    val _ = LiveDb.transaction(config)(
      SqlStatements.update("DELETE FROM grit.tombstones WHERE kind = 'file'")(_ => ())
    )
  }

  /** Short transactions on `config`'s database, as the engine's [[Jot]] makes them. */
  def jot(config: DbConfig): Jot = {
    val ds = new PGSimpleDataSource()
    ds.setURL(config.jdbcUrl)
    ds.setUser(config.user)
    ds.setPassword(config.password)
    new SqlJot(ds, new Opener(TestLabels.Trialled))
  }
}
