package grit.dbos.engine

import java.io.{ByteArrayInputStream, InputStream}
import java.time.Instant
import java.util.concurrent.{BrokenBarrierException, CyclicBarrier, TimeUnit, TimeoutException}

import scala.concurrent.duration.*

import grit.core.clock.SetClock
import grit.core.files.{
  FileArea,
  FileCatalog,
  FileError,
  FileItem,
  FileOrigin,
  FilesContract,
  Incoming,
  PutKey
}
import grit.core.host.TempBlobs
import grit.core.id.EdgeName
import grit.core.place.Place
import grit.core.store.{Jot, StoreError, Tx}
import grit.core.visibility.{Integrity, Provenance, Readers, Subject, TestLabels}
import grit.dbos.sql.{DbConfig, LiveDb, Opener, SqlFileCatalog, SqlJot, SqlStatements, TestPostgres}

import org.postgresql.ds.PGSimpleDataSource
import utest.*

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
  protected def emptied(): Unit = {
    val _ = LiveDb.transaction(config)(SqlStatements.update("DELETE FROM grit.files")(_ => ()))
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
