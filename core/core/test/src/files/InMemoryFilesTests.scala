package grit.core.files

import grit.core.store.{InMemoryTombstones, Jot, TestJot, Tombstones, Tx}
import grit.core.visibility.{Clearance, Visibility}
import grit.dbos.sql.TestTx

/** The file area's contract, kept over the in-memory catalog. */
object InMemoryFilesTests extends FilesContract {

  // Replaced whole by `emptied`, on the test's one thread.
  @caps.unsafe.untrackedCaptures
  private var kept = new InMemoryFileCatalog

  protected def catalog: FileCatalog = kept

  protected def jot: Jot = TestJot()

  protected def reading[A](body: (Tx^) ?=> A): A =
    body(using TestTx.fake(Clearance.of(Visibility.Shipped.compartments.top)))

  // Replaced whole by `emptied`, on the test's one thread.
  @caps.unsafe.untrackedCaptures
  private var marks = new InMemoryTombstones()

  protected def tombstones: Tombstones = marks

  protected def emptied(): Unit = {
    kept = new InMemoryFileCatalog
    marks = new InMemoryTombstones()
  }
}
