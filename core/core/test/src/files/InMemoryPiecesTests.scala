package grit.core.files

import grit.core.id.PluginName
import grit.core.store.{Tombstones, Tx}
import grit.core.visibility.Clearance
import grit.dbos.sql.TestTx

/** The pieces contract, kept by the in-memory fake. */
object InMemoryPiecesTests extends PieceContract {

  private val pieces = new InMemoryPieces

  protected val store: PieceStore = pieces

  protected def keeper(plugin: PluginName, bound: Long): PieceKeeper = pieces.keeper(plugin, bound)

  protected val tombstones: Tombstones = pieces.tombstones

  protected def opened[A](clearance: Clearance)(body: (Tx^) ?=> A): A =
    body(using TestTx.fake(clearance))
}
