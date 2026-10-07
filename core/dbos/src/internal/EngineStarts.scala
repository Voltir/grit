package grit.dbos.internal

import scala.util.Using

import grit.core.store.{StoreError, Tx}
import grit.dbos.engine.Build
import grit.dbos.sql.SqlEntryStore

/** `grit.engine_starts`, read back: [[grit.dbos.engine.EngineLock]] writes it. */
private[dbos] object EngineStarts {

  /** Every engine start recorded, oldest first. */
  def all()(using tx: Tx^): Either[StoreError, Vector[Build.Started]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    SqlEntryStore.attempt {
      Using.resource(
        conn.prepareStatement(
          """SELECT started_at, machine, pid, epoch, commit, dirty FROM grit.engine_starts
            | ORDER BY started_at, id""".stripMargin
        )
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[Build.Started]
          while (rs.next()) {
            val build = Option(rs.getString("commit")) match {
              case Some(commit) => Build.Known(commit, rs.getBoolean("dirty"))
              case None => Build.Unknown
            }
            rows += Build.Started(
              rs.getTimestamp("started_at").toInstant,
              rs.getString("machine"),
              rs.getLong("pid"),
              rs.getString("epoch"),
              build
            )
          }
          rows.result()
        }
      }
    }
  }
}
