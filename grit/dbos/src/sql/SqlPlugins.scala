package grit.dbos.sql

import java.sql.PreparedStatement

import scala.util.Using

import grit.core.period.CloseOrdinal
import grit.core.plugin.{PluginCursors, PluginDocs, PluginName}
import grit.core.store.{StoreError, Tx}

/** [[PluginDocs]] over `plugin`'s rows of `grit.plugin_docs`, and no other plugin's. Keys
  * order bytewise (`COLLATE "C"`), as the in-memory fake orders them.
  */
final class SqlPluginDocs(plugin: PluginName) extends PluginDocs {
  import SqlPlugins.*

  def put(key: String, doc: ujson.Value)(using tx: Tx^): Either[StoreError, Unit] =
    update(
      """INSERT INTO grit.plugin_docs (plugin, key, doc) VALUES (?, ?, ?::jsonb)
        |ON CONFLICT (plugin, key) DO UPDATE SET doc = EXCLUDED.doc""".stripMargin
    ) { ps =>
      ps.setString(1, PluginName.value(plugin))
      ps.setString(2, key)
      ps.setString(3, doc.render())
    }.map(_ => ())

  def get(key: String)(using tx: Tx^): Either[StoreError, Option[ujson.Value]] =
    query("SELECT key, doc FROM grit.plugin_docs WHERE plugin = ? AND key = ?") { ps =>
      ps.setString(1, PluginName.value(plugin))
      ps.setString(2, key)
    }.map(_.headOption.map(_._2))

  def newest(prefix: String, n: Int)(using
      tx: Tx^
  ): Either[StoreError, Vector[(String, ujson.Value)]] =
    query(
      """SELECT key, doc FROM grit.plugin_docs
        | WHERE plugin = ? AND starts_with(key, ?)
        | ORDER BY key COLLATE "C" DESC LIMIT ?""".stripMargin
    ) { ps =>
      ps.setString(1, PluginName.value(plugin))
      ps.setString(2, prefix)
      ps.setInt(3, n max 0)
    }
}

/** [[PluginCursors]] over `grit.plugin_cursors`, clearing `grit.plugin_docs` on a new version. */
final class SqlPluginCursors extends PluginCursors {
  import SqlPlugins.*

  def start(plugin: PluginName, version: Int)(using tx: Tx^): Either[StoreError, CloseOrdinal] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    val stored = SqlEntryStore.attempt {
      Using.resource(
        conn.prepareStatement(
          "SELECT version, ordinal FROM grit.plugin_cursors WHERE plugin = ? FOR UPDATE"
        )
      ) { ps =>
        ps.setString(1, PluginName.value(plugin))
        Using.resource(ps.executeQuery()) { rs =>
          if (rs.next()) Some((rs.getInt("version"), rs.getLong("ordinal"))) else None
        }
      }
    }
    stored.flatMap {
      case Some((v, ordinal)) if v == version =>
        CloseOrdinal
          .of(ordinal)
          .toRight(StoreError.Invalid(s"plugin cursor ${PluginName.value(plugin)}: $ordinal"))
      case _ =>
        for {
          _ <- update("DELETE FROM grit.plugin_docs WHERE plugin = ?")(
            _.setString(1, PluginName.value(plugin))
          )
          _ <- set(plugin, version, CloseOrdinal.Start)
        } yield CloseOrdinal.Start
    }
  }

  def advance(plugin: PluginName, version: Int, to: CloseOrdinal)(using
      tx: Tx^
  ): Either[StoreError, Unit] =
    set(plugin, version, to)

  private def set(plugin: PluginName, version: Int, at: CloseOrdinal)(using
      tx: Tx^
  ): Either[StoreError, Unit] =
    update(
      """INSERT INTO grit.plugin_cursors (plugin, version, ordinal) VALUES (?, ?, ?)
        |ON CONFLICT (plugin) DO UPDATE SET version = EXCLUDED.version, ordinal = EXCLUDED.ordinal""".stripMargin
    ) { ps =>
      ps.setString(1, PluginName.value(plugin))
      ps.setInt(2, version)
      ps.setLong(3, CloseOrdinal.value(at))
    }.map(_ => ())
}

private object SqlPlugins {

  def update(
      sql: String
  )(bind: PreparedStatement => Unit)(using tx: Tx^): Either[StoreError, Int] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    SqlEntryStore.attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        bind(ps)
        ps.executeUpdate()
      }
    }
  }

  def query(sql: String)(bind: PreparedStatement => Unit)(using
      tx: Tx^
  ): Either[StoreError, Vector[(String, ujson.Value)]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    SqlEntryStore.attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        bind(ps)
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[(String, ujson.Value)]
          while (rs.next()) rows += (rs.getString("key") -> ujson.read(rs.getString("doc")))
          rows.result()
        }
      }
    }
  }
}
