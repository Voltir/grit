package grit.dbos.sql

import java.sql.PreparedStatement
import java.time.Instant

import scala.util.Using

import grit.core.id.PluginName
import grit.core.period.CloseOrdinal
import grit.core.plugin.{CacheDocs, PluginCursors, PluginDocs}
import grit.core.retention.Target
import grit.core.store.{StoreError, Tombstones, Tx}

/** [[PluginDocs]] over `plugin`'s rows of `grit.plugin_docs` of its cursor's generation, and
  * no other plugin's. Keys order bytewise (`COLLATE "C"`), as the in-memory fake orders them.
  */
final class SqlPluginDocs(plugin: PluginName) extends PluginDocs {
  import SqlPlugins.*

  def get(key: String)(using tx: Tx^): Either[StoreError, Option[ujson.Value]] =
    query(
      s"""SELECT key, doc FROM grit.plugin_docs
         | WHERE plugin = ? AND generation = $Generation AND key = ?""".stripMargin
    ) { ps =>
      ps.setString(1, PluginName.value(plugin))
      ps.setString(2, PluginName.value(plugin))
      ps.setString(3, key)
    }.map(_.headOption.map(_._2))

  def newest(prefix: String, n: Int)(using
      tx: Tx^
  ): Either[StoreError, Vector[(String, ujson.Value)]] =
    query(
      s"""SELECT key, doc FROM grit.plugin_docs
         | WHERE plugin = ? AND generation = $Generation AND starts_with(key, ?)
         | ORDER BY key COLLATE "C" DESC LIMIT ?""".stripMargin
    ) { ps =>
      ps.setString(1, PluginName.value(plugin))
      ps.setString(2, PluginName.value(plugin))
      ps.setString(3, prefix)
      ps.setInt(4, n max 0)
    }
}

/** [[CacheDocs]] over `plugin`'s rows of `grit.plugin_docs`, posted from the closed period
  * `source`, in its cursor's generation: a `DatabaseError` when it has no cursor.
  */
final class SqlCacheDocs(plugin: PluginName, source: CloseOrdinal) extends CacheDocs {
  import SqlPlugins.*

  def put(key: String, doc: ujson.Value)(using tx: Tx^): Either[StoreError, Unit] =
    update(
      s"""INSERT INTO grit.plugin_docs (plugin, generation, key, doc, source)
         |VALUES (?, $Generation, ?, ?::jsonb, ?)
         |ON CONFLICT (plugin, generation, key)
         |  DO UPDATE SET doc = EXCLUDED.doc, source = EXCLUDED.source""".stripMargin
    ) { ps =>
      ps.setString(1, PluginName.value(plugin))
      ps.setString(2, PluginName.value(plugin))
      ps.setString(3, key)
      ps.setString(4, doc.render())
      ps.setLong(5, CloseOrdinal.value(source))
    }.map(_ => ())
}

/** [[PluginCursors]] over `grit.plugin_cursors` and `grit.plugin_docs`, marking restarts in
  * `tombstones`.
  */
final class SqlPluginCursors(tombstones: Tombstones) extends PluginCursors {
  import SqlPlugins.*

  def start(plugin: PluginName, version: Int, at: Instant)(using
      tx: Tx^
  ): Either[StoreError, CloseOrdinal] = {
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
      case Some(_) =>
        for {
          _ <- update(
            """UPDATE grit.plugin_cursors SET version = ?, ordinal = ?, generation = generation + 1
              | WHERE plugin = ?""".stripMargin
          ) { ps =>
            ps.setInt(1, version)
            ps.setLong(2, CloseOrdinal.value(CloseOrdinal.Start))
            ps.setString(3, PluginName.value(plugin))
          }
          _ <- tombstones.write(Target.Restarted(plugin), at)
        } yield CloseOrdinal.Start
      case None =>
        update(
          "INSERT INTO grit.plugin_cursors (plugin, version, ordinal, generation) VALUES (?, ?, ?, 1)"
        ) { ps =>
          ps.setString(1, PluginName.value(plugin))
          ps.setInt(2, version)
          ps.setLong(3, CloseOrdinal.value(CloseOrdinal.Start))
        }.map(_ => CloseOrdinal.Start)
    }
  }

  def advance(plugin: PluginName, version: Int, to: CloseOrdinal)(using
      tx: Tx^
  ): Either[StoreError, Unit] =
    update(
      """INSERT INTO grit.plugin_cursors (plugin, version, ordinal, generation) VALUES (?, ?, ?, 1)
        |ON CONFLICT (plugin) DO UPDATE SET version = EXCLUDED.version, ordinal = EXCLUDED.ordinal""".stripMargin
    ) { ps =>
      ps.setString(1, PluginName.value(plugin))
      ps.setInt(2, version)
      ps.setLong(3, CloseOrdinal.value(to))
    }.map(_ => ())

  def stored()(using tx: Tx^): Either[StoreError, Vector[(PluginName, Int)]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    SqlEntryStore
      .attempt {
        Using.resource(
          conn.prepareStatement(
            "SELECT plugin, version FROM grit.plugin_cursors ORDER BY plugin COLLATE \"C\""
          )
        ) { ps =>
          Using.resource(ps.executeQuery()) { rs =>
            val rows = Vector.newBuilder[(String, Int)]
            while (rs.next()) rows += ((rs.getString("plugin"), rs.getInt("version")))
            rows.result()
          }
        }
      }
      .flatMap(rows =>
        rows.foldLeft[Either[StoreError, Vector[(PluginName, Int)]]](Right(Vector.empty)) {
          case (acc, (p, v)) =>
            acc.flatMap(done =>
              PluginName.of(p).map(n => done :+ (n, v)).left.map(StoreError.Invalid(_))
            )
        }
      )
  }

  def forgetPosted(order: CloseOrdinal)(using tx: Tx^): Either[StoreError, Unit] =
    update("DELETE FROM grit.plugin_docs WHERE source = ?")(
      _.setLong(1, CloseOrdinal.value(order))
    ).map(_ => ())

  def retire(plugin: PluginName)(using tx: Tx^): Either[StoreError, Unit] =
    update(s"DELETE FROM grit.plugin_docs WHERE plugin = ? AND generation < $Generation") { ps =>
      ps.setString(1, PluginName.value(plugin))
      ps.setString(2, PluginName.value(plugin))
    }.map(_ => ())

  def remove(plugin: PluginName)(using tx: Tx^): Either[StoreError, Unit] =
    for {
      _ <- update("DELETE FROM grit.plugin_docs WHERE plugin = ?")(
        _.setString(1, PluginName.value(plugin))
      )
      _ <- update("DELETE FROM grit.plugin_cursors WHERE plugin = ?")(
        _.setString(1, PluginName.value(plugin))
      )
    } yield ()
}

private object SqlPlugins {

  /** The generation of the cursor of the plugin bound at this point: none without one. */
  val Generation = "(SELECT generation FROM grit.plugin_cursors WHERE plugin = ?)"

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
