package grit.dbos.sql

import java.sql.PreparedStatement
import java.time.{Instant, OffsetDateTime, ZoneOffset}

import scala.util.Using

import grit.core.retention.{Target, Tombstone}
import grit.core.store.{StoreError, Tombstones, Tx}

/** [[Tombstones]] over `grit.tombstones`. Keys order bytewise (`COLLATE "C"`), as the
  * in-memory fake orders them.
  */
final class SqlTombstones extends Tombstones {
  import SqlEntryStore.attempt

  def write(target: Target, at: Instant)(using tx: Tx^): Either[StoreError, Unit] =
    update(
      """INSERT INTO grit.tombstones (kind, target, written_at) VALUES (?, ?, ?)
        |ON CONFLICT (kind, target) DO UPDATE
        |   SET written_at = EXCLUDED.written_at, deferred_at = NULL, collected_at = NULL,
        |       spared = false
        | WHERE grit.tombstones.collected_at IS NOT NULL""".stripMargin
    ) { ps =>
      bind(ps, target)
      ps.setObject(3, at.atOffset(ZoneOffset.UTC))
    }.map(_ => ())

  def spare(target: Target, at: Instant)(using tx: Tx^): Either[StoreError, Unit] =
    end(target, at, spared = true)

  def collected(target: Target, at: Instant)(using tx: Tx^): Either[StoreError, Unit] =
    end(target, at, spared = false)

  def deferred(target: Target, at: Instant)(using tx: Tx^): Either[StoreError, Unit] =
    update(
      """UPDATE grit.tombstones SET deferred_at = ?
        | WHERE kind = ? AND target = ? AND collected_at IS NULL""".stripMargin
    ) { ps =>
      ps.setObject(1, at.atOffset(ZoneOffset.UTC))
      ps.setString(2, target.kind.name)
      ps.setString(3, Target.key(target))
    }.map(_ => ())

  def due(kind: Target.Kind, before: Instant, n: Int)(using
      tx: Tx^
  ): Either[StoreError, Vector[Tombstone]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """SELECT target, written_at FROM grit.tombstones
            | WHERE kind = ? AND collected_at IS NULL AND written_at < ?
            | ORDER BY coalesce(deferred_at, written_at), target COLLATE "C" LIMIT ?""".stripMargin
        )
      ) { ps =>
        ps.setString(1, kind.name)
        ps.setObject(2, before.atOffset(ZoneOffset.UTC))
        ps.setInt(3, n max 0)
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[Tombstone]
          while (rs.next()) {
            val target = Target.read(kind, rs.getString("target")) match {
              case Right(t) => t
              // grit's own bug, not a caller's expected failure: `attempt` reports it.
              case Left(why) => throw new IllegalStateException(why)
            }
            rows += Tombstone(
              target,
              rs.getObject("written_at", classOf[OffsetDateTime]).toInstant
            )
          }
          rows.result()
        }
      }
    }
  }

  def forget(before: Instant)(using tx: Tx^): Either[StoreError, Int] =
    update("DELETE FROM grit.tombstones WHERE collected_at < ?")(
      _.setObject(1, before.atOffset(ZoneOffset.UTC))
    )

  private def end(target: Target, at: Instant, spared: Boolean)(using
      tx: Tx^
  ): Either[StoreError, Unit] =
    update(
      """UPDATE grit.tombstones SET collected_at = ?, spared = ?
        | WHERE kind = ? AND target = ? AND collected_at IS NULL""".stripMargin
    ) { ps =>
      ps.setObject(1, at.atOffset(ZoneOffset.UTC))
      ps.setBoolean(2, spared)
      ps.setString(3, target.kind.name)
      ps.setString(4, Target.key(target))
    }.map(_ => ())

  private def bind(ps: PreparedStatement, target: Target): Unit = {
    ps.setString(1, target.kind.name)
    ps.setString(2, Target.key(target))
  }

  private def update(sql: String)(set: PreparedStatement => Unit)(using
      tx: Tx^
  ): Either[StoreError, Int] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        set(ps)
        ps.executeUpdate()
      }
    }
  }
}
