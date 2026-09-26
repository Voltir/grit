package grit.dbos.sql

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.period.{LifecycleSettings, Windows}
import grit.core.store.{LifecycleStore, StoreError, Tx}

/** [[LifecycleStore]] over the one row of `grit.lifecycle_settings`. The durations are
  * `interval`s, so they can be changed by hand (`SET idle = '3 minutes'`), and read back to
  * the millisecond.
  */
final class SqlLifecycleStore extends LifecycleStore {
  import SqlEntryStore.attempt

  def current()(using tx: Tx^): Either[StoreError, LifecycleSettings] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """SELECT (extract(epoch FROM idle) * 1000)::bigint AS idle,
            |       (extract(epoch FROM grace) * 1000)::bigint AS grace,
            |       (extract(epoch FROM retention) * 1000)::bigint AS retention,
            |       closings
            |  FROM grit.lifecycle_settings""".stripMargin
        )
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          if (!rs.next()) Right(LifecycleSettings.Default)
          else
            Windows
              .of(
                rs.getLong("idle").millis,
                rs.getLong("grace").millis,
                rs.getLong("retention").millis
              )
              .flatMap(LifecycleSettings.of(_, rs.getInt("closings")))
              .left
              .map(why => StoreError.Invalid(s"lifecycle settings: $why"))
        }
      }
    }.flatten
  }

  def seed(settings: LifecycleSettings)(using tx: Tx^): Either[StoreError, LifecycleSettings] =
    write(settings, "ON CONFLICT (one) DO NOTHING").flatMap(_ => current())

  def set(settings: LifecycleSettings)(using tx: Tx^): Either[StoreError, Unit] =
    write(
      settings,
      """ON CONFLICT (one) DO UPDATE SET idle = EXCLUDED.idle, grace = EXCLUDED.grace,
        |  retention = EXCLUDED.retention, closings = EXCLUDED.closings""".stripMargin
    )

  private def write(settings: LifecycleSettings, onConflict: String)(using
      tx: Tx^
  ): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    val w = settings.windows
    attempt {
      Using.resource(
        conn.prepareStatement(
          s"""INSERT INTO grit.lifecycle_settings (idle, grace, retention, closings)
             |VALUES (? * interval '1 millisecond', ? * interval '1 millisecond',
             |        ? * interval '1 millisecond', ?)
             |$onConflict""".stripMargin
        )
      ) { ps =>
        ps.setLong(1, w.idle.toMillis)
        ps.setLong(2, w.grace.toMillis)
        ps.setLong(3, w.retention.toMillis)
        ps.setInt(4, settings.closings)
        ps.executeUpdate()
        ()
      }
    }
  }
}
