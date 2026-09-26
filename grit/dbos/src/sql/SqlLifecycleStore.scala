package grit.dbos.sql

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.period.{LifecycleSettings, Probability, Windows}
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
            |       (extract(epoch FROM retention) * 1000)::bigint AS retention,
            |       balance,
            |       (extract(epoch FROM settle) * 1000)::bigint AS settle,
            |       finished_at, asks
            |  FROM grit.lifecycle_settings""".stripMargin
        )
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          if (!rs.next()) Right(LifecycleSettings.Default)
          else {
            val finishedAt = rs.getDouble("finished_at")
            (for {
              windows <- Windows.of(rs.getLong("idle").millis, rs.getLong("retention").millis)
              at <- Probability
                .of(finishedAt)
                .toRight(s"finished_at $finishedAt is not a probability")
              settings <- LifecycleSettings.of(
                windows,
                rs.getInt("balance"),
                rs.getLong("settle").millis,
                at,
                rs.getInt("asks")
              )
            } yield settings).left.map(why => StoreError.Invalid(s"lifecycle settings: $why"))
          }
        }
      }
    }.flatten
  }

  def seed(settings: LifecycleSettings)(using tx: Tx^): Either[StoreError, LifecycleSettings] =
    write(settings, "ON CONFLICT (one) DO NOTHING").flatMap(_ => current())

  def set(settings: LifecycleSettings)(using tx: Tx^): Either[StoreError, Unit] =
    write(
      settings,
      """ON CONFLICT (one) DO UPDATE SET idle = EXCLUDED.idle, retention = EXCLUDED.retention,
        |  balance = EXCLUDED.balance, settle = EXCLUDED.settle,
        |  finished_at = EXCLUDED.finished_at, asks = EXCLUDED.asks""".stripMargin
    )

  private def write(settings: LifecycleSettings, onConflict: String)(using
      tx: Tx^
  ): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    val w = settings.windows
    attempt {
      Using.resource(
        conn.prepareStatement(
          s"""INSERT INTO grit.lifecycle_settings (idle, retention, balance, settle, finished_at, asks)
             |VALUES (? * interval '1 millisecond', ? * interval '1 millisecond', ?,
             |        ? * interval '1 millisecond', ?, ?)
             |$onConflict""".stripMargin
        )
      ) { ps =>
        ps.setLong(1, w.idle.toMillis)
        ps.setLong(2, w.retention.toMillis)
        ps.setInt(3, settings.balance)
        ps.setLong(4, settings.settle.toMillis)
        ps.setDouble(5, Probability.value(settings.finishedAt))
        ps.setInt(6, settings.asks)
        ps.executeUpdate()
        ()
      }
    }
  }
}
