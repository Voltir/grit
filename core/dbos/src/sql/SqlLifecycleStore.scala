package grit.dbos.sql

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.period.{LifecycleSettings, Probability, Windows}
import grit.core.place.Weight
import grit.core.store.{LifecycleStore, StoreError, Tx}

/** [[LifecycleStore]] over the one row of `grit.lifecycle_settings`. The durations are
  * `interval`s, so they can be changed by hand (`SET idle = '3 minutes'`), and read back to
  * the millisecond.
  */
final class SqlLifecycleStore extends LifecycleStore {
  import SqlStatements.attempt

  def current()(using tx: Tx^): Either[StoreError, LifecycleSettings] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """SELECT (extract(epoch FROM idle) * 1000)::bigint AS idle,
            |       (extract(epoch FROM retention) * 1000)::bigint AS retention,
            |       (extract(epoch FROM ledger) * 1000)::bigint AS ledger,
            |       balance,
            |       (extract(epoch FROM settle) * 1000)::bigint AS settle,
            |       resolve_at, asks, weight
            |  FROM grit.lifecycle_settings""".stripMargin
        )
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          if (!rs.next()) Right(LifecycleSettings.Default)
          else {
            val resolveAt = rs.getDouble("resolve_at")
            (for {
              weight <- Weight.of(rs.getDouble("weight"))
              windows <- Windows.of(
                rs.getLong("idle").millis,
                rs.getLong("retention").millis,
                rs.getLong("ledger").millis
              )
              at <- Probability
                .of(resolveAt)
                .toRight(s"resolve_at $resolveAt is not a probability")
              settings <- LifecycleSettings.of(
                windows,
                rs.getInt("balance"),
                rs.getLong("settle").millis,
                at,
                rs.getInt("asks"),
                weight
              )
            } yield settings).left.map(why => StoreError.Invalid(s"lifecycle settings: $why"))
          }
        }
      }
    }.flatten
  }

  def set(settings: LifecycleSettings)(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    val w = settings.windows
    attempt {
      Using.resource(
        conn.prepareStatement(
          """INSERT INTO grit.lifecycle_settings
             |       (idle, retention, ledger, balance, settle, resolve_at, asks, weight)
             |VALUES (? * interval '1 millisecond', ? * interval '1 millisecond',
             |        ? * interval '1 millisecond', ?,
             |        ? * interval '1 millisecond', ?, ?, ?)
             |ON CONFLICT (one) DO UPDATE SET idle = EXCLUDED.idle, retention = EXCLUDED.retention,
             |  ledger = EXCLUDED.ledger,
             |  balance = EXCLUDED.balance, settle = EXCLUDED.settle,
             |  resolve_at = EXCLUDED.resolve_at, asks = EXCLUDED.asks,
             |  weight = EXCLUDED.weight""".stripMargin
        )
      ) { ps =>
        ps.setLong(1, w.idle.toMillis)
        ps.setLong(2, w.retention.toMillis)
        ps.setLong(3, w.ledger.toMillis)
        ps.setInt(4, settings.balance)
        ps.setLong(5, settings.settle.toMillis)
        ps.setDouble(6, Probability.value(settings.resolveAt))
        ps.setInt(7, settings.asks)
        ps.setDouble(8, Weight.value(settings.weight))
        ps.executeUpdate()
        ()
      }
    }
  }
}
