package grit.dbos.sql

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.period.{LifecycleSettings, Probability, Windows}
import grit.core.place.{Locality, Prefix, Scope, Weight}
import grit.core.store.{LifecycleStore, StoreError, Tx}

/** [[LifecycleStore]] over the one row of `grit.lifecycle_settings`. The durations are
  * `interval`s, so they can be changed by hand (`SET idle = '3 minutes'`), and read back to
  * the millisecond; the scope is its places as written (`SET scope = '{fs:/home/you}'`).
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
            |       (extract(epoch FROM ledger) * 1000)::bigint AS ledger,
            |       balance,
            |       (extract(epoch FROM settle) * 1000)::bigint AS settle,
            |       resolve_at, asks, to_jsonb(scope)::text AS scope, weight
            |  FROM grit.lifecycle_settings""".stripMargin
        )
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          if (!rs.next()) Right(LifecycleSettings.Default)
          else {
            val resolveAt = rs.getDouble("resolve_at")
            val written = ujson.read(rs.getString("scope")).arr.toVector.map(_.str)
            (for {
              // Each place read on its own, so one written by hand may hold a space.
              prefixes <- written.foldLeft[Either[String, Vector[Prefix]]](Right(Vector.empty)) {
                (acc, w) => acc.flatMap(done => Prefix.read(w).map(done :+ _))
              }
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
                Locality(Scope(prefixes), weight)
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
             |       (idle, retention, ledger, balance, settle, resolve_at, asks, scope, weight)
             |VALUES (? * interval '1 millisecond', ? * interval '1 millisecond',
             |        ? * interval '1 millisecond', ?,
             |        ? * interval '1 millisecond', ?, ?,
             |        ARRAY(SELECT jsonb_array_elements_text(?::jsonb)), ?)
             |ON CONFLICT (one) DO UPDATE SET idle = EXCLUDED.idle, retention = EXCLUDED.retention,
             |  ledger = EXCLUDED.ledger,
             |  balance = EXCLUDED.balance, settle = EXCLUDED.settle,
             |  resolve_at = EXCLUDED.resolve_at, asks = EXCLUDED.asks,
             |  scope = EXCLUDED.scope, weight = EXCLUDED.weight""".stripMargin
        )
      ) { ps =>
        ps.setLong(1, w.idle.toMillis)
        ps.setLong(2, w.retention.toMillis)
        ps.setLong(3, w.ledger.toMillis)
        ps.setInt(4, settings.balance)
        ps.setLong(5, settings.settle.toMillis)
        ps.setDouble(6, Probability.value(settings.resolveAt))
        ps.setInt(7, settings.asks)
        // The scope's places as written, sent as JSON so no Java array crosses JDBC.
        ps.setString(
          8,
          ujson.Arr.from(settings.locality.scope.prefixes.map(p => ujson.Str(p.written))).render()
        )
        ps.setDouble(9, Weight.value(settings.locality.weight))
        ps.executeUpdate()
        ()
      }
    }
  }
}
