package grit.dbos.sql

import scala.concurrent.duration.*

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}

/** The connections one engine, link or reader shares for its own short transactions, so a
  * transaction takes an open session rather than opening one. A connection that keeps session
  * state past its transaction (a session advisory lock, a `LISTEN`, a `SET`) is never taken
  * from it: those open their own.
  */
private[dbos] object Pool {

  /** The sessions a pool keeps open, and the most it opens at once: 4. */
  val Size: Int = 4

  /** How long a transaction waits for a free session before it fails as an unreachable
    * database does: 10 seconds.
    */
  val Wait: FiniteDuration = 10.seconds

  /** What a pool's sessions are named in `pg_stat_activity`: `grit`. */
  val ApplicationName: String = "grit"

  /** A new pool on `config`'s database, its sessions kept read-only by Postgres when
    * `readOnly` (`default_transaction_read_only`); whoever opens it closes it, also when what
    * it builds over the pool throws. Throws when the database cannot be reached.
    */
  def open(config: DbConfig, readOnly: Boolean): HikariDataSource =
    new HikariDataSource(settings(config, readOnly))

  private def settings(config: DbConfig, readOnly: Boolean): HikariConfig = {
    val c = new HikariConfig()
    c.setJdbcUrl(config.jdbcUrl)
    c.setUsername(config.user)
    c.setPassword(config.password)
    c.setMaximumPoolSize(Size)
    c.setMinimumIdle(Size)
    c.setConnectionTimeout(Wait.toMillis)
    c.addDataSourceProperty("ApplicationName", ApplicationName)
    // A startup parameter: Postgres itself makes every transaction on these sessions
    // read-only, whatever the driver or DBOS's client does with them.
    if (readOnly) c.addDataSourceProperty("options", "-c default_transaction_read_only=on")
    c
  }
}
