package grit.dbos.engine

import java.sql.{Connection, DriverManager}
import java.time.format.DateTimeFormatter
import java.time.{Duration, Instant, ZoneId}

import scala.concurrent.duration.*
import scala.util.Using
import scala.util.control.NonFatal

import grit.dbos.sql.DbConfig

/** This database's one engine: a session advisory lock held on a connection of its own until
  * [[close]], and a row in `grit.engines` saying whose it is (ADR 0015). The lock is the
  * truth; the row describes it.
  */
final class EngineLock private (conn: Connection, val beat: FiniteDuration)
    extends caps.SharedCapability,
      AutoCloseable {

  /** Writes this holder's row: its machine, process, epoch and start, over any row a dead
    * holder left. Needs the schema; only [[Engine.start]] calls it.
    */
  private[engine] def claim(epoch: String): Either[String, Unit] =
    try {
      Using.resource(
        conn.prepareStatement(
          """INSERT INTO grit.engines (slot, machine, pid, backend_pid, epoch, started_at, heartbeat_at)
            |VALUES (true, ?, ?, pg_backend_pid(), ?, now(), now())
            |ON CONFLICT (slot) DO UPDATE SET machine = EXCLUDED.machine, pid = EXCLUDED.pid,
            |  backend_pid = EXCLUDED.backend_pid, epoch = EXCLUDED.epoch,
            |  started_at = EXCLUDED.started_at, heartbeat_at = EXCLUDED.heartbeat_at""".stripMargin
        )
      ) { ps =>
        ps.setString(1, EngineLock.machine)
        ps.setLong(2, EngineLock.pid)
        ps.setString(3, epoch)
        ps.executeUpdate()
      }
      Right(())
    } catch { case NonFatal(e) => Left(Option(e.getMessage).getOrElse(e.toString)) }

  /** Writes this holder's heartbeat. False when the lock is lost: the connection failed, or
    * the row is no longer this connection's (another holder took it, or it was deleted).
    */
  private[engine] def beatOnce(): Boolean =
    try
      Using.resource(
        conn.prepareStatement(
          "UPDATE grit.engines SET heartbeat_at = now() WHERE backend_pid = pg_backend_pid()"
        )
      )(_.executeUpdate() == 1)
    catch { case NonFatal(_) => false }

  /** Releases the lock, by closing its connection. */
  def close(): Unit =
    try conn.close()
    catch { case NonFatal(_) => () }
}

object EngineLock {

  /** The lock's key, the bytes of `gritengn`. Advisory locks are scoped to their database, so
    * one constant serves every database, and two databases on one server never contend.
    */
  val Key: Long = 0x67726974656e676eL

  /** How often the holder writes its heartbeat, and so the longest a lost lock goes
    * unnoticed: 2 seconds.
    */
  val Beat: FiniteDuration = 2.seconds

  /** The lock on the database `config` names. `Held` when another process has it, naming
    * that process once it has written its row; `Unreachable` when the database cannot be
    * reached. Takes nothing else: the schema is applied by [[Engine.start]].
    */
  def take(config: DbConfig): Either[NotTaken, EngineLock^] = take(config, Beat)

  /** [[take]], beating every `beat`: tests use a shorter one. */
  private[engine] def take(config: DbConfig, beat: FiniteDuration): Either[NotTaken, EngineLock^] =
    try {
      val conn = DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
      try {
        val got = Using.resource(conn.prepareStatement("SELECT pg_try_advisory_lock(?)")) { ps =>
          ps.setLong(1, Key)
          Using.resource(ps.executeQuery())(rs => rs.next() && rs.getBoolean(1))
        }
        if (got) Right(new EngineLock(conn, beat))
        else {
          val holder = heldBy(conn)
          conn.close()
          Left(NotTaken.Held(holder))
        }
      } catch {
        case NonFatal(e) =>
          conn.close()
          throw e
      }
    } catch {
      case NonFatal(e) => Left(NotTaken.Unreachable(Option(e.getMessage).getOrElse(e.toString)))
    }

  /** The engine holding the lock on the database `config` names, read on a short connection
    * of its own; `None` when none holds it, it has not written its row, or the database
    * cannot be read.
    */
  def holder(config: DbConfig): Option[Holder] =
    try
      Using.resource(DriverManager.getConnection(config.jdbcUrl, config.user, config.password))(
        heldBy
      )
    catch { case NonFatal(_) => None }

  /** The row of the process holding the lock: only a row whose connection holds it, so a row
    * a dead engine left is never named. `None` when there is none, or it cannot be read.
    */
  private def heldBy(conn: Connection): Option[Holder] =
    try
      Using.resource(
        conn.prepareStatement(
          """SELECT e.machine, e.pid, e.epoch, e.started_at, e.heartbeat_at FROM grit.engines e
            |  JOIN pg_locks l ON l.locktype = 'advisory' AND l.granted AND l.pid = e.backend_pid
            |   AND l.classid = ? AND l.objid = ? AND l.objsubid = 1
            | WHERE l.database = (SELECT oid FROM pg_database WHERE datname = current_database())""".stripMargin
        )
      ) { ps =>
        ps.setLong(1, Key >>> 32)
        ps.setLong(2, Key & 0xffffffffL)
        Using.resource(ps.executeQuery()) { rs =>
          Option.when(rs.next())(
            Holder(
              rs.getString(1),
              rs.getLong(2),
              rs.getString(3),
              rs.getTimestamp(4).toInstant,
              rs.getTimestamp(5).toInstant
            )
          )
        }
      }
    catch { case NonFatal(_) => None }

  /** This process's id, as the operating system knows it: read through the runtime's
    * management interface, which names no process API.
    */
  private[engine] def pid: Long = java.lang.management.ManagementFactory.getRuntimeMXBean.getPid

  /** This machine's name; `unknown` when it cannot be read. */
  private[engine] def machine: String =
    try java.net.InetAddress.getLocalHost.getHostName
    catch { case NonFatal(_) => "unknown" }
}

/** Why a lock was not taken. */
enum NotTaken {

  /** Another process holds it: `holder`, once it has written its row. */
  case Held(holder: Option[Holder])

  /** The database could not be reached: `why`, in the driver's words. */
  case Unreachable(why: String)

  /** One line for a person, saying who holds the lock and what to do, at `now`. */
  def message(now: Instant): String = this match {
    case Held(Some(h)) =>
      val started =
        DateTimeFormatter.ofPattern("HH:mm:ss").format(h.started.atZone(ZoneId.systemDefault()))
      val ago = Duration.between(h.heartbeat, now).getSeconds.max(0)
      s"grit is already running on this database: pid ${h.pid} on ${h.machine}, started $started, " +
        s"last heartbeat ${ago}s ago. Quit it, or set GRIT_DATABASE_URL to another database."
    case Held(None) =>
      "grit is already running on this database (it has not said where yet)."
    case Unreachable(why) => s"the database cannot be reached: $why"
  }
}

/** The engine holding a database's lock: its `machine` and process `pid`, its compatibility
  * `epoch`, when it `started`, and its last `heartbeat`.
  */
final case class Holder(
    machine: String,
    pid: Long,
    epoch: String,
    started: Instant,
    heartbeat: Instant
)
