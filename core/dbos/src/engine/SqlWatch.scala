package grit.dbos.engine

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import java.util.concurrent.{ConcurrentHashMap, Semaphore, TimeUnit}
import javax.sql.DataSource

import scala.annotation.tailrec
import scala.concurrent.duration.*
import scala.util.Using
import scala.util.control.NonFatal

import grit.core.edge.{EdgeWatch, WatchError, WatchFor, Watched}
import grit.core.identity.Account
import grit.core.place.Place
import grit.dbos.sql.{DbConfig, Opener, SqlAsking, SqlDb, SqlNotices, SqlWatched}

/** [[EdgeWatch]] over Postgres (ADR 0051): one listener, on a connection of its own to `config`'s
  * database and a thread of its own, both started by the first watch and kept until [[close]],
  * rings the watches a notice may meet ([[SqlNotices]]); each watch reads on `dataSource`, from
  * its own thread, read-only and rolled back, as its asker ([[SqlAsking]]). A watch registers
  * before its first read, and the listener rings every watch each time it begins listening, so
  * no commit after a watch begins goes unread. While the listener is down a watch reads again
  * every [[SqlWatch.Reread]]; it reconnects after [[SqlWatch.FirstBackoff]], doubling to at most
  * [[SqlWatch.LongestBackoff]]. Closing lapses every watch under way. At most `most` watches
  * run at once.
  */
final class SqlWatch private[dbos] (
    config: DbConfig,
    dataSource: DataSource,
    opener: Opener,
    most: Int = EdgeWatch.Most
) extends EdgeWatch,
      AutoCloseable {

  /** A watch under way: which notices may meet it, and the bell the listener rings. */
  private final class Waiter(val hears: SqlNotices.Heard -> Boolean) {
    val bell = new Semaphore(0)
    def ring(): Unit = bell.release()
  }

  private val waiters = ConcurrentHashMap.newKeySet[Waiter]()
  private val under = new AtomicInteger(0)
  private val asleep = new AtomicInteger(0)
  private val started = new AtomicBoolean(false)
  private val up = new AtomicBoolean(false)
  private val closed = new AtomicBoolean(false)
  private val listener = new Thread(() => listen(), "grit-watch")
  listener.setDaemon(true)

  /** How many watches are under way. */
  private[dbos] def underWay: Int = under.get

  /** How many watches are waiting to be rung, read and unmet. */
  private[dbos] def waiting: Int = asleep.get

  /** Whether the listener is listening now. */
  private[dbos] def listening: Boolean = up.get

  def until(
      by: Account,
      room: Place,
      what: WatchFor,
      within: FiniteDuration
  ): Either[WatchError, Watched] =
    if (under.incrementAndGet() > most) {
      under.decrementAndGet()
      Left(WatchError.Busy)
    } else
      try {
        val waiter = new Waiter(SqlWatch.hears(what))
        waiters.add(waiter)
        try {
          if (started.compareAndSet(false, true)) listener.start()
          val deadline = SqlWatch.monotonic() + EdgeWatch.bounded(within).toNanos
          watching(waiter, by, room, what, deadline)
        } finally waiters.remove(waiter)
      } finally {
        val _ = under.decrementAndGet()
      }

  /** Stops the listener and lapses every watch under way. Once; later calls return at once. */
  def close(): Unit =
    if (closed.compareAndSet(false, true)) {
      waiters.forEach(_.ring())
      listener.interrupt()
    }

  @tailrec
  private def watching(
      waiter: Waiter,
      by: Account,
      room: Place,
      what: WatchFor,
      deadline: Long
  ): Either[WatchError, Watched] = {
    waiter.bell.drainPermits()
    read(by, room, what) match {
      case Left(e) => Left(e)
      case Right(Some(met)) => Right(met)
      case Right(None) =>
        val left = deadline - SqlWatch.monotonic()
        if (left <= 0 || closed.get) Right(Watched.Lapsed)
        else {
          val wait = if (up.get) left else left.min(SqlWatch.Reread.toNanos)
          val rung =
            try {
              asleep.incrementAndGet()
              val _ = waiter.bell.tryAcquire(wait, TimeUnit.NANOSECONDS)
              true
            } catch { case _: InterruptedException => false }
            finally {
              val _ = asleep.decrementAndGet()
            }
          if (!rung || closed.get) Right(Watched.Lapsed)
          else watching(waiter, by, room, what, deadline)
        }
    }
  }

  /** What meets `what` as the person `by` is linked to reads it now, in `room`. */
  private def read(by: Account, room: Place, what: WatchFor): Either[WatchError, Option[Watched]] =
    SqlDb.rolledBack(dataSource) { conn =>
      SqlAsking.as(conn, opener, by, room)(WatchError.NotVouched(_), WatchError.Unavailable(_)) {
        (_, tx) => SqlWatched.met(what)(using tx).left.map(e => WatchError.Unavailable(e.toString))
      }
    }(e => WatchError.Unavailable(e.cause))

  /** The listener's loop, until [[close]]: listen, ring every watch, then ring each watch a notice
    * may meet; on a dropped connection ring every watch, wait out the backoff, and listen again.
    */
  private def listen(): Unit = {
    var backoff = SqlWatch.FirstBackoff
    while (!closed.get) {
      try {
        Using.resource(config.connect(SqlWatch.ApplicationName)) { conn =>
          SqlNotices.listen(conn, Vector(SqlNotices.Documents, SqlNotices.Sessions))
          up.set(true)
          backoff = SqlWatch.FirstBackoff
          waiters.forEach(_.ring())
          while (!closed.get) {
            val heard = SqlNotices.drain(conn, SqlWatch.Drain)
            if (heard.nonEmpty) waiters.forEach(w => if (heard.exists(w.hears)) w.ring())
          }
        }
      } catch { case NonFatal(_) => () }
      up.set(false)
      waiters.forEach(_.ring())
      if (!closed.get) {
        try Thread.sleep(backoff.toMillis)
        catch { case _: InterruptedException => () }
        backoff = (backoff * 2).min(SqlWatch.LongestBackoff)
      }
    }
  }
}

object SqlWatch {

  /** 30 seconds: how often a watch reads again while the listener is down. */
  val Reread: FiniteDuration = 30.seconds

  /** 1 second: how long the listener waits before listening again after its connection drops. */
  val FirstBackoff: FiniteDuration = 1.second

  /** 30 seconds: the longest the listener waits before listening again. */
  val LongestBackoff: FiniteDuration = 30.seconds

  /** `grit-watch`: what the listener's connection is named in `pg_stat_activity`. */
  val ApplicationName: String = "grit-watch"

  /** How long the listener waits for a notice before looking whether it was closed. */
  private val Drain: FiniteDuration = 1.second

  /** The notices that may meet `what`: its key's versions, or any commit. */
  private def hears(what: WatchFor): SqlNotices.Heard -> Boolean = what match {
    case h: WatchFor.Holding =>
      _ == SqlNotices.Heard(SqlNotices.Documents, SqlNotices.versioned(h.plugin, h.key))
    case _: WatchFor.Committed => _.channel == SqlNotices.Sessions
  }

  /** The JVM's monotonic nanoseconds, for a watch's deadline: a watch takes no Clock, as its
    * lapse is a bound on its own wait, never a time it records.
    */
  private def monotonic(): Long = System.nanoTime() // clock-check: see the doc above
}
