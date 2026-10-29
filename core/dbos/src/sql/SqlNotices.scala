package grit.dbos.sql

import java.sql.Connection

import scala.concurrent.duration.FiniteDuration
import scala.util.Using

import grit.core.store.{StoreError, Tx}

import org.postgresql.PGConnection

/** Postgres notices (`NOTIFY`): what a writer sends inside its transaction reaches every
  * connection listening on its channel when that transaction commits, and never when it rolls
  * back. A listening connection keeps session state, so it is one of its own, never the pool's.
  */
private[dbos] object SqlNotices {

  /** The channel a write that versions a document notices on as it commits, the payload
    * `plugin/key`: once per key a transaction versions.
    */
  val Documents = "grit_documents"

  /** The channel a recording that logs a commit notices on as it commits, the payload empty: once
    * per transaction.
    */
  val Sessions = "grit_sessions"

  /** A notice a listener heard: its `channel` and `payload`. */
  final case class Heard(channel: String, payload: String)

  /** Sends `payload` on `channel` as this transaction commits; the same payload sent again on the
    * same channel in one transaction is delivered once. A payload is under 8,000 bytes, or the
    * transaction fails.
    */
  def send(channel: String, payload: String)(using Tx^): Either[StoreError, Unit] =
    SqlStatements
      .one("SELECT pg_notify(?, ?)") { ps =>
        ps.setString(1, channel)
        ps.setString(2, payload)
      }(_ => ())
      .map(_ => ())

  /** `conn`, in autocommit, listening on each of `channels` from now; throws when it cannot. */
  def listen(conn: Connection, channels: Vector[String]): Unit =
    Using.resource(conn.createStatement()) { st =>
      channels.foreach(c => st.execute(s"""LISTEN "$c""""))
    }

  /** The notices `conn` heard since the last drain, in the order their transactions committed,
    * waiting up to `within` (at least a millisecond) for the first; throws when `conn` is closed
    * or broken.
    */
  def drain(conn: Connection, within: FiniteDuration): Vector[Heard] =
    Option(
      conn
        .unwrap(classOf[PGConnection])
        .getNotifications(within.toMillis.max(1L).min(Int.MaxValue).toInt)
    ).fold(Vector.empty[Heard])(_.toVector.map(n => Heard(n.getName, n.getParameter)))
}
