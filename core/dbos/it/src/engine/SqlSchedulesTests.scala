package grit.dbos.engine

import java.time.Instant

import grit.core.clock.Clock
import grit.core.id.{JobName, PluginName, PrincipalId, TurnRef, TurnSeq}
import grit.core.job.{ScheduleContract, ScheduleDesk, ScheduleStore, Slot}
import grit.core.store.{Jot, Origin, StoreError, Tombstones, Tx}
import grit.dbos.sql.{DbConfig, LiveDb, SqlJot, SqlSchedules, SqlTombstones, TestPostgres}

import org.postgresql.ds.PGSimpleDataSource

/** The schedules contract, kept by the SQL store against a real Postgres. */
object SqlSchedulesTests extends ScheduleContract {
  protected def fresh(): ScheduleContract.Under = SqlSchedulesUnder("sql_schedules")
}

/** An empty SQL store in a database of its own, as the contracts take one. A turn is the first
  * of its origin's conversation; one asked from is recorded as the inbox and an edge record it:
  * its first entry a message its asker wrote, and its delivery's address. Each desk
  * transaction runs `beforeCommit` once its body has returned, before it commits.
  */
private[engine] object SqlSchedulesUnder {

  private def ok[A](what: String)(e: Either[StoreError, A]): A =
    e.fold(err => sys.error(s"$what: $err"), identity)

  def apply(suite: String, beforeCommit: () -> Unit = () => ()): ScheduleContract.Under = {
    // Opening an engine applies schema.sql; nothing here launches DBOS.
    val config: DbConfig = TestPostgres.freshDatabase(suite)
    LiveEngine.open(config, "test").close()
    val marks = new SqlTombstones
    val schedules = new SqlSchedules(marks)
    new ScheduleContract.Under {
      val store: ScheduleStore = schedules
      val tombstones: Tombstones = marks

      def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)

      def start(slot: Slot, version: Int, following: Option[Instant]): Unit =
        ok("starting")(LiveDb.transaction(config)(schedules.started(slot, version, following)))

      def turn(origin: Origin): TurnRef =
        TurnRef(LiveDb.conversation(config, origin).id, TurnSeq.First)

      def asking(turn: TurnRef, by: PrincipalId, address: Option[String]): Unit =
        LiveDb.asking(config, turn, by, address)

      def desk(plugin: PluginName, jobs: Vector[JobName], clock: Clock^): ScheduleDesk^ = {
        val ds = new PGSimpleDataSource()
        ds.setURL(config.jdbcUrl)
        ds.setUser(config.user)
        ds.setPassword(config.password)
        schedules.desk(plugin, jobs, new Held(new SqlJot(ds), beforeCommit), clock)
      }
    }
  }

  /** `inner`, running `beforeCommit` inside each transaction once its body has returned. */
  private final class Held(inner: Jot, beforeCommit: () -> Unit) extends Jot {
    def write[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      inner.write {
        val result = body
        beforeCommit()
        result
      }
  }
}
