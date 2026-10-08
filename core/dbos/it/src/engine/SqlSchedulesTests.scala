package grit.dbos.engine

import java.time.Instant

import scala.util.Using

import grit.core.clock.Clock
import grit.core.id.{JobName, PluginName, PrincipalId, TurnRef, TurnSeq}
import grit.core.identity.Account
import grit.core.job.{ScheduleContract, ScheduleDesk, ScheduleStore, Slot}
import grit.core.store.{Jot, Origin, StoreError, Tombstones, Tx}
import grit.core.visibility.{GroupName, Label, Subject, TestLabels}
import grit.dbos.sql.{
  DbConfig,
  LiveDb,
  Opener,
  SqlEntryStore,
  SqlIdentities,
  SqlJot,
  SqlSchedules,
  SqlTombstones,
  TestPostgres
}

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

  def apply(suite: String, beforeCommit: () -> Unit = () => ()): ScheduleContract.Under =
    withConfig(suite, beforeCommit)._1

  /** As [[apply]], with the database it is in. */
  def withConfig(
      suite: String,
      beforeCommit: () -> Unit = () => ()
  ): (ScheduleContract.Under, DbConfig) = {
    // Opening an engine applies schema.sql; nothing here launches DBOS.
    val config: DbConfig = TestPostgres.freshDatabase(suite)
    LiveEngine.open(config, "test").close()
    val marks = new SqlTombstones
    val schedules = new SqlSchedules(marks)
    val under = new ScheduleContract.Under {
      val store: ScheduleStore = schedules
      val tombstones: Tombstones = marks

      def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)

      def start(slot: Slot, version: Int, following: Option[Instant]): Unit =
        ok("starting")(LiveDb.transaction(config)(schedules.started(slot, version, following)))

      def turn(origin: Origin, label: Label): TurnRef =
        TurnRef(LiveDb.conversation(config, origin, label).id, TurnSeq.First)

      def asking(turn: TurnRef, by: Account, address: Option[String]): Unit =
        LiveDb.asking(config, turn, by, address)
      def added(account: Account, group: GroupName): Unit =
        ok("adding")(LiveDb.transaction(config) { (tx: Tx^) ?=>
          SqlIdentities.enroll(Set(account)).flatMap { _ =>
            SqlEntryStore.attempt {
              val conn: java.sql.Connection^{tx} = Tx.connection(tx)
              Using.resource(
                conn.prepareStatement(
                  "INSERT INTO grit.group_members (group_name, account) VALUES (?, ?)"
                )
              ) { ps =>
                ps.setString(1, GroupName.value(group))
                ps.setString(2, Account.written(account))
                ps.executeUpdate()
              }
            }
          }
        })

      def principal(account: Account): PrincipalId = LiveDb.principal(config, account)

      def desk(plugin: PluginName, jobs: Vector[JobName], clock: Clock^): ScheduleDesk^ = {
        val ds = new PGSimpleDataSource()
        ds.setURL(config.jdbcUrl)
        ds.setUser(config.user)
        ds.setPassword(config.password)
        schedules.desk(
          plugin,
          jobs,
          new Held(new SqlJot(ds, new Opener(TestLabels.Trialled)), beforeCommit),
          clock
        )
      }
    }
    (under, config)
  }

  /** `inner`, running `beforeCommit` inside each transaction once its body has returned. */
  private final class Held(inner: Jot, beforeCommit: () -> Unit) extends Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      inner.write(subject) {
        val result = body
        beforeCommit()
        result
      }
  }
}
