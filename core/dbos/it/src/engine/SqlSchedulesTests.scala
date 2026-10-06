package grit.dbos.engine

import java.time.Instant

import grit.core.clock.Clock
import grit.core.id.{ConversationId, EntryId, JobName, PluginName, PrincipalId, TurnRef, TurnSeq}
import grit.core.job.{ScheduleContract, ScheduleDesk, ScheduleStore, Slot}
import grit.core.message.Message
import grit.core.store.{Entry, Origin, Payload, StoreError, Tombstones, Tx}
import grit.dbos.sql.{
  DbConfig,
  LiveDb,
  SqlDeliveries,
  SqlEntryStore,
  SqlJot,
  SqlPrincipals,
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
  * of the task conversation `contract/{name}`; one asked from is recorded as the inbox and an
  * edge record it: its first entry a message its asker wrote, and its delivery's address.
  */
private[engine] object SqlSchedulesUnder {

  private def ok[A](what: String)(e: Either[StoreError, A]): A =
    e.fold(err => sys.error(s"$what: $err"), identity)

  def apply(suite: String): ScheduleContract.Under = {
    // Opening an engine applies schema.sql; nothing here launches DBOS.
    val config: DbConfig = TestPostgres.freshDatabase(suite)
    LiveEngine.open(config, "test").close()
    val marks = new SqlTombstones
    val schedules = new SqlSchedules(marks)
    val entries = new SqlEntryStore()
    new ScheduleContract.Under {
      val store: ScheduleStore = schedules
      val tombstones: Tombstones = marks

      def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)

      def start(slot: Slot, version: Int, following: Option[Instant]): Unit =
        ok("starting")(LiveDb.transaction(config)(schedules.started(slot, version, following)))

      def turn(origin: Origin): TurnRef =
        TurnRef(LiveDb.conversation(config, origin).id, TurnSeq.First)

      def asking(turn: TurnRef, by: PrincipalId, address: Option[String]): Unit =
        ok("asking")(LiveDb.transaction(config) {
          val id = EntryId(s"${ConversationId.value(turn.conversationId)}:asked")
          for {
            _ <-
              if (by == PrincipalId.Local || by == PrincipalId.Grit) Right(())
              else new SqlPrincipals().enroll(by, PrincipalId.value(by))
            next <- entries.lockNext(turn.conversationId)
            _ <- entries.insert(
              Entry(
                id,
                turn.conversationId,
                turn.turnSeq,
                None,
                next.seq,
                Payload.Message(Message.User("remind me")),
                Instant.parse("2026-10-07T08:00:00Z")
              )
            )
            _ <- authored(id, by)
            _ <- address.fold[Either[StoreError, Unit]](Right(()))(
              new SqlDeliveries().await(turn, _)
            )
          } yield ()
        })

      def desk(plugin: PluginName, jobs: Vector[JobName], clock: Clock^): ScheduleDesk^ = {
        val ds = new PGSimpleDataSource()
        ds.setURL(config.jdbcUrl)
        ds.setUser(config.user)
        ds.setPassword(config.password)
        schedules.desk(plugin, jobs, new SqlJot(ds), clock)
      }
    }
  }

  /** Records that `by` wrote the inbound entry `id`, as the inbox does. */
  private def authored(id: EntryId, by: PrincipalId)(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    scala.util.Using.resource(
      conn.prepareStatement("INSERT INTO grit.inbound (entry_id, author) VALUES (?, ?)")
    ) { ps =>
      ps.setString(1, EntryId.value(id))
      ps.setString(2, PrincipalId.value(by))
      ps.executeUpdate()
    }
    Right(())
  }
}
