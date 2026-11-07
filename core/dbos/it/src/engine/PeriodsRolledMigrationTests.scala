package grit.dbos.engine

import java.time.Instant

import scala.io.Source
import scala.util.Using

import grit.core.id.{CloseRef, EntryId, PeriodRef, PeriodSeq}
import grit.core.message.Message
import grit.core.period.{CloseReason, TestClosings}
import grit.core.store.{Entry, Origin, Payload, Sealed, StoreError, Tx}
import grit.core.visibility.Integrity
import grit.dbos.sql.{DbConfig, LiveDb, SqlEntryStore, SqlPeriodStore, TestPostgres}

import utest.*

/** `periods-rolled.sql`, the one-off that widens `periods_reason_check` for a rolled close, run
  * on a database of the schema before it: this schema's table with the CHECK as it was.
  */
object PeriodsRolledMigrationTests extends TestSuite {

  private val at: Instant = Instant.parse("2026-11-06T12:00:00Z")

  private val migration: String =
    Using.resource(
      Option(getClass.getResourceAsStream("/periods-rolled.sql"))
        .getOrElse(sys.error("no periods-rolled.sql"))
    )(Source.fromInputStream(_).mkString)

  /** Runs `statements` on their own connection. */
  private def sql(config: DbConfig, statements: String): Unit =
    Using.resource(
      java.sql.DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
    )(conn => Using.resource(conn.createStatement())(_.execute(statements)))

  /** A fresh database whose `periods_reason_check` admits only the reasons before rolling. */
  private def previous(name: String): DbConfig = {
    val c = TestPostgres.freshDatabase(name)
    LiveEngine.open(c, "test").close()
    sql(
      c,
      """ALTER TABLE grit.periods DROP CONSTRAINT periods_reason_check;
        |ALTER TABLE grit.periods ADD CONSTRAINT periods_reason_check
        |    CHECK (reason IN ('resolved', 'lapsed', 'unearned', 'ran'))""".stripMargin
    )
    c
  }

  private val entries = new SqlEntryStore()
  private val periods = new SqlPeriodStore(entries)

  /** A room's first period, one message in it, sealed as rolled: what the store answered. */
  private def rolled(config: DbConfig, channel: String): Either[StoreError, Sealed] = {
    val c = LiveDb.conversation(config, Origin.Room("T1", channel)).id
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      for {
        next <- entries.lockNext(c)
        _ <- periods.openFor(c, next.turnSeq, at)
        _ <- entries.insert(
          Entry(
            EntryId(s"$channel:said"),
            c,
            next.turnSeq,
            None,
            next.seq,
            Payload.Message(Message.User("hi")),
            at,
            Integrity.Member
          )
        )
      } yield next.turnSeq
    } match {
      case Left(e) => Left(e)
      case Right(turn) =>
        // Its own transaction, so a refused seal rolls back nothing else.
        scala.util
          .Try(
            LiveDb.transaction(config)(
              periods.seal(
                CloseRef(PeriodRef(c, PeriodSeq.First), turn, at),
                CloseReason.Rolled,
                TestClosings.prose("said hi", None),
                at
              )
            )
          )
          .fold(e => Left(StoreError.DatabaseError(e.getMessage)), identity)
    }
  }

  val tests = Tests {
    test(
      "before the file a rolled close is refused; after it, sealed; run again, it changes nothing"
    ) {
      val config = previous("periods_rolled")
      rolled(config, "C1") match {
        case Left(StoreError.DatabaseError(why)) =>
          assert(why.contains("periods_reason_check"))
        case other => throw new java.lang.AssertionError(s"not refused: $other")
      }
      sql(config, migration)
      sql(config, migration)
      rolled(config, "C2").map(_.isInstanceOf[Sealed.Closed]) ==> Right(true)
    }
  }
}
