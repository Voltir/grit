package grit.dbos.engine

import java.time.Instant

import scala.io.Source
import scala.util.Using

import grit.core.id.{EntryId, TurnRef}
import grit.core.speech.{Reach, ReplyTo}
import grit.core.store.{Entry, Origin, Payload, Tx}
import grit.core.visibility.Integrity
import grit.dbos.sql.{DbConfig, LiveDb, SqlEntryStore, SqlSpeechStore, TestPostgres}

import utest.*

/** `speech-room-to.sql`, the one-off that adds a heard message's room address and a draft's
  * venue, run on a database of the schema before it: this schema's tables without `room_to`.
  */
object SpeechRoomToMigrationTests extends TestSuite {

  private val at: Instant = Instant.parse("2026-11-07T12:00:00Z")

  private val migration: String =
    Using.resource(
      Option(getClass.getResourceAsStream("/speech-room-to.sql"))
        .getOrElse(sys.error("no speech-room-to.sql"))
    )(Source.fromInputStream(_).mkString)

  /** Runs `statements` on their own connection. */
  private def sql(config: DbConfig, statements: String): Unit =
    Using.resource(
      java.sql.DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
    )(conn => Using.resource(conn.createStatement())(_.execute(statements)))

  /** A fresh database whose grit.heard and grit.speech have no `room_to`. */
  private def previous(name: String): DbConfig = {
    val c = TestPostgres.freshDatabase(name)
    LiveEngine.open(c, "test").close()
    sql(
      c,
      """ALTER TABLE grit.heard DROP COLUMN room_to;
        |ALTER TABLE grit.speech DROP COLUMN room_to""".stripMargin
    )
    c
  }

  private val entries = new SqlEntryStore()
  private val speech = new SqlSpeechStore()

  private val reach = Reach(Some(ReplyTo("C1/1.0/1.0", Some("C1//1.0"))), Set.empty)

  /** A message heard in `channel`'s top level, its reach kept and read back. */
  private def heard(config: DbConfig, channel: String): Either[String, Option[Reach]] = {
    val c = LiveDb.conversation(config, Origin.Room("T1", channel)).id
    scala.util
      .Try(LiveDb.transaction(config) { (tx: Tx^) ?=>
        for {
          next <- entries.lockNext(c)
          _ <- entries.insert(
            Entry(
              EntryId(s"$channel:heard"),
              c,
              next.turnSeq,
              None,
              next.seq,
              Payload.Heard("heads up"),
              at,
              Integrity.Member
            )
          )
          turn = TurnRef(c, next.turnSeq)
          _ <- speech.heard(turn, reach)
          kept <- speech.reach(turn)
        } yield kept
      })
      .fold(e => Left(e.getMessage), _.left.map(_.toString))
  }

  val tests = Tests {
    test(
      "before the file a room address cannot be kept; after it, it is kept and read back; run again, it changes nothing"
    ) {
      val config = previous("speech_room_to")
      heard(config, "C1") match {
        case Left(why) => assert(why.contains("room_to"))
        case other => throw new java.lang.AssertionError(s"not refused: $other")
      }
      sql(config, migration)
      sql(config, migration)
      heard(config, "C2") ==> Right(Some(reach))
    }
  }
}
