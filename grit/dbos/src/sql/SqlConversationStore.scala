package grit.dbos.sql

import java.time.OffsetDateTime

import scala.util.Using

import grit.core.id.ConversationId
import grit.core.place.Directory
import grit.core.store.{Conversation, ConversationStore, Origin, StoreError, Tx}

/** [[ConversationStore]] over the `grit.conversations` table. */
final class SqlConversationStore extends ConversationStore {
  import SqlEntryStore.attempt

  def findOrCreate(
      origin: Origin
  )(using tx: Tx^): Either[StoreError, Conversation] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    // A no-op DO UPDATE, not DO NOTHING, so RETURNING yields the row whether it
    // was inserted or already there: one statement, correct under any isolation.
    val sql =
      """INSERT INTO grit.conversations (origin) VALUES (?::jsonb)
        |ON CONFLICT (origin) DO UPDATE SET origin = EXCLUDED.origin
        |RETURNING id, created_at""".stripMargin
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        ps.setString(1, SqlConversationStore.originJson(origin).render())
        Using.resource(ps.executeQuery()) { rs =>
          rs.next()
          Conversation(
            id = ConversationId(rs.getString("id")),
            origin = origin,
            createdAt = rs.getObject("created_at", classOf[OffsetDateTime]).toInstant
          )
        }
      }
    }
  }
}

private[dbos] object SqlConversationStore {

  /** The stored form of an origin, and the conversation's unique key. Changing
    * it for an existing origin orphans that origin's conversation.
    */
  def originJson(origin: Origin): ujson.Obj = origin match {
    case Origin.Tui(directory, session) =>
      ujson.Obj("kind" -> "tui", "directory" -> Directory.value(directory), "session" -> session)
    case Origin.Slack(team, channel, threadTs) =>
      ujson.Obj("kind" -> "slack", "team" -> team, "channel" -> channel, "threadTs" -> threadTs)
    case Origin.Task(name, run) =>
      ujson.Obj("kind" -> "task", "name" -> name, "run" -> run)
  }

  /** The origin stored as `v` ([[originJson]]'s form), or why it is none. */
  def readOrigin(v: ujson.Value): Either[String, Origin] = {
    def str(o: collection.Map[String, ujson.Value], key: String): Either[String, String] =
      o.get(key).toRight(s"missing field: $key").flatMap(_.strOpt.toRight(s"$key is not a string"))
    for {
      o <- v.objOpt.toRight("expected an object")
      kind <- str(o, "kind")
      origin <- kind match {
        case "tui" =>
          for {
            raw <- str(o, "directory")
            session <- str(o, "session")
            directory <- Directory.of(raw)
          } yield Origin.Tui(directory, session)
        case "slack" =>
          for {
            team <- str(o, "team")
            channel <- str(o, "channel")
            threadTs <- str(o, "threadTs")
          } yield Origin.Slack(team, channel, threadTs)
        case "task" => str(o, "name").flatMap(n => str(o, "run").map(Origin.Task(n, _)))
        case other => Left(s"unknown origin kind: $other")
      }
    } yield origin
  }
}
