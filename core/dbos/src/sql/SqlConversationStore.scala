package grit.dbos.sql

import java.time.OffsetDateTime

import scala.util.Using

import grit.core.id.{CallSlot, ConversationId, PrincipalId}
import grit.core.place.Directory
import grit.core.store.{Conversation, ConversationStore, Origin, StoreError, Tx}

/** [[ConversationStore]] over the `grit.conversations` table, and `grit.places`. */
final class SqlConversationStore extends ConversationStore {
  import SqlEntryStore.attempt

  def findOrCreate(
      origin: Origin,
      by: PrincipalId
  )(using tx: Tx^): Either[StoreError, Conversation] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    // A no-op DO UPDATE, not DO NOTHING, so RETURNING yields the row whether it
    // was inserted or already there: one statement, correct under any isolation. The place
    // goes in first the same way, its path sent as JSON so no Java array crosses JDBC.
    val sql =
      """WITH place AS (
        |  INSERT INTO grit.places (path)
        |  VALUES (ARRAY(SELECT jsonb_array_elements_text(?::jsonb)))
        |  ON CONFLICT (path) DO UPDATE SET path = EXCLUDED.path
        |  RETURNING id)
        |INSERT INTO grit.conversations (origin, place_id, created_by)
        |SELECT ?::jsonb, id, ? FROM place
        |ON CONFLICT (origin) DO UPDATE SET origin = EXCLUDED.origin
        |RETURNING id, created_by, created_at""".stripMargin
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        ps.setString(1, ujson.Arr.from(origin.place.segments.map(ujson.Str(_))).render())
        ps.setString(2, SqlConversationStore.originJson(origin).render())
        ps.setString(3, PrincipalId.value(by))
        Using.resource(ps.executeQuery()) { rs =>
          rs.next()
          Conversation(
            id = ConversationId(rs.getString("id")),
            origin = origin,
            createdBy = PrincipalId(rs.getString("created_by")),
            createdAt = rs.getObject("created_at", classOf[OffsetDateTime]).toInstant
          )
        }
      }
    }
  }

  def find(origin: Origin)(using tx: Tx^): Either[StoreError, Option[Conversation]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement("SELECT id FROM grit.conversations WHERE origin = ?::jsonb")
      ) { ps =>
        ps.setString(1, SqlConversationStore.originJson(origin).render())
        Using.resource(ps.executeQuery()) { rs =>
          Option.when(rs.next())(ConversationId(rs.getString(1)))
        }
      }
    }.flatMap {
      case None => Right(None)
      case Some(id) => get(id)
    }
  }

  def postedBy(
      conversation: ConversationId
  )(using tx: Tx^): Either[StoreError, Option[CallSlot]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement("SELECT request FROM grit.posted WHERE conversation_id = ?::uuid")
      ) { ps =>
        ps.setString(1, ConversationId.value(conversation))
        Using.resource(ps.executeQuery())(rs => Option.when(rs.next())(rs.getString(1)))
      }
    }.flatMap {
      case None => Right(None)
      case Some(key) =>
        CallSlot
          .read(key)
          .map(Some(_))
          .toRight(StoreError.Invalid(s"grit.posted holds $key, which is no call's slot"))
    }
  }

  def get(id: ConversationId)(using tx: Tx^): Either[StoreError, Option[Conversation]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          "SELECT origin::text, created_by, created_at FROM grit.conversations WHERE id = ?::uuid"
        )
      ) { ps =>
        ps.setString(1, ConversationId.value(id))
        Using.resource(ps.executeQuery()) { rs =>
          Option.when(rs.next()) {
            (
              rs.getString(1),
              rs.getString(2),
              rs.getObject(3, classOf[OffsetDateTime]).toInstant
            )
          }
        }
      }
    }.flatMap {
      case None => Right(None)
      case Some((origin, by, at)) =>
        SqlConversationStore
          .readOrigin(ujson.read(origin))
          .map(o => Some(Conversation(id, o, PrincipalId(by), at)))
          .left
          .map(why => StoreError.DatabaseError(s"conversation ${ConversationId.value(id)}: $why"))
    }
  }

  def remove(conversation: ConversationId)(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    // Entries, periods and their verdicts go by cascade. A place another conversation took
    // meanwhile is kept: the foreign key refuses its delete, and the caller tries again.
    attempt {
      Using.resource(
        conn.prepareStatement(
          """WITH gone AS (DELETE FROM grit.conversations WHERE id = ?::uuid RETURNING place_id)
            |DELETE FROM grit.places p USING gone
            | WHERE p.id = gone.place_id
            |   AND NOT EXISTS (SELECT 1 FROM grit.conversations c
            |                    WHERE c.place_id = gone.place_id AND c.id <> ?::uuid)""".stripMargin
        )
      ) { ps =>
        ps.setString(1, ConversationId.value(conversation))
        ps.setString(2, ConversationId.value(conversation))
        ps.executeUpdate()
        ()
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
