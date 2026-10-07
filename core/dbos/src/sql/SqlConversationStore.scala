package grit.dbos.sql

import java.time.OffsetDateTime

import scala.util.Using

import grit.core.id.{CallSlot, ConversationId, PrincipalId}
import grit.core.place.Directory
import grit.core.store.{Conversation, ConversationStore, Origin, StoreError, Tx}
import grit.core.visibility.Label

/** [[ConversationStore]] over the `grit.conversations` table, and `grit.places`. */
final class SqlConversationStore extends ConversationStore {
  import SqlEntryStore.attempt

  def findOrCreate(
      origin: Origin,
      by: PrincipalId,
      label: Label
  )(using tx: Tx^): Either[StoreError, Conversation] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    // A no-op DO UPDATE, not DO NOTHING, so RETURNING yields the row whether it was inserted or
    // already there: one statement, correct under any isolation. Its place and its room go in
    // first the same way, in one insert (they are one row for a directory's session), their
    // paths sent as JSON so no Java array crosses JDBC. The label is interned before, so the
    // statement can read its row back.
    val sql =
      s"""WITH wanted (kind, path) AS (
         |  VALUES ('place', ARRAY(SELECT jsonb_array_elements_text(?::jsonb))),
         |         ('room', ARRAY(SELECT jsonb_array_elements_text(?::jsonb)))),
         |placed AS (
         |  INSERT INTO grit.places (path) SELECT DISTINCT path FROM wanted
         |  ON CONFLICT (path) DO UPDATE SET path = EXCLUDED.path
         |  RETURNING id, path),
         |made AS (
         |  INSERT INTO grit.conversations (origin, place_id, room_id, created_by, label_id)
         |  SELECT ?::jsonb, p.id, r.id, ?, ?
         |    FROM wanted wp JOIN placed p ON p.path = wp.path,
         |         wanted wr JOIN placed r ON r.path = wr.path
         |   WHERE wp.kind = 'place' AND wr.kind = 'room'
         |  ON CONFLICT (origin) DO UPDATE SET origin = EXCLUDED.origin
         |  RETURNING id, created_by, created_at, label_id)
         |SELECT made.id, made.created_by, made.created_at, ${SqlLabels.columns("l")}
         |  FROM made JOIN grit.labels l ON l.id = made.label_id""".stripMargin
    SqlLabels.intern(label).flatMap { interned =>
      attempt {
        Using.resource(conn.prepareStatement(sql)) { ps =>
          ps.setString(1, ujson.Arr.from(origin.place.segments.map(ujson.Str(_))).render())
          ps.setString(2, ujson.Arr.from(origin.room.segments.map(ujson.Str(_))).render())
          ps.setString(3, SqlConversationStore.originJson(origin).render())
          ps.setString(4, PrincipalId.value(by))
          ps.setInt(5, interned)
          Using.resource(ps.executeQuery()) { rs =>
            rs.next()
            Conversation(
              id = ConversationId(rs.getString("id")),
              origin = origin,
              createdBy = PrincipalId(rs.getString("created_by")),
              createdAt = rs.getObject("created_at", classOf[OffsetDateTime]).toInstant,
              label = SqlLabels.read(rs)
            )
          }
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
          s"""SELECT c.origin::text AS origin, c.created_by, c.created_at, ${SqlLabels.columns("l")}
             |  FROM grit.conversations c JOIN grit.labels l ON l.id = c.label_id
             | WHERE c.id = ?::uuid""".stripMargin
        )
      ) { ps =>
        ps.setString(1, ConversationId.value(id))
        Using.resource(ps.executeQuery()) { rs =>
          Option.when(rs.next()) {
            (
              rs.getString("origin"),
              rs.getString("created_by"),
              rs.getObject("created_at", classOf[OffsetDateTime]).toInstant,
              SqlLabels.read(rs)
            )
          }
        }
      }
    }.flatMap {
      case None => Right(None)
      case Some((origin, by, at, label)) =>
        SqlConversationStore
          .readOrigin(ujson.read(origin))
          .map(o => Some(Conversation(id, o, PrincipalId(by), at, label)))
          .left
          .map(why => StoreError.DatabaseError(s"conversation ${ConversationId.value(id)}: $why"))
    }
  }

  def remove(conversation: ConversationId)(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    // Entries, periods and their verdicts go by cascade. A room a document was kept in stays
    // with the document. A place or room another conversation took meanwhile is kept: the foreign key refuses its delete, and the caller tries again.
    attempt {
      Using.resource(
        conn.prepareStatement(
          """WITH gone AS (
            |  DELETE FROM grit.conversations WHERE id = ?::uuid RETURNING place_id, room_id)
            |DELETE FROM grit.places p USING gone
            | WHERE p.id IN (gone.place_id, gone.room_id)
            |   AND NOT EXISTS (SELECT 1 FROM grit.conversations c
            |                    WHERE (c.place_id = p.id OR c.room_id = p.id)
            |                      AND c.id <> ?::uuid)
            |   AND NOT EXISTS (SELECT 1 FROM grit.documents d WHERE d.room_id = p.id)""".stripMargin
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
