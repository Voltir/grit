package grit.dbos.sql

import java.time.OffsetDateTime

import scala.util.Using

import grit.core.id.{CallSlot, ConversationId}
import grit.core.identity.Account
import grit.core.place.Directory
import grit.core.store.{Conversation, ConversationStore, Origin, StoreError, Tx}
import grit.core.visibility.Label

/** [[ConversationStore]] over the `grit.conversations` table, and `grit.places`. */
final class SqlConversationStore extends ConversationStore {
  import SqlEntryStore.attempt

  def findOrCreate(
      origin: Origin,
      by: Account,
      label: Label
  )(using tx: Tx^): Either[StoreError, Conversation] =
    find(origin).flatMap {
      case Some(found) => Right(found)
      case None =>
        for {
          interned <- SqlLabels.intern(label)
          _ <- SqlIdentities.enroll(Set(by))
          _ <- created(origin, by, interned)
          // Read committed: this statement sees the conversation whichever insert made it.
          made <- find(origin)
          conversation <- made.toRight(
            StoreError.Invalid(
              s"the conversation of ${SqlConversationStore.originJson(origin)} was not made"
            )
          )
        } yield conversation
    }

  /** `origin`'s conversation inserted, with its place and its room, unless there already. Every
    * insert is `ON CONFLICT DO NOTHING`, never `DO UPDATE`: an update would lock the existing
    * row, and a transaction writing a second entry to another conversation in the room holds a
    * key-share lock on that room's row (and on the conversation's, when it is this one) until it
    * commits, so making or finding a conversation would wait on whatever else that transaction
    * does. A concurrent insert of the same row is waited for, and its row kept. Paths go as JSON,
    * so no Java array crosses JDBC.
    */
  private def created(origin: Origin, by: Account, label: Int)(using
      tx: Tx^
  ): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    def path(place: grit.core.place.Place): String =
      ujson.Arr.from(place.segments.map(ujson.Str(_))).render()
    attempt {
      Using.resource(
        conn.prepareStatement(
          """INSERT INTO grit.places (path)
            |SELECT DISTINCT path
            |  FROM (VALUES (ARRAY(SELECT jsonb_array_elements_text(?::jsonb))),
            |               (ARRAY(SELECT jsonb_array_elements_text(?::jsonb)))) AS wanted (path)
            |ON CONFLICT (path) DO NOTHING""".stripMargin
        )
      ) { ps =>
        ps.setString(1, path(origin.place))
        ps.setString(2, path(origin.room))
        ps.executeUpdate()
      }
      Using.resource(
        conn.prepareStatement(
          """INSERT INTO grit.conversations (origin, place_id, room_id, created_by, label_id)
            |SELECT ?::jsonb, p.id, r.id, ?, ?
            |  FROM grit.places p, grit.places r
            | WHERE p.path = ARRAY(SELECT jsonb_array_elements_text(?::jsonb))
            |   AND r.path = ARRAY(SELECT jsonb_array_elements_text(?::jsonb))
            |ON CONFLICT (origin) DO NOTHING""".stripMargin
        )
      ) { ps =>
        ps.setString(1, SqlConversationStore.originJson(origin).render())
        ps.setString(2, SqlIdentities.written(by))
        ps.setInt(3, label)
        ps.setString(4, path(origin.place))
        ps.setString(5, path(origin.room))
        ps.executeUpdate()
        ()
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
          .left
          .map(why => StoreError.DatabaseError(s"conversation ${ConversationId.value(id)}: $why"))
          .flatMap(o => SqlIdentities.read(by).map(a => Some(Conversation(id, o, a, at, label))))
    }
  }

  def remove(conversation: ConversationId)(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    // Entries, periods, their verdicts and its tool requests go by cascade. A room a document,
    // a plugin's document or a schedule was kept in, or another conversation's request writes
    // to, stays with it. A place or room another conversation took meanwhile is kept: the foreign key refuses its delete, and the caller tries again.
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
            |   AND NOT EXISTS (SELECT 1 FROM grit.documents d WHERE d.room_id = p.id)
            |   AND NOT EXISTS (SELECT 1 FROM grit.plugin_docs d WHERE d.room_id = p.id)
            |   AND NOT EXISTS (SELECT 1 FROM grit.schedules s WHERE s.room_id = p.id)
            |   AND NOT EXISTS (SELECT 1 FROM grit.tool_requests r
            |                    WHERE r.destination_id = p.id AND r.conversation_id <> ?::uuid)""".stripMargin
        )
      ) { ps =>
        ps.setString(1, ConversationId.value(conversation))
        ps.setString(2, ConversationId.value(conversation))
        ps.setString(3, ConversationId.value(conversation))
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
    case Origin.Direct(account, thread) =>
      ujson.Obj("kind" -> "direct", "account" -> Account.written(account), "thread" -> thread)
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
        case "direct" =>
          for {
            written <- str(o, "account")
            read <- Account.read(written)
            account <- Account.Sourced
              .of(read)
              .toRight(s"a direct message is with an account a source names: $written")
            thread <- str(o, "thread")
          } yield Origin.Direct(account, thread)
        case other => Left(s"unknown origin kind: $other")
      }
    } yield origin
  }
}
