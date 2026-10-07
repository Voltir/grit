package grit.dbos.engine

import java.time.Instant

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.document.{DocLabel, DocText, DocWeight, DocumentTerms}
import grit.core.id.{ConversationId, DocKey, EntryId, PluginName}
import grit.core.message.Message
import grit.core.place.Directory
import grit.core.store.{Entry, Origin, Payload, Tx}
import grit.core.visibility.{Clearance, Compartment, Label, Level}
import grit.dbos.sql.{
  DbConfig,
  LiveDb,
  SqlConversationStore,
  SqlDocuments,
  SqlEntryStore,
  SqlTombstones,
  TestPostgres
}

import utest.*

/** What the SQL store keeps of a conversation beyond the contract: its room by identity, which
  * goes when no conversation is in it, and the label each entry recorded in it is kept at.
  */
object ConversationRoomsLiveTests extends TestSuite {

  private lazy val config: DbConfig = {
    val c = TestPostgres.freshDatabase("conversation_rooms")
    LiveEngine.open(c, "test").close()
    c
  }

  private def directory(path: String): Directory =
    Directory.of(path).fold(e => throw new java.lang.AssertionError(e), identity)

  private val trial: Compartment =
    Compartment.of("trial").fold(e => throw new java.lang.AssertionError(e), identity)

  /** Every row `sql` reads, its columns joined by `|`. */
  private def rows(sql: String): Vector[String] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement(sql)) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val n = rs.getMetaData.getColumnCount
          val out = Vector.newBuilder[String]
          while (rs.next()) out += (1 to n).map(rs.getString).mkString("|")
          out.result()
        }
      }
    }

  /** The path of `c`'s room, as written. */
  private def room(c: ConversationId): Vector[String] =
    rows(
      s"""SELECT array_to_string(r.path, '/') FROM grit.conversations c
         |  JOIN grit.places r ON r.id = c.room_id
         | WHERE c.id = '${ConversationId.value(c)}'""".stripMargin
    )

  private def place(path: String): Vector[String] =
    rows(s"SELECT count(*) FROM grit.places WHERE path = string_to_array('$path', '/')")

  val tests = Tests {
    test("an entry is kept at the label its conversation was created at") {
      val internal = Label.at(Level.Internal, trial)
      val c = LiveDb.conversation(config, Origin.Task("rooms", "entry"), internal).id
      val entries = new SqlEntryStore()
      LiveDb.transaction(config) {
        entries.lockNext(c).flatMap { next =>
          entries.insert(
            Entry(
              EntryId(s"${ConversationId.value(c)}:hi"),
              c,
              next.turnSeq,
              None,
              next.seq,
              Payload.Message(Message.User("hi")),
              Instant.EPOCH
            )
          )
        }
      } ==> Right(())
      rows(
        s"""SELECT l.level, array_to_string(l.compartments, ',') FROM grit.entries e
           |  JOIN grit.labels l ON l.id = e.label_id
           | WHERE e.conversation_id = '${ConversationId.value(c)}'""".stripMargin
      ) ==> Vector("1|trial")
    }

    test(
      "a conversation's room is its origin's by identity: a directory's session and a nested one's are in two, a channel's threads in one"
    ) {
      val outer = LiveDb.conversation(config, Origin.Tui(directory("/rooms/a"), "s1")).id
      val inner = LiveDb.conversation(config, Origin.Tui(directory("/rooms/a/b"), "s1")).id
      val first = LiveDb.conversation(config, Origin.Slack("T", "C", "1.0")).id
      val second = LiveDb.conversation(config, Origin.Slack("T", "C", "2.0")).id
      val run = LiveDb.conversation(config, Origin.Task("remind", "s@2026-10-07T09:00:00Z")).id
      Vector(outer, inner, first, second, run).map(room) ==> Vector(
        Vector("fs/rooms/a"),
        Vector("fs/rooms/a/b"),
        Vector("slack/T/C"),
        Vector("slack/T/C"),
        Vector("task/remind")
      )
    }

    test(
      "a conversation is made in a room, or found again, while another transaction writes to one there, without waiting on it"
    ) {
      val held = LiveDb.conversation(config, Origin.Slack("T", "busy", "1.0")).id
      val entries = new SqlEntryStore()
      val conn = java.sql.DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
      try {
        conn.setAutoCommit(false)
        // Open, uncommitted, as a turn's step is: two entries recorded in the room's first
        // conversation, so its row is written twice and its references to its place and room
        // are checked, under key-share locks, the second time.
        val tx = LiveDb.Trialled.maintained(conn)
        Vector("first", "second").map { name =>
          entries.lockNext(held)(using tx).flatMap { next =>
            entries.insert(
              Entry(
                EntryId(s"${ConversationId.value(held)}:$name"),
                held,
                next.turnSeq,
                None,
                next.seq,
                Payload.Message(Message.User(name)),
                Instant.EPOCH
              )
            )(using tx)
          }
        } ==> Vector(Right(()), Right(()))
        given scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
        val made = scala.concurrent.Future {
          (
            LiveDb.conversation(config, Origin.Slack("T", "busy", "2.0")).id,
            LiveDb.conversation(config, Origin.Slack("T", "busy", "1.0")).id
          )
        }
        val both = scala.util.Try(scala.concurrent.Await.result(made, 10.seconds))
        conn.rollback()
        both.toOption.map((made, again) => (room(made), again)) ==>
          Some((Vector("slack/T/busy"), held))
      } finally conn.close()
    }

    test("a removed conversation's room goes with it once no conversation is in it") {
      val store = new SqlConversationStore()
      val first = LiveDb.conversation(config, Origin.Slack("T", "gone", "1.0")).id
      val second = LiveDb.conversation(config, Origin.Slack("T", "gone", "2.0")).id
      LiveDb.transaction(config)(store.remove(first)) ==> Right(())
      val kept = place("slack/T/gone")
      LiveDb.transaction(config)(store.remove(second)) ==> Right(())
      (kept, place("slack/T/gone"), place("slack/T/gone/2.0")) ==>
        (Vector("1"), Vector("0"), Vector("0"))
    }

    test("a room a document was kept in stays when its last conversation goes") {
      val store = new SqlConversationStore()
      val c = LiveDb.conversation(config, Origin.Slack("T", "kept", "1.0")).id
      rows(
        """INSERT INTO grit.documents (plugin, key, place, body, written_at, last_placed, room_id)
          |SELECT 'digest', 'room:kept', '{}', 'lines', now(), now(), id
          |  FROM grit.places WHERE path = '{slack,T,kept}' RETURNING version""".stripMargin
      ).size ==> 1
      LiveDb.transaction(config)(store.remove(c)) ==> Right(())
      (place("slack/T/kept"), place("slack/T/kept/1.0")) ==> (Vector("1"), Vector("0"))
    }

    test("a room a plugin's document was kept in stays when its last conversation goes") {
      val store = new SqlConversationStore()
      val c = LiveDb.conversation(config, Origin.Slack("T", "cached", "1.0")).id
      rows(
        """INSERT INTO grit.plugin_docs (plugin, generation, key, doc, source, room_id)
          |SELECT 'digest', 1, 'room:cached', '{}', 1, id
          |  FROM grit.places WHERE path = '{slack,T,cached}' RETURNING key""".stripMargin
      ).size ==> 1
      LiveDb.transaction(config)(store.remove(c)) ==> Right(())
      (place("slack/T/cached"), place("slack/T/cached/1.0")) ==> (Vector("1"), Vector("0"))
    }

    test("a key has one current document per label, and only one at each") {
      def write(label: String): Either[String, Int] =
        try
          Right(
            rows(
              s"""INSERT INTO grit.documents (plugin, key, place, body, written_at, last_placed, label_id)
                 |VALUES ('digest', 'room:labelled', '{}', 'lines', now(), now(), $label)
                 |RETURNING version""".stripMargin
            ).size
          )
        catch {
          case scala.util.control.NonFatal(e) =>
            Left(
              Option(e.getMessage)
                .filter(_.contains("idx_documents_current"))
                .fold("other")(_ => "idx_documents_current")
            )
        }
      val other = rows("SELECT grit.intern_label(ROW(1, '{trial}')::grit.label)").mkString
      (write("1"), write(other), write("1")) ==> (Right(1), Right(1), Left("idx_documents_current"))
    }

    test(
      "a document is kept in its writer's own room, and its withdrawal at the same label and room"
    ) {
      val origin = Origin.Slack("T", "withdrawn", "1.0")
      val _ = LiveDb.conversation(config, origin)
      val trialLabel = Label.at(Level.Public, trial)
      val keeper = new SqlDocuments(new SqlTombstones).keeper(
        PluginName.of("rooms").fold(e => throw new java.lang.AssertionError(e), identity),
        DocumentTerms
          .of(
            DocLabel.of("notes").fold(e => throw new java.lang.AssertionError(e), identity),
            DocWeight.Unscaled,
            1.day,
            10
          )
          .fold(e => throw new java.lang.AssertionError(e), identity)
      )
      val key = DocKey.of("k").fold(e => throw new java.lang.AssertionError(e), identity)
      val text = DocText.of("ours").fold(e => throw new java.lang.AssertionError(e), identity)
      val writer = Clearance.inRoom(origin.room, trialLabel, trialLabel)
      LiveDb.transaction(config, writer)(
        for {
          _ <- keeper.write(key, Label.Public, origin.place, text, ujson.Obj(), Instant.EPOCH)
          gone <- keeper.withdraw(key, trialLabel, Instant.EPOCH.plusSeconds(1))
        } yield gone.nonEmpty
      ) ==> Right(true)
      rows(
        s"""SELECT d.body IS NULL, l.compartments::text, array_to_string(r.path, '/')
           |  FROM grit.documents d
           |  JOIN grit.labels l ON l.id = d.label_id
           |  LEFT JOIN grit.places r ON r.id = d.room_id
           | WHERE d.plugin = 'rooms' ORDER BY d.version""".stripMargin
      ) ==> Vector("f|{trial}|slack/T/withdrawn", "t|{trial}|slack/T/withdrawn")
    }

    test(
      "the migration's room for an existing conversation is the one the store writes, for every kind of origin"
    ) {
      val origins = Vector(
        Origin.Tui(directory("/migrated/x"), "s"),
        Origin.Slack("T9", "C9", "9.0"),
        Origin.Task("remind", "declared:deployment:k@2026-10-07T09:00:00Z"),
        Origin.Task("a/b", "r/1")
      )
      origins.foreach(LiveDb.conversation(config, _))
      // The backfill given for databases made before conversations kept their room: Origin.room
      // decoded from the stored origin.
      val migrated =
        """CASE c.origin ->> 'kind'
          |  WHEN 'tui' THEN p.path
          |  WHEN 'slack' THEN ARRAY['slack'] || ARRAY(
          |    SELECT s FROM unnest(string_to_array((c.origin ->> 'team') || '/' || (c.origin ->> 'channel'), '/'))
          |      WITH ORDINALITY AS t(s, i) WHERE s <> '' ORDER BY i)
          |  WHEN 'task' THEN ARRAY['task'] || ARRAY(
          |    SELECT s FROM unnest(string_to_array(c.origin ->> 'name', '/'))
          |      WITH ORDINALITY AS t(s, i) WHERE s <> '' ORDER BY i)
          |END""".stripMargin
      rows(
        s"""SELECT count(*) FILTER (WHERE r.path IS DISTINCT FROM $migrated)
           |  FROM grit.conversations c
           |  JOIN grit.places p ON p.id = c.place_id
           |  JOIN grit.places r ON r.id = c.room_id""".stripMargin
      ) ==> Vector("0")
      rows("SELECT count(*) > 3 FROM grit.conversations") ==> Vector("t")
    }
  }
}
