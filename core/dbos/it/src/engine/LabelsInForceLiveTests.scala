package grit.dbos.engine

import java.sql.DriverManager
import java.time.Instant

import scala.util.Using

import grit.core.id.SourceId
import grit.core.identity.{Account, TestAccounts}
import grit.core.inbox.InboxError
import grit.core.message.Message
import grit.core.place.Place
import grit.core.store.{Conversation, Origin, StoreError, Tx}
import grit.core.visibility.{
  Compartment,
  Compartments,
  Grant,
  Group,
  GroupName,
  Label,
  Labelled,
  Level,
  RoomLabels,
  Subject,
  Visibility
}
import grit.dbos.internal.Reader
import grit.dbos.sql.{DbConfig, LiveDb, SqlConversationStore, SqlRooms, TestPostgres}

import utest.*

/** The labels in force are read whole as each transaction opens (`grit.rooms`,
  * `grit.group_members`): a change committed by one transaction is in force from the next,
  * and a row nothing could read refuses every transaction rather than being skipped.
  */
object LabelsInForceLiveTests extends TestSuite {

  private lazy val config: DbConfig = {
    val c = TestPostgres.freshDatabase("labels_in_force")
    LiveEngine.open(c, "test").close()
    c
  }

  private val trial: Compartment =
    Compartment.of("trial").fold(e => throw new java.lang.AssertionError(e), identity)

  private val trialists: GroupName =
    GroupName.of("trialists").fold(e => throw new java.lang.AssertionError(e), identity)

  private val internalTrial = Label.at(Level.Internal, trial)

  private val cleared = TestAccounts.account("slack:T1/U-cleared")
  private val added = TestAccounts.account("slack:T1/U-added")

  /** `trial` declared, every room public unless set, and `trialists` (only `cleared` declared)
    * granted internal·trial.
    */
  private val visibility: Visibility =
    (for {
      compartments <- Compartments.of(Vector(trial)).left.map(_.toString)
      rooms <- RoomLabels.of(Vector.empty, Labelled.Mapped(Label.Public)).left.map(_.written)
      v <- Visibility
        .of(
          compartments,
          rooms,
          Vector(Group(trialists, Set(cleared))),
          Vector(Grant(trialists, internalTrial))
        )
        .left
        .map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)

  /** `body` in a transaction of its own, opened under [[visibility]] at maintenance. */
  private def inForce[A](c: DbConfig)(body: (Tx^) ?=> A): A = LiveDb.under(c, visibility)(body)

  /** `statement` run on `c`'s database, committed, each of `binds` bound in turn. */
  private def sql(c: DbConfig, statement: String, binds: String*): Unit =
    Using.resource(DriverManager.getConnection(c.jdbcUrl, c.user, c.password)) { conn =>
      Using.resource(conn.prepareStatement(statement)) { ps =>
        binds.zipWithIndex.foreach((b, i) => ps.setString(i + 1, b))
        val _ = ps.executeUpdate()
      }
    }

  /** `room` recorded with its label set at `label`'s row, as a change through grit would. */
  private def setLabel(c: DbConfig, room: Place, label: Label): Unit =
    inForce(c) { (tx: Tx^) ?=>
      for {
        place <- grit.dbos.sql.SqlPlaces.id(room)
        id <- grit.dbos.sql.SqlLabels.intern(label)
        _ <- grit.dbos.sql.SqlEntryStore.attempt {
          val conn: java.sql.Connection^{tx} = Tx.connection(tx)
          Using.resource(
            conn.prepareStatement(
              """INSERT INTO grit.rooms (place_id, label_id) VALUES (?::uuid, ?)
                |ON CONFLICT (place_id) DO UPDATE SET label_id = EXCLUDED.label_id""".stripMargin
            )
          ) { ps =>
            ps.setString(1, place)
            ps.setInt(2, id)
            ps.executeUpdate()
          }
        }
      } yield ()
    }.fold(e => throw new java.lang.AssertionError(s"setting a label: $e"), identity)

  /** `origin`'s conversation, created at the label in force for it if new. */
  private def conversation(c: DbConfig, origin: Origin): Conversation =
    inForce(c)(
      SqlRooms
        .label(origin)
        .flatMap(new SqlConversationStore().findOrCreate(origin, Account.Local, _))
    ).fold(e => throw new java.lang.AssertionError(s"a conversation: $e"), identity)

  private def stored(c: DbConfig, conversation: Conversation): Option[Label] =
    inForce(c)(new SqlConversationStore().get(conversation.id))
      .fold(e => throw new java.lang.AssertionError(s"reading: $e"), _.map(_.label))

  val tests = Tests {
    test(
      "a label set for a room is in force from the next transaction: a conversation begun after takes it, one begun before keeps its own"
    ) {
      val before = conversation(config, Origin.Slack("T1", "C-raised", "1.0"))
      val room = before.origin.room
      setLabel(config, room, internalTrial)
      val after = conversation(config, Origin.Slack("T1", "C-raised", "2.0"))
      (stored(config, before), stored(config, after), inForce(config)(Tx.roomLabel(room))) ==>
        (Some(Label.Public), Some(internalTrial), internalTrial)
    }

    test(
      "a person added to a declared group through grit is cleared for its grant from the next transaction"
    ) {
      val person = TestAccounts.principal(added)
      val was = inForce(config)(Tx.clearanceOf(person))
      inForce(config)(grit.dbos.sql.SqlIdentities.enroll(Set(added)))
        .fold(e => throw new java.lang.AssertionError(s"enrolling: $e"), identity)
      sql(
        config,
        "INSERT INTO grit.group_members (group_name, account) VALUES (?, ?)",
        GroupName.value(trialists),
        Account.written(added)
      )
      (was, inForce(config)(Tx.clearanceOf(person))) ==> (Label.Public, internalTrial)
    }

    test(
      "a room labelled before its first conversation has its place, which removing a conversation there keeps"
    ) {
      val origin = Origin.Slack("T1", "C-early", "1.0")
      setLabel(config, origin.room, internalTrial)
      val place = inForce(config)(grit.dbos.sql.SqlPlaces.id(origin.room))
      val made = conversation(config, origin)
      val removed = inForce(config)(new SqlConversationStore().remove(made.id))
      // The same id after: the row was kept, not deleted and made again.
      val after = inForce(config)(grit.dbos.sql.SqlPlaces.id(origin.room))
      (made.label, removed, inForce(config)(Tx.roomLabel(origin.room)), after == place) ==>
        (internalTrial, Right(()), internalTrial, true)
    }

    test(
      "a room's row naming no place grit could read refuses every transaction, at each opener, and the next start"
    ) {
      val broken = TestPostgres.freshDatabase("labels_unreadable")
      val engine = LiveEngine.open(broken, "test", visibility = visibility)
      val seen =
        try {
          sql(broken, "INSERT INTO grit.places (path) VALUES ('{nowhere,x}')")
          sql(
            broken,
            "INSERT INTO grit.rooms (place_id) SELECT id FROM grit.places WHERE path = '{nowhere,x}'"
          )
          (
            engine.db.read(Subject.Public)(Right(())),
            engine.jot.write(Subject.Public)(Right(())),
            engine.inbox.ingest(
              Origin.Slack("T1", "C1", "1.0"),
              SourceId("m1"),
              Message.User("hello"),
              cleared
            ),
            engine.sweep(Instant.parse("2026-10-08T00:00:00Z")).left.toOption
          )
        } finally engine.close()
      val refused =
        StoreError.Invalid("what is recorded of rooms and groups: a place: no namespace nowhere")
      val reopened =
        scala.util.Try(LiveEngine.open(broken, "test").close()).failed.toOption.map(_.getMessage)
      val reader =
        try {
          Reader.open(broken).close()
          None
        } catch { case e: IllegalStateException => Some(e.getMessage) }
      seen ==> (Left(refused), Left(refused), Left(InboxError.stored(refused)), Some(refused))
      (reopened, reader) ==> (
        Some(s"the compartments could not be recorded: $refused"),
        Some(s"the database is unread: $refused")
      )
    }
  }
}
