package grit.dbos.engine

import java.sql.DriverManager
import java.time.Instant

import scala.util.Using

import grit.core.id.{ConversationId, EntryId, PrincipalId, TurnRef, TurnSeq}
import grit.core.place.{Directory, Place}
import grit.core.store.{Entry, Origin, Payload, StoreError, Tx}
import grit.core.visibility.{
  Clearance,
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
import grit.dbos.sql.{DbConfig, LiveDb, Opener, SqlEntryStore, TestPostgres}

import utest.*

/** What a subject resolves to as a transaction opens: its conversation's room and label, and
  * its asker's clearance, read from the rows the subject names.
  */
object OpenerLiveTests extends TestSuite {

  private lazy val config: DbConfig = {
    val c = TestPostgres.freshDatabase("opening")
    LiveEngine.open(c, "test").close()
    c
  }

  private val trial: Compartment =
    Compartment.of("trial").fold(e => throw new java.lang.AssertionError(e), identity)

  private val trialLabel: Label = Label.at(Level.Public, trial)

  private val cleared = PrincipalId("slack:T1/U-cleared")
  private val uncleared = PrincipalId("slack:T1/U-uncleared")

  private val visibility: Visibility = {
    val name = GroupName.of("trialists").fold(e => throw new java.lang.AssertionError(e), identity)
    val made = for {
      compartments <- Compartments.of(Vector(trial)).left.map(_.toString)
      v <- Visibility
        .of(
          compartments,
          RoomLabels.Public,
          Vector(Group(name, Set(cleared))),
          Vector(Grant(name, trialLabel))
        )
        .left
        .map(_.toString)
    } yield v
    made.fold(e => throw new java.lang.AssertionError(e), identity)
  }

  private val opener = new Opener(visibility)

  private def resolved(subject: Subject): Either[StoreError, Clearance] =
    LiveDb.transaction(config)(opener.clearance(subject))

  private def slack(thread: String): Origin = Origin.Slack("T1", "C1", thread)

  private def first(c: ConversationId): TurnRef = TurnRef(c, TurnSeq.First)

  /** `turn`'s first entry, grit's own: one no principal wrote. */
  private def gritSaid(turn: TurnRef): Unit =
    LiveDb.transaction(config) {
      val entries = new SqlEntryStore()
      entries.lockNext(turn.conversationId).flatMap { next =>
        entries.insert(
          Entry(
            EntryId(s"${ConversationId.value(turn.conversationId)}:posted"),
            turn.conversationId,
            turn.turnSeq,
            None,
            next.seq,
            Payload.Posted("posted"),
            Instant.parse("2026-10-07T08:00:00Z")
          )
        )
      }
    } ==> Right(())

  /** `turn`'s entries, and so their authors, purged. */
  private def purged(turn: TurnRef): Unit =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement("DELETE FROM grit.entries WHERE conversation_id = ?::uuid")
      ) { ps =>
        ps.setString(1, ConversationId.value(turn.conversationId))
        ps.executeUpdate()
      }
    }

  val tests = Tests {
    test("a turn in a labelled room asked by an uncleared person reads beyond it publicly") {
      val origin = slack("1.1")
      val turn = first(LiveDb.conversation(config, origin, trialLabel).id)
      LiveDb.asking(config, turn, uncleared, None)
      resolved(Subject.Turn(turn)) ==> Right(
        Clearance.inRoom(origin.room, trialLabel, Label.Public)
      )
    }

    test("a turn asked by a person cleared for its room's label reads at it everywhere") {
      val origin = slack("1.2")
      val turn = first(LiveDb.conversation(config, origin, trialLabel).id)
      LiveDb.asking(config, turn, cleared, None)
      val got = resolved(Subject.Turn(turn))
      got ==> Right(Clearance.inRoom(origin.room, trialLabel, trialLabel))
      assert(got.exists(_.everywhere == trialLabel))
    }

    test("a job's run reads at its conversation's label, its schedule's") {
      val origin = Origin.Task("remind", "s1:2026-10-07T08:00:00Z")
      val turn = first(LiveDb.conversation(config, origin, trialLabel).id)
      LiveDb.asking(config, turn, uncleared, None)
      resolved(Subject.Turn(turn)) ==> Right(Clearance.inRoom(origin.room, trialLabel, trialLabel))
    }

    test("a turn begun by grit's own post reads at its room's label") {
      val origin = slack("1.3")
      val turn = first(LiveDb.conversation(config, origin, trialLabel).id)
      gritSaid(turn)
      resolved(Subject.Turn(turn)) ==> Right(Clearance.inRoom(origin.room, trialLabel, trialLabel))
    }

    test("a turn whose first entry was purged has no asker, so reads beyond its room publicly") {
      val origin = slack("1.4")
      val turn = first(LiveDb.conversation(config, origin, trialLabel).id)
      LiveDb.asking(config, turn, cleared, None)
      purged(turn)
      resolved(Subject.Turn(turn)) ==> Right(
        Clearance.inRoom(origin.room, trialLabel, Label.Public)
      )
    }

    test("a turn whose conversation is gone reads only what is public") {
      val gone = TurnRef(ConversationId("01920000-0000-7000-8000-000000000000"), TurnSeq.First)
      resolved(Subject.Turn(gone)) ==> Right(Clearance.of(Label.Public))
    }

    test("a conversation's work reads at its label, in its room and beyond") {
      val origin = Origin.Tui(
        Directory.of("/tmp/opening").fold(e => throw new java.lang.AssertionError(e), identity),
        "s"
      )
      val c = LiveDb.conversation(config, origin, trialLabel).id
      resolved(Subject.Conversation(c)) ==> Right(
        Clearance.inRoom(origin.room, trialLabel, trialLabel)
      )
    }

    test("an id no conversation could have reads only what is public, as a gone one does") {
      resolved(Subject.Turn(TurnRef(ConversationId("c"), TurnSeq.First))) ==>
        Right(Clearance.of(Label.Public))
    }

    test("a gone conversation's work reads only what is public") {
      val gone = ConversationId("01920000-0000-7000-8000-000000000001")
      resolved(Subject.Conversation(gone)) ==> Right(Clearance.of(Label.Public))
    }

    test("the public subject reads only what is public") {
      resolved(Subject.Public) ==> Right(Clearance.of(Label.Public))
    }

    test("a transaction it opens labels places as the deployment's visibility does") {
      val board =
        Place.read("slack:T1/C-board").fold(e => throw new java.lang.AssertionError(e), identity)
      val labelling = RoomLabels
        .of(Vector(board -> trialLabel), Labelled.Mapped(Label.Public))
        .left
        .map(_.written)
        .flatMap(rooms =>
          Compartments
            .of(Vector(trial))
            .left
            .map(_.toString)
            .flatMap(Visibility.of(_, rooms, Vector.empty, Vector.empty).left.map(_.toString))
        )
        .fold(e => throw new java.lang.AssertionError(e), identity)
      val origin = slack("1.9")
      val turn = first(LiveDb.conversation(config, origin, trialLabel).id)
      val labelled = new Opener(labelling)
      val read = Using.resource(
        DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
      ) { conn =>
        labelled
          .open(Subject.Turn(turn), conn)
          .map(tx =>
            (
              Tx.writable(board)(using tx),
              Tx.writesTo(board)(using tx),
              Tx.writesTo(origin.room)(using tx)
            )
          )
      }
      read ==> Right((Some(trialLabel), true, false))
    }
  }
}
