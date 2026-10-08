package grit.dbos.engine

import java.time.Instant

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.clock.SetClock
import grit.core.id.{
  ConversationId,
  EntryId,
  PluginName,
  PrincipalId,
  TestCallSlots,
  TurnRef,
  TurnSeq
}
import grit.core.identity.{Account, TestAccounts}
import grit.core.job.JobTests.{Count, Counting}
import grit.core.job.ScheduleContract.{booking, hour}
import grit.core.job.{DeskRefusal, ScheduleContract, ScheduleDesk, When}
import grit.core.store.{Origin, Tx}
import grit.core.visibility.{Clearance, Subject, TestLabels}
import grit.dbos.sql.{DbConfig, LiveDb, Opener, SqlIdentities, SqlRoomReads, TestPostgres}

import utest.*

/** A person holding two accounts, against a real Postgres: whatever was written through
  * either is theirs, for their clearance, their pending cap, their messages and their
  * schedules.
  */
object PeopleLiveTests extends TestSuite {

  private val ana = TestAccounts.account("slack:T1/U-ana")
  private val ben = TestAccounts.account("slack:T1/U-ben")

  /** A fresh database an engine has started on. */
  private def fresh(suite: String): DbConfig = {
    val c = TestPostgres.freshDatabase(suite)
    LiveEngine.open(c, "test", visibility = TestLabels.Trialled).close()
    c
  }

  /** `moved` made the person `onto` is, as a vouching links it: both kept first, and `moved`'s
    * home kept.
    */
  private def linked(config: DbConfig, moved: Account, onto: Account): Unit =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      SqlIdentities.enroll(Set(moved, onto)).fold(e => sys.error(s"enrolling: $e"), identity)
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement(
          """UPDATE grit.identities
            |   SET principal_id = (SELECT principal_id FROM grit.identities WHERE account = ?),
            |       evidence = 'vouched', email = 'person@example.com'
            | WHERE account = ?""".stripMargin
        )
      ) { ps =>
        ps.setString(1, Account.written(onto))
        ps.setString(2, Account.written(moved))
        val _ = ps.executeUpdate()
      }
    }

  private val remind = new Counting("remind")

  private def desk(u: ScheduleContract.Under): ScheduleDesk^ =
    u.desk(
      PluginName.of("reminders").fold(sys.error, identity),
      Vector(remind.name),
      new SetClock(Instant.parse("2026-10-07T09:00:00Z"))
    )

  val tests = Tests {
    test(
      "a turn asked through one account is cleared by a group naming its person's other account"
    ) {
      val config = fresh("people_cleared")
      val origin = Origin.Slack("T1", "C1", "1.0")
      val turn = TurnRef(LiveDb.conversation(config, origin, TestLabels.Trial).id, TurnSeq.First)
      LiveDb.asking(config, turn, ana, None)
      linked(config, ana, TestLabels.Trialist)
      LiveDb.transaction(config)(
        new Opener(TestLabels.Trialled).clearance(Subject.Turn(turn))
      ) ==> Right(Clearance.inRoom(origin.room, TestLabels.Trial, TestLabels.Trial))
    }

    test("two accounts of one person share the pending cap") {
      val (u, config) = SqlSchedulesUnder.withConfig("people_cap")
      val hers = u.turn("4.0")
      val his = u.turn("4.1")
      u.asking(hers, ana, Some("C1/4.0"))
      u.asking(his, ben, Some("C1/4.1"))
      linked(config, ben, ana)
      val d = desk(u)
      def ask(from: TurnRef, index: Int) =
        d.ask(
          TestCallSlots.at(from, index = index),
          booking(remind),
          When.In(1.hour),
          hour,
          Count(index)
        )
      (0 until ScheduleDesk.PendingCap).map(ask(hers, _)).collect { case Left(r) => r } ==> Vector()
      ask(his, 100) ==> Left(DeskRefusal.TooMany(ScheduleDesk.PendingCap))
    }

    test("a person's messages are those written through any of their accounts") {
      val config = fresh("people_said")
      val turns = Vector(ana -> "5.0", ben -> "5.1").map { (by, thread) =>
        val turn =
          TurnRef(LiveDb.conversation(config, Origin.Slack("T1", "C1", thread)).id, TurnSeq.First)
        LiveDb.asking(config, turn, by, None)
        turn
      }
      linked(config, ben, ana)
      val person: PrincipalId = LiveDb.principal(config, ana)
      LiveDb
        .transaction(config)(
          new SqlRoomReads().saidBy(
            Origin.Slack("T1", "C1", "5.0").room,
            person,
            Instant.parse("2026-10-07T07:00:00Z"),
            Instant.parse("2026-10-07T09:00:00Z"),
            Set.empty,
            10
          )
        )
        .map(_.map(_.entry.id).toSet) ==> Right(
        turns.map(t => EntryId(s"${ConversationId.value(t.conversationId)}:asked")).toSet
      )
    }

    test(
      "a schedule asked through one account is listed through the other in its room, and not from a room that may not read it"
    ) {
      val (u, config) = SqlSchedulesUnder.withConfig("people_listed")
      val asked = u.turn(Origin.Slack("T1", "C9", "6.0"), TestLabels.Trial)
      val same = u.turn(Origin.Slack("T1", "C9", "6.1"), TestLabels.Trial)
      val lower = u.turn(Origin.Slack("T1", "C8", "6.2"))
      u.asking(asked, ana, Some("C9/6.0"))
      u.asking(same, ben, Some("C9/6.1"))
      u.asking(lower, ben, Some("C8/6.2"))
      linked(config, ben, ana)
      val d = desk(u)
      val id = d
        .ask(TestCallSlots.at(asked), booking(remind), When.In(1.hour), hour, Count(1))
        .fold(r => sys.error(s"$r"), _.id)
      def listed(from: TurnRef) =
        d.pending(TestCallSlots.at(from, index = 1), booking(remind)).map(_.schedules.map(_.id))
      (listed(same), listed(lower)) ==> (Right(Vector(id)), Right(Vector()))
    }
  }
}
