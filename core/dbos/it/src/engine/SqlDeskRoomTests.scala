package grit.dbos.engine

import java.sql.DriverManager

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.clock.SetClock
import grit.core.id.{PluginName, PrincipalId, ScheduleId, TestCallSlots}
import grit.core.job.JobTests.{Count, Counting}
import grit.core.job.{ScheduleContract, When}
import grit.core.store.Origin
import grit.core.visibility.TestLabels

import utest.*

/** What decides who reads an asked schedule in the SQL store: the room it was kept in, never
  * the call's key it was asked from.
  */
object SqlDeskRoomTests extends TestSuite {

  val tests = Tests {
    test(
      "an asked schedule is read where its room is, whatever conversation its key names"
    ) {
      val (u, config) = SqlSchedulesUnder.withConfig("sql_desk_room")
      val ann = PrincipalId("ann")
      val remind = new Counting("remind")
      val trial = u.turn(Origin.Slack("T1", "C9", "2.3"), TestLabels.Trial)
      val lower = u.turn(Origin.Slack("T1", "C8", "2.5"))
      u.asking(trial, ann, Some("C9/2.3"))
      u.asking(lower, ann, Some("C8/2.5"))
      val desk = u.desk(
        PluginName.of("reminders").fold(sys.error, identity),
        Vector(remind.name),
        new SetClock(java.time.Instant.parse("2026-10-07T09:00:00Z"))
      )
      val id = desk
        .ask(
          TestCallSlots.at(trial),
          ScheduleContract.booking(remind),
          When.In(30.minutes),
          ScheduleContract.hour,
          Count(1)
        )
        .fold(r => sys.error(s"$r"), _.id)
      // The key rewritten to name the public room's conversation's call.
      Using.resource(DriverManager.getConnection(config.jdbcUrl, config.user, config.password)) {
        conn =>
          Using.resource(
            conn.prepareStatement("UPDATE grit.schedules SET asked_in = ? WHERE id = ?")
          ) { ps =>
            ps.setString(1, TestCallSlots.at(lower, index = 7).key)
            ps.setString(2, ScheduleId.value(id))
            val _ = ps.executeUpdate()
          }
      }
      def listed(from: grit.core.id.TurnRef) =
        desk
          .pending(TestCallSlots.at(from, index = 1), ScheduleContract.booking(remind))
          .map(_.schedules.map(_.id))
      (listed(trial), listed(lower)) ==> (Right(Vector(id)), Right(Vector()))
    }
  }
}
