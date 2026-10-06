package grit.dbos.engine

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{BrokenBarrierException, CyclicBarrier, TimeUnit, TimeoutException}

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

import grit.core.clock.SetClock
import grit.core.id.{PluginName, PrincipalId, ScheduleId, TestCallSlots}
import grit.core.job.JobTests.{Count, Counting}
import grit.core.job.ScheduleContract.{booking, hour}
import grit.core.job.{Asked, DeskRefusal, ScheduleDesk, When}

import utest.*

/** What only the SQL desk shows: its cap holds between asks whose transactions run at once. */
object SqlDeskCapTests extends TestSuite {

  private val remind = new Counting("remind")

  val tests = Tests {
    test(
      "two asks at once, with one short of the cap pending, write one schedule and refuse the other"
    ) {
      // Once armed, each desk transaction waits before it commits for another to arrive, so two
      // asks that are not serialised both pass the cap's count before either commits. Asks
      // serialised by the asker's lock never meet: the first gives up waiting and commits, and
      // the second then counts its schedule.
      val armed = new AtomicBoolean(false)
      val barrier = new CyclicBarrier(2)
      def meet(): Unit =
        if (armed.get())
          try { val _ = barrier.await(3, TimeUnit.SECONDS) }
          catch { case _: TimeoutException | _: BrokenBarrierException => () }
      val u = SqlSchedulesUnder("sql_desk_cap", () => meet())
      val thread = u.turn("thread")
      u.asking(thread, PrincipalId("ann"), Some("C1/1.0"))
      val desk = u.desk(
        PluginName.of("reminders").fold(sys.error, identity),
        Vector(remind.name),
        new SetClock(java.time.Instant.parse("2026-10-07T09:00:00Z"))
      )
      def ask(index: Int): Either[DeskRefusal, Asked[Count]] =
        desk.ask(
          TestCallSlots.at(thread, index = index),
          booking(remind),
          When.In(1.hour),
          hour,
          Count(index)
        )
      (0 until ScheduleDesk.PendingCap - 1).map(ask).collect { case Left(r) => r } ==> Vector()
      armed.set(true)
      given ExecutionContext = ExecutionContext.global
      val both = Future.sequence(Vector(100, 101).map(i => Future(ask(i))))
      val outcomes = Await.result(both, 30.seconds).map(_.map(_.id))
      val asked = Vector(100, 101).map(i => ScheduleId.asked(TestCallSlots.at(thread, index = i)))
      (
        outcomes.collect { case Right(id) => asked.contains(id) },
        outcomes.collect { case Left(r) =>
          r
        }
      ) ==> (Vector(true), Vector(DeskRefusal.TooMany(ScheduleDesk.PendingCap)))
      armed.set(false)
      desk.pending(TestCallSlots.at(thread), booking(remind)).map(_.schedules.size) ==>
        Right(ScheduleDesk.PendingCap)
    }
  }
}
