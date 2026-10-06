package grit.job.clock

import java.time.Instant

import grit.core.clock.SetClock
import grit.core.id.{Declarer, ScheduleId, ScheduleKey, SourceId}
import grit.core.inbox.{InMemoryInbox, InboxError, Slotted}
import grit.core.job.JobTests.{Count, Counting}
import grit.core.job.ScheduleContract.hour
import grit.core.job.{Declared, Jobs, Slot, SlotRule}
import grit.core.store.{Db, StoreError, Tx}
import grit.dbos.sql.TestTx

import utest.*

object ClockEdgeTests extends TestSuite {

  private val Due = Instant.parse("2026-10-07T09:00:00Z")

  private val remind = new Counting("remind")

  private def jobs(job: Counting*): Jobs =
    Jobs.of(job.toVector).fold(n => sys.error(s"two jobs named $n"), identity)

  private object FakeDb extends Db {
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  private object DownDb extends Db {
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      Left(StoreError.DatabaseError("the database is down"))
  }

  private def key(k: String): ScheduleKey = ScheduleKey.of(k).fold(sys.error, identity)

  private def id(k: String): ScheduleId = ScheduleId.declared(Declarer.Deployment, key(k))

  /** An inbox whose schedules are `remind`'s once slots, each keyed and due as given. */
  private def inboxOf(slots: (String, Instant)*): InMemoryInbox = {
    val inbox = InMemoryInbox.fresh()
    inbox.schedules
      .declare(
        slots.toVector.map((k, at) =>
          Declarer.Deployment -> Declared(key(k), remind, SlotRule.Once(at, hour), Count(1))
        ),
        Due.minusSeconds(3600)
      )(using TestTx.fake)
      .fold(e => sys.error(s"$e"), identity)
    inbox
  }

  private def edge(inbox: InMemoryInbox, at: Instant, deployed: Jobs): ClockEdge =
    new ClockEdge(inbox, inbox.schedules, FakeDb, new SetClock(at), deployed)

  val tests = Tests {
    test(
      "a pass starts each schedule waiting at its clock's now at its job's version, and leaves one not yet due"
    ) {
      val inbox = inboxOf("due" -> Due, "later" -> Due.plusSeconds(3600))
      val ticked = edge(inbox, Due.plusSeconds(30), jobs(new Counting("remind", 2))).tick()
      ticked.map(_.slotted.map(_._1)) ==> Right(Vector(id("due")))
      ticked.map(_.slotted.collect { case (_, Slotted.Started(_, slot)) => slot }) ==>
        Right(Vector(Slot(id("due"), Due)))
      inbox.ingested(Slot(id("due"), Due).origin(remind.name), SourceId("v2")).map(_.nonEmpty) ==>
        Right(true)
    }

    test("a schedule whose job the deployment lacks is started with none: past its grace, missed") {
      val inbox = inboxOf("jobless" -> Due)
      edge(inbox, Due.plusSeconds(3601), jobs()).tick().map(_.slotted) ==>
        Right(Vector(id("jobless") -> Slotted.Missed(Slot(id("jobless"), Due))))
    }

    test(
      "a pass starts at most a batch of schedules, soonest first; a pass after their runs replied, the rest"
    ) {
      val keys = (0 to ClockEdge.Batch).map(n => f"s$n%03d").toVector
      val inbox = inboxOf(keys.map(_ -> Due)*)
      val clock = edge(inbox, Due.plusSeconds(30), jobs(remind))
      val first = clock.tick()
      first.map(_.slotted.map(_._1)) ==> Right(keys.init.map(id))
      keys.init.foreach(k =>
        inbox.schedules
          .replied(Slot(id(k), Due), 1, Due.plusSeconds(31))(using TestTx.fake)
          .fold(e => sys.error(s"$e"), identity)
      )
      clock.tick().map(_.slotted.map(_._1)) ==> Right(Vector(id(keys.last)))
    }

    test("a schedule the inbox fails is named as failed, and started by a later pass") {
      val inbox = inboxOf("flaky" -> Due)
      val clock = edge(inbox, Due.plusSeconds(30), jobs(remind))
      inbox.down = true
      clock.tick() ==> Right(
        Ticked(Vector(), Vector(id("flaky") -> InboxError.Unavailable("the database is down")))
      )
      inbox.down = false
      clock.tick().map(_.slotted.map(_._1)) ==> Right(Vector(id("flaky")))
    }

    test("a pass whose schedules cannot be read is Left, and starts nothing") {
      val inbox = inboxOf("due" -> Due)
      new ClockEdge(inbox, inbox.schedules, DownDb, new SetClock(Due), jobs(remind)).tick() ==>
        Left(StoreError.DatabaseError("the database is down"))
      inbox.started ==> Vector()
    }
  }
}
