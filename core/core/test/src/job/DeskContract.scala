package grit.core.job

import scala.concurrent.duration.*

import grit.core.clock.SetClock
import grit.core.id.{CallSlot, EdgeName, JobName, PluginName, ScheduleId, TestCallSlots}
import grit.core.identity.{Account, TestAccounts}
import grit.core.retention.{Target, Tombstone}
import grit.core.store.Origin
import grit.core.visibility.{Label, TestLabels}

import utest.*
import JobTests.{Count, Counting}
import ScheduleContract.{Under, at, booking, got, hour}

/** The contract every [[ScheduleDesk]] keeps, over the store it writes, run against the
  * in-memory fake in core and the SQL store in grit.dbos. Each test starts from an empty store
  * ([[DeskContract.fresh]]). The clock stands at 2026-10-07T09:00:00Z unless a test moves it.
  */
abstract class DeskContract extends TestSuite {

  /** An empty store under test. */
  protected def fresh(): Under

  private val remind = new Counting("remind")
  private val nudge = new Counting("nudge")
  private val reminders = got(PluginName.of("reminders"))
  private val nudges = got(PluginName.of("nudges"))
  private val Ann = TestAccounts.account("test:ann")
  private val Bob = TestAccounts.account("test:bob")

  /** A store under test, its clock, and the desk of the reminders plugin, owning `remind`. */
  private final class Fixture(val u: Under, val clock: SetClock, val desk: ScheduleDesk^) {

    /** The call at `index` in the first turn of `conversation`. */
    def call(conversation: String, index: Int = 0): CallSlot =
      TestCallSlots.at(u.turn(conversation), index = index)
  }

  /** A store with ann's turn `thread` posted at `C1/1.0`, bob's `other` at `C2/2.0`, ann's
    * second `later` at `C1/3.0`, and the local person's `tui` posted nowhere; `nowhere` is
    * not recorded.
    */
  private def setUp(): Fixture^ = {
    val u = fresh()
    u.asking(u.turn("thread"), Ann, Some("C1/1.0"))
    u.asking(u.turn("other"), Bob, Some("C2/2.0"))
    u.asking(u.turn("later"), Ann, Some("C1/3.0"))
    u.asking(u.turn("tui"), Account.Local, None)
    val clock = new SetClock(at("09:00"))
    new Fixture(u, clock, u.desk(reminders, Vector(remind.name), clock))
  }

  private def ask(
      desk: ScheduleDesk^,
      at: CallSlot,
      when: When,
      n: Int = 1
  ): Either[DeskRefusal, Asked[Count]] =
    desk.ask(at, booking(remind), when, hour, Count(n))

  private def asked(e: Either[DeskRefusal, Asked[Count]]): ScheduleId =
    e.fold(r => sys.error(s"$r"), _.id)

  val tests = Tests {
    test(
      "a desk writes a once slot for the asker, reported where the turn's reply is posted, under the id its call makes"
    ) {
      val f = setUp()
      import f.*
      val id = ScheduleId.asked(call("thread"))
      ask(desk, call("thread"), When.In(30.minutes), 3) ==> Right(Asked(id, at("09:30"), Count(3)))
      u.read(id) ==> Some(
        Schedule(
          remind.name,
          ujson.Num(3),
          u.principal(Ann),
          Report.Posted(Destination(EdgeName.Slack, "C1/1.0")),
          SlotRule.Once(at("09:30"), hour),
          None,
          Label.Public
        )
      )
      u.due("09:30") ==> Vector(id)
    }

    test(
      "a desk reports through the edge the asking turn's conversation came by, at its reply's address"
    ) {
      val f = setUp()
      import f.*
      val standup = u.turn(Origin.Task("standup", "2026-10-07"))
      u.asking(standup, Ann, Some("C9/9.0"))
      val asks = Vector(call("thread"), TestCallSlots.at(standup))
      asks.map(c => ask(desk, c, When.In(30.minutes)).map(_ => ())) ==> Vector(Right(()), Right(()))
      asks.map(c => u.read(ScheduleId.asked(c)).map(_.report)) ==> Vector(
        Some(Report.Posted(Destination(EdgeName.Slack, "C1/1.0"))),
        Some(Report.Posted(Destination(EdgeName.Task, "C9/9.0")))
      )
    }

    test("an instant asked is kept to the microsecond, as asked and as read") {
      val f = setUp()
      import f.*
      clock.at = at("09:00").plusNanos(123456789)
      val kept = at("09:30").plusNanos(123456000)
      val id = ScheduleId.asked(call("thread"))
      ask(desk, call("thread"), When.In(30.minutes)).map(_.at) ==> Right(kept)
      u.read(id).map(_.rule) ==> Some(SlotRule.Once(kept, hour))
      desk.pending(call("thread"), booking(remind)).map(_.schedules.map(_.at)) ==>
        Right(Vector(kept))
    }

    test("the same call asking again gets the schedule it first wrote, unchanged") {
      val f = setUp()
      import f.*
      val first = ask(desk, call("thread"), When.In(30.minutes), 3)
      clock.at = at("10:00")
      ask(desk, call("thread"), When.At(at("2026-10-09T00:00:00Z")), 9) ==> first
      u.read(ScheduleId.asked(call("thread"))).map(_.params) ==> Some(ujson.Num(3))
    }

    test(
      "a desk refuses a turn posted nowhere or unrecorded, and an instant not after now or past its horizon, writing nothing"
    ) {
      val f = setUp()
      import f.*
      val now = at("09:00")
      val limit = now.plusSeconds(366L * 24 * 3600)
      Vector(
        ask(desk, call("tui"), When.In(1.hour)),
        ask(desk, call("nowhere"), When.In(1.hour)),
        ask(desk, call("thread", 0), When.At(now)),
        ask(desk, call("thread", 1), When.At(limit.plusSeconds(1)))
      ) ==> Vector(
        Left(DeskRefusal.Unaddressed),
        Left(DeskRefusal.Unaddressed),
        Left(DeskRefusal.Past(now, now)),
        Left(DeskRefusal.TooFar(limit.plusSeconds(1), limit))
      )
      Vector(call("tui"), call("nowhere"), call("thread", 0), call("thread", 1))
        .map(c => u.read(ScheduleId.asked(c))) ==> Vector(None, None, None, None)
      ask(desk, call("thread", 2), When.At(limit)).map(_.at) ==> Right(limit)
    }

    test("a desk refuses an asker with the cap pending, of any job, until one ends") {
      val f = setUp()
      import f.*
      val other = u.desk(nudges, Vector(nudge.name), clock)
      val first = asked(ask(desk, call("thread", 0), When.In(1.hour)))
      (1 until ScheduleDesk.PendingCap - 1).foreach(i =>
        asked(ask(desk, call("thread", i), When.In(1.hour)))
      )
      val _ = other.ask(call("later"), booking(nudge), When.In(1.hour), hour, Count(1))
      ask(desk, call("thread", 99), When.In(1.hour)) ==>
        Left(DeskRefusal.TooMany(ScheduleDesk.PendingCap))
      ask(desk, call("other"), When.In(1.hour)).map(_.id) ==>
        Right(ScheduleId.asked(call("other")))
      desk.cancel(call("later", 1), booking(remind), first) ==> Right(())
      ask(desk, call("thread", 99), When.In(1.hour)).map(_.id) ==>
        Right(ScheduleId.asked(call("thread", 99)))
    }

    test("a desk refuses a booking of a job not its plugin's, writing nothing") {
      val f = setUp()
      import f.*
      val theirs = booking(nudge)
      val refused = DeskRefusal.NotOwn(reminders, nudge.name)
      desk.ask(call("thread"), theirs, When.In(1.hour), hour, Count(1)) ==> Left(refused)
      desk.pending(call("thread"), theirs) ==> Left(refused)
      desk.cancel(call("thread"), theirs, ScheduleId.asked(call("thread"))) ==> Left(refused)
      u.read(ScheduleId.asked(call("thread"))) ==> None
    }

    test(
      "pending lists the asker's own of the booking's job, soonest first, as of now; not another's, another job's, an ended one or one its job cannot read"
    ) {
      val f = setUp()
      import f.*
      val both = u.desk(reminders, Vector(remind.name, nudge.name), clock)
      val late = asked(ask(desk, call("thread", 0), When.In(3.hours), 1))
      val soon = asked(ask(desk, call("thread", 1), When.In(1.hour), 2))
      val gone = asked(ask(desk, call("thread", 2), When.In(2.hours), 3))
      val _ = asked(ask(desk, call("other"), When.In(1.hour), 4))
      val _ = both.ask(call("thread", 3), booking(nudge), When.In(1.hour), hour, Count(5))
      val _ = desk.ask(call("thread", 4), booking(Unreadable), When.In(1.hour), hour, Count(6))
      val _ = desk.cancel(call("thread"), booking(remind), gone)
      clock.at = at("09:05")
      desk.pending(call("later"), booking(remind)) ==> Right(
        Pending(
          at("09:05"),
          Vector(Asked(soon, at("10:00"), Count(2)), Asked(late, at("12:00"), Count(1)))
        )
      )
      desk.pending(call("nowhere"), booking(remind)) ==> Right(Pending(at("09:05"), Vector()))
    }

    test(
      "cancel ends the asker's pending schedule, marked for deletion; another's or another job's is not found; an ended one says how"
    ) {
      val f = setUp()
      import f.*
      val both = u.desk(reminders, Vector(remind.name, nudge.name), clock)
      val id = asked(ask(desk, call("thread"), When.In(1.hour)))
      clock.at = at("09:10")
      desk.cancel(call("other"), booking(remind), id) ==> Left(DeskRefusal.NotFound(id))
      both.cancel(call("later"), booking(nudge), id) ==> Left(DeskRefusal.NotFound(id))
      desk.cancel(call("later"), booking(remind), id) ==> Right(())
      u.read(id).flatMap(_.ended) ==> Some(Ending.Cancelled)
      u.marked ==> Vector(Tombstone(Target.Schedule(id), at("09:10")))
      desk.cancel(call("thread"), booking(remind), id) ==>
        Left(DeskRefusal.Ended(id, Ending.Cancelled))
      u.due("10:00") ==> Vector()
    }

    test("one asked in a {trial} room by a person cleared for its label is kept at it") {
      keptAt(TestLabels.Trialist, "2.1") ==> Some(TestLabels.Trial)
    }

    test(
      "one asked in a {trial} room by a person cleared only public is kept at the room's label: " +
        "its parameters are the room's"
    ) {
      keptAt(Ann, "2.2") ==> Some(TestLabels.Trial)
    }

    test(
      "pending and cancel asked from a public room neither list nor find the asker's own " +
        "asked in a {trial} room they are cleared below; asked from that room, they do"
    ) {
      val u = fresh()
      val ann = Ann
      val trial = u.turn(Origin.Slack("T1", "C9", "2.3"), TestLabels.Trial)
      val sameRoom = u.turn(Origin.Slack("T1", "C9", "2.4"), TestLabels.Trial)
      val lower = u.turn(Origin.Slack("T1", "C8", "2.5"))
      u.asking(trial, ann, Some("C9/2.3"))
      u.asking(sameRoom, ann, Some("C9/2.4"))
      u.asking(lower, ann, Some("C8/2.5"))
      val desk = u.desk(reminders, Vector(remind.name), new SetClock(at("09:00")))
      val id = asked(ask(desk, TestCallSlots.at(trial), When.In(30.minutes)))
      def listed(from: grit.core.id.TurnRef) =
        desk.pending(TestCallSlots.at(from), booking(remind)).map(_.schedules.map(_.id))
      (
        listed(lower),
        desk.cancel(TestCallSlots.at(lower), booking(remind), id),
        listed(sameRoom)
      ) ==> (Right(Vector()), Left(DeskRefusal.NotFound(id)), Right(Vector(id)))
    }
  }

  /** The label of the schedule `by` asks for from thread `thread` of a `{trial}` room. */
  private def keptAt(by: Account, thread: String): Option[Label] = {
    val u = fresh()
    val turn = u.turn(Origin.Slack("T1", "C1", thread), TestLabels.Trial)
    u.asking(turn, by, Some(s"C1/$thread"))
    val desk = u.desk(reminders, Vector(remind.name), new SetClock(at("09:00")))
    u.read(asked(ask(desk, TestCallSlots.at(turn), When.In(30.minutes)))).map(_.label)
  }

  /** `remind`, writing its parameters in a form its own reading refuses. */
  private object Unreadable extends Job[Count] {
    val name: JobName = remind.name
    val version: Int = 1
    def write(params: Count): ujson.Value = ujson.Str(s"${params.n}")
    def read(params: ujson.Value): Either[String, Count] = remind.read(params)
    def reply(run: JobRun[Count]): String = ""
  }

}
