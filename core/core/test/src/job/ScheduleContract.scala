package grit.core.job

import java.time.{Instant, LocalTime, ZoneOffset}

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.id.{
  Declarer,
  JobName,
  PluginName,
  PrincipalId,
  ScheduleId,
  ScheduleKey,
  TestCallSlots,
  TurnRef
}
import grit.core.retention.{Target, Tombstone}
import grit.core.store.{Origin, StoreError, Tombstones, Tx}

import utest.*
import JobTests.{Count, Counting}

/** The contract every [[ScheduleStore]] keeps, run against the in-memory fake in core and the
  * SQL store in grit.dbos. Each test starts from an empty store ([[ScheduleContract.fresh]]),
  * since a declaration replaces every declared schedule.
  */
abstract class ScheduleContract extends TestSuite {
  import ScheduleContract.*

  /** An empty store under test. */
  protected def fresh(): Under

  private val standup = new Counting("standup")

  private def key(text: String): ScheduleKey = got(ScheduleKey.of(text))

  private def daily(at: Int): SlotRule = SlotRule.Daily(LocalTime.of(at, 0), ZoneOffset.UTC)

  private def declared(k: String, rule: SlotRule, n: Int = 1): (Declarer, Declared[Count]) =
    (Declarer.Deployment, Declared(key(k), standup, rule, Count(n)))

  private def id(k: String): ScheduleId = ScheduleId.declared(Declarer.Deployment, key(k))

  val tests = Tests {
    test(
      "a declared schedule is kept for grit, reported in its own runs, its first slot after now; a once one at its instant, however past"
    ) {
      val u = fresh()
      val once = SlotRule.Once(at("2026-10-07T09:00:00Z"), hour)
      u.declare(Vector(declared("standup", daily(9), 1), declared("launch", once, 2)), "10:00")
      u.read(id("standup")) ==>
        Some(Schedule(standup.name, ujson.Num(1), PrincipalId.Grit, Report.Kept, daily(9), None))
      u.read(id("launch")) ==>
        Some(Schedule(standup.name, ujson.Num(2), PrincipalId.Grit, Report.Kept, once, None))
      u.read(id("never")) ==> None
      u.waiting("10:00") ==> Vector(id("launch"))
      u.waiting("2026-10-08T09:00:00Z") ==> Vector(id("launch"), id("standup"))
    }

    test(
      "a changed declaration is rewritten in place; its next slot moves only when its rule does"
    ) {
      val u = fresh()
      u.declare(Vector(declared("standup", daily(9), 1)), "10:00")
      u.declare(Vector(declared("standup", daily(9), 2)), "2026-10-08T12:00:00Z")
      u.read(id("standup")).map(_.params) ==> Some(ujson.Num(2))
      u.waiting("2026-10-08T12:00:00Z") ==> Vector(id("standup"))
      u.declare(Vector(declared("standup", daily(17), 2)), "2026-10-08T12:00:00Z")
      u.read(id("standup")).map(_.rule) ==> Some(daily(17))
      u.waiting("2026-10-08T12:00:00Z") ==> Vector()
      u.waiting("2026-10-08T17:00:00Z") ==> Vector(id("standup"))
    }

    test(
      "a declaration dropped is ended undeclared and marked for deletion, and revived when declared again; an asked one is never touched"
    ) {
      val u = fresh()
      val remind = u.desk(PluginName.of("remind").fold(sys.error, identity), Vector(standup.name))
      val turn = u.turn("contract")
      val call = TestCallSlots.at(turn)
      u.asking(turn, PrincipalId("ann"), Some("C1/1.0"))
      val asked = remind
        .ask(call, booking(standup), When.At(at("2026-10-20T09:00:00Z")), hour, Count(7))
        .fold(r => sys.error(s"$r"), _.id)
      u.declare(Vector(declared("a", daily(9)), declared("b", daily(9))), "10:00")
      u.declare(Vector(declared("a", daily(9))), "11:00")
      u.read(id("b")).flatMap(_.ended) ==> Some(Ending.Undeclared)
      u.read(asked).flatMap(_.ended) ==> None
      u.marked ==> Vector(Tombstone(Target.Schedule(id("b")), at("11:00")))
      u.waiting("2026-10-08T09:00:00Z") ==> Vector(id("a"))
      u.declare(Vector(declared("a", daily(9)), declared("b", daily(9))), "12:00")
      u.read(id("b")).flatMap(_.ended) ==> None
      u.waiting("2026-10-08T09:00:00Z") ==> Vector(id("a"), id("b"))
    }

    test(
      "a revived recurrence's next slot is its first after the revival, not one it passed while undeclared"
    ) {
      val u = fresh()
      u.declare(Vector(declared("a", daily(9)), declared("b", daily(9))), "08:00")
      u.declare(Vector(declared("a", daily(9))), "08:30")
      u.declare(Vector(declared("a", daily(9)), declared("b", daily(9))), "10:00")
      u.waiting("10:00") ==> Vector(id("a"))
      u.waiting("2026-10-08T09:00:00Z") ==> Vector(id("a"), id("b"))
    }

    test(
      "a declared slot is kept to the microsecond, truncated: due at that microsecond, never after its instant"
    ) {
      val u = fresh()
      val nine = at("2026-10-07T09:00:00Z")
      u.declare(Vector(declared("launch", SlotRule.Once(nine.plusNanos(1500), hour))), "08:00")
      u.waiting("2026-10-07T09:00:00.000001Z") ==> Vector(id("launch"))
    }

    test("a declared schedule that ran stays ended when declared again") {
      val u = fresh()
      val nine = at("2026-10-07T09:00:00Z")
      u.declare(Vector(declared("launch", SlotRule.Once(nine, hour))), "08:00")
      u.start(Slot(id("launch"), nine), 1, None)
      u.replied(Slot(id("launch"), nine), 1, "09:01")
      u.declare(Vector(declared("launch", SlotRule.Once(nine, hour))), "10:00")
      u.read(id("launch")).flatMap(_.ended) ==> Some(Ending.Ran)
    }

    test(
      "waiting lists those due and those with a run in flight, soonest first, a run's by its slot, ties by id, at most n"
    ) {
      val u = fresh()
      u.declare(
        Vector(
          declared("x", daily(7)),
          declared("y", daily(9)),
          declared("z", daily(8)),
          declared("tie-b", daily(8)),
          declared("tie-a", daily(8))
        ),
        "2026-10-07T00:00:00Z"
      )
      u.start(Slot(id("x"), at("2026-10-07T07:00:00Z")), 1, Some(at("2026-10-08T07:00:00Z")))
      u.waiting("08:30") ==> Vector(id("x"), id("tie-a"), id("tie-b"), id("z"))
      u.waiting("08:30", 2) ==> Vector(id("x"), id("tie-a"))
    }

    test(
      "a reply clears its run, and ends a once schedule ran, marked for deletion; another slot's or version's changes nothing"
    ) {
      val u = fresh()
      val nine = at("2026-10-07T09:00:00Z")
      u.declare(
        Vector(declared("launch", SlotRule.Once(nine, hour)), declared("standup", daily(9))),
        "08:00"
      )
      u.start(Slot(id("launch"), nine), 2, None)
      u.start(Slot(id("standup"), nine), 1, Some(at("2026-10-08T09:00:00Z")))
      u.replied(Slot(id("launch"), nine), 1, "09:01")
      u.replied(Slot(id("launch"), at("2026-10-07T08:00:00Z")), 2, "09:01")
      u.waiting("09:02") ==> Vector(id("launch"), id("standup"))
      u.replied(Slot(id("launch"), nine), 2, "09:03")
      u.replied(Slot(id("standup"), nine), 1, "09:03")
      u.read(id("launch")).flatMap(_.ended) ==> Some(Ending.Ran)
      u.read(id("standup")).flatMap(_.ended) ==> None
      u.marked ==> Vector(Tombstone(Target.Schedule(id("launch")), at("09:03")))
      u.waiting("09:04") ==> Vector()
    }
  }
}

object ScheduleContract {

  /** A store under test, with what a test needs beside it. */
  trait Under {
    def store: ScheduleStore

    /** Where the store marks the schedules it ends. */
    def tombstones: Tombstones

    /** Runs `body` in one transaction, committed when it returns. */
    def transaction[A](body: (Tx^) ?=> A): A

    /** `slot`'s run started at `version`, its schedule's next slot `following`, as the inbox
      * starts one.
      */
    def start(slot: Slot, version: Int, following: Option[Instant]): Unit

    /** The first turn of `origin`'s conversation, recorded or not. */
    def turn(origin: Origin): TurnRef

    /** The first turn of the Slack thread this store knows by `name`, recorded or not. */
    final def turn(name: String): TurnRef = turn(Origin.Slack("T1", "C1", name))

    /** `turn` recorded as rooted on a message `by` wrote, its reply posted at `address`, or
      * nowhere.
      */
    def asking(turn: TurnRef, by: PrincipalId, address: Option[String]): Unit

    /** `plugin`'s desk, holding the job names `jobs`, its now `clock`'s. */
    def desk(plugin: PluginName, jobs: Vector[JobName], clock: Clock^): ScheduleDesk^

    private def ok[A](e: Either[StoreError, A]): A = e.fold(err => sys.error(s"$err"), identity)

    final def declare(declared: Vector[(Declarer, Declared[?])], now: String): Unit =
      ok(transaction(store.declare(declared, at(now))))

    final def read(id: ScheduleId): Option[Schedule] = ok(transaction(store.read(id)))

    final def waiting(now: String, n: Int = 100): Vector[ScheduleId] =
      ok(transaction(store.waiting(at(now), n))).map(_._1)

    final def replied(slot: Slot, version: Int, now: String): Unit =
      ok(transaction(store.replied(slot, version, at(now))))

    /** The pending schedule tombstones, oldest first. */
    final def marked: Vector[Tombstone] =
      ok(transaction(tombstones.due(Target.Kind.Schedule, at("9999-01-01T00:00:00Z"), 1000)))

    /** `plugin`'s desk, its clock stopped at 2026-10-07T09:00:00Z. */
    final def desk(plugin: PluginName, jobs: Vector[JobName]): ScheduleDesk^ =
      desk(plugin, jobs, new grit.core.clock.SetClock(at("09:00")))
  }

  val hour: Grace = Grace.of(1.hour).getOrElse(sys.error("1 h"))

  /** `text`, an instant, or a time on 2026-10-07 (UTC). */
  private[job] def at(text: String): Instant =
    if (text.contains('T')) Instant.parse(text) else Instant.parse(s"2026-10-07T$text:00Z")

  private[job] def got[A](e: Either[String, A]): A = e.fold(sys.error, identity)

  /** `job` booked by a plugin whose only job it is. */
  def booking[P <: caps.Pure](job: Job[P]): Booking[P] =
    OwnJobs
      .over(got(PluginName.of("own")), Vector(job))
      .of(job)
      .fold(n => sys.error(s"$n"), identity)
}
