package grit.core.job

import java.time.{Instant, LocalTime, ZoneId}

import scala.concurrent.duration.*

import utest.*

/** [[Starting.of]]: what a pending schedule gets when the inbox starts what it has waiting. */
object StartingTests extends TestSuite {

  private def at(text: String): Instant = Instant.parse(text)
  private val Nine = SlotRule.Daily(LocalTime.of(9, 0), ZoneId.of("UTC"))
  private val Hour = Grace.of(1.hour).getOrElse(throw new java.lang.AssertionError("1 h"))
  private val Once = SlotRule.Once(at("2026-10-07T09:00:00Z"), Hour)
  private val Due = Some(at("2026-10-07T09:00:00Z"))

  val tests = Tests {
    test("with no run in flight, a due slot starts at the current version; none due, nothing") {
      Starting.of(Once, Due, None, Some(2), at("2026-10-07T09:30:00Z")) ==>
        Starting.Start(at("2026-10-07T09:00:00Z"), None, 2, superseding = false)
      Starting.of(Once, Due, None, Some(2), at("2026-10-07T08:59:59Z")) ==> Starting.Idle
      Starting.of(Nine, None, None, Some(2), at("2026-10-07T09:30:00Z")) ==> Starting.Idle
    }

    test("a recurrence due after downtime starts only its latest slot, the next one after") {
      Starting.of(
        Nine,
        Some(at("2026-10-05T09:00:00Z")),
        Some(LastRun(at("2026-10-04T09:00:00Z"), None)),
        Some(1),
        at("2026-10-07T12:00:00Z")
      ) ==> Starting.Start(
        at("2026-10-07T09:00:00Z"),
        Some(at("2026-10-08T09:00:00Z")),
        1,
        superseding = false
      )
    }

    test(
      "a once slot past its grace is missed, with or without its job; within it, jobless waits"
    ) {
      val late = at("2026-10-07T10:00:00.000001Z")
      Starting.of(Once, Due, None, Some(1), late) ==> Starting.Miss(at("2026-10-07T09:00:00Z"))
      Starting.of(Once, Due, None, None, late) ==> Starting.Miss(at("2026-10-07T09:00:00Z"))
      Starting.of(Once, Due, None, None, at("2026-10-07T10:00:00Z")) ==> Starting.Idle
    }

    test("a run in flight is left going, restarted when its start was lost, or failed") {
      def flying(flight: InFlight): Starting =
        Starting.of(
          Once,
          None,
          Some(LastRun(at("2026-10-07T09:00:00Z"), Some(flight))),
          Some(2),
          at("2026-10-07T09:00:05Z")
        )
      Vector(InFlight.Going(2), InFlight.Replied, InFlight.Unknown, InFlight.Ended(2))
        .map(flying) ==>
        Vector(Starting.Idle, Starting.Idle, Starting.Restart, Starting.Fail)
    }

    test(
      "a run at another version is superseded by its own slot, or a recurrence's latest due; never while the job is lacking"
    ) {
      val going = Some(LastRun(at("2026-10-06T09:00:00Z"), Some(InFlight.Going(1))))
      Starting.of(
        Nine,
        Some(at("2026-10-07T09:00:00Z")),
        going,
        Some(2),
        at("2026-10-07T12:00:00Z")
      ) ==>
        Starting.Start(
          at("2026-10-07T09:00:00Z"),
          Some(at("2026-10-08T09:00:00Z")),
          2,
          superseding = true
        )
      Starting.of(
        Nine,
        Some(at("2026-10-07T09:00:00Z")),
        going,
        None,
        at("2026-10-07T12:00:00Z")
      ) ==>
        Starting.Idle
      Starting.of(
        Once,
        None,
        Some(LastRun(at("2026-10-07T09:00:00Z"), Some(InFlight.Going(1)))),
        Some(2),
        at("2026-10-08T09:00:00Z")
      ) ==> Starting.Start(at("2026-10-07T09:00:00Z"), None, 2, superseding = true)
    }

    test("a next slot not after the last run's, that run not in flight, was run: passed") {
      val ran = Some(LastRun(at("2026-10-07T09:00:00Z"), None))
      Starting.of(Once, Due, ran, Some(1), at("2026-10-07T09:30:00Z")) ==> Starting.Passed(None)
      Starting.of(Nine, Due, ran, Some(1), at("2026-10-07T09:30:00Z")) ==>
        Starting.Passed(Some(at("2026-10-08T09:00:00Z")))
    }
  }
}
