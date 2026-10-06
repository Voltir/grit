package grit.core.job

import java.time.{Instant, LocalTime, ZoneId}

import scala.concurrent.duration.*

import utest.*

/** [[Due.of]]: what a schedule has due. */
object DueTests extends TestSuite {

  private def at(text: String): Instant = Instant.parse(text)
  private def grace(d: FiniteDuration): Grace =
    Grace.of(d).getOrElse(throw new java.lang.AssertionError(s"$d is a grace"))
  private val Nine = SlotRule.Daily(LocalTime.of(9, 0), ZoneId.of("UTC"))

  val tests = Tests {
    test("a slot after now is not yet due") {
      Due.of(Nine, at("2026-10-07T09:00:00Z"), at("2026-10-07T08:59:59Z")) ==> Due.NotYet
    }

    test("a once slot runs up to its grace past, the edge included, and is missed after") {
      val nine = at("2026-10-07T09:00:00Z")
      val once = SlotRule.Once(nine, grace(1.hour))
      Due.of(once, nine, nine) ==> Due.Run(nine, None)
      Due.of(once, nine, at("2026-10-07T10:00:00Z")) ==> Due.Run(nine, None)
      Due.of(once, nine, at("2026-10-07T10:00:00.001Z")) ==> Due.Missed(nine)
    }

    test("a once slot with no grace runs only at its instant") {
      val nine = at("2026-10-07T09:00:00Z")
      val once = SlotRule.Once(nine, Grace.Zero)
      Due.of(once, nine, nine) ==> Due.Run(nine, None)
      Due.of(once, nine, at("2026-10-07T09:00:00.001Z")) ==> Due.Missed(nine)
    }

    test("a recurrence runs only its latest slot at or before now, and names the one after") {
      Due.of(Nine, at("2026-10-03T09:00:00Z"), at("2026-10-06T12:00:00Z")) ==>
        Due.Run(at("2026-10-06T09:00:00Z"), Some(at("2026-10-07T09:00:00Z")))
      Due.of(Nine, at("2026-10-03T09:00:00Z"), at("2026-10-06T09:00:00Z")) ==>
        Due.Run(at("2026-10-06T09:00:00Z"), Some(at("2026-10-07T09:00:00Z")))
    }

    test("a recurrence's slot is never missed, however late") {
      Due.of(Nine, at("2026-10-06T09:00:00Z"), at("2026-10-06T23:59:00Z")) ==>
        Due.Run(at("2026-10-06T09:00:00Z"), Some(at("2026-10-07T09:00:00Z")))
      Due.of(Nine, at("2025-10-06T09:00:00Z"), at("2026-10-06T08:00:00Z")) ==>
        Due.Run(at("2026-10-05T09:00:00Z"), Some(at("2026-10-06T09:00:00Z")))
    }
  }
}
