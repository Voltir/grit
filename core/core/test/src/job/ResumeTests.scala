package grit.core.job

import java.time.{Instant, LocalTime, ZoneId}

import scala.concurrent.duration.*

import utest.*

/** [[Resume.of]]: what a schedule with a run in flight gets. */
object ResumeTests extends TestSuite {

  private def at(text: String): Instant = Instant.parse(text)
  private val Nine = SlotRule.Daily(LocalTime.of(9, 0), ZoneId.of("UTC"))
  private val Hour = Grace.of(1.hour).getOrElse(throw new java.lang.AssertionError("1 h"))

  // A recurrence's run started for its 6 October slot, its next slot the 7th, read at noon.
  private def daily(flight: InFlight, current: Option[Int]): Resume =
    Resume.of(
      flight,
      current,
      Nine,
      at("2026-10-06T09:00:00Z"),
      Some(at("2026-10-07T09:00:00Z")),
      at("2026-10-06T12:00:00Z")
    )

  val tests = Tests {
    test("a run that ended without a reply fails, and one whose start was lost is enqueued") {
      daily(InFlight.Failed, Some(1)) ==> Resume.Fail
      daily(InFlight.Unknown, Some(1)) ==> Resume.Enqueue
    }

    // A run whose job left the deployment ends cleanly, with no reply: still failed, so a once
    // schedule ends and a recurrence goes on, rather than waiting on it for ever.
    test("a run that ended without a reply fails whether or not the deployment has its job") {
      daily(InFlight.Failed, None) ==> Resume.Fail
    }

    test("a run that replied, or is going at the current version, is left") {
      daily(InFlight.Replied, Some(2)) ==> Resume.Leave
      daily(InFlight.Going(2), Some(2)) ==> Resume.Leave
    }

    test("a run going while the deployment lacks its job is left: there is no version to start") {
      daily(InFlight.Going(1), None) ==> Resume.Leave
    }

    // Wisdom's (a): the slot started in time, so it runs again at the new version, however long
    // past its grace it is now.
    test("a once slot started in time at another version is superseded by itself, past its grace") {
      val nine = at("2026-10-07T09:00:00Z")
      Resume.of(
        InFlight.Going(1),
        Some(2),
        SlotRule.Once(nine, Hour),
        nine,
        None,
        at("2026-10-07T12:00:00Z")
      ) ==> Resume.Supersede(nine, None)
    }

    test("a recurrence at another version with no later slot due supersedes the same slot") {
      daily(InFlight.Going(1), Some(2)) ==>
        Resume.Supersede(at("2026-10-06T09:00:00Z"), Some(at("2026-10-07T09:00:00Z")))
    }

    // Wisdom's (b): S1 ran at v1, grit was down through S2 and S3, and is up at v2.
    test("a recurrence at another version with later slots due supersedes to the latest") {
      Resume.of(
        InFlight.Going(1),
        Some(2),
        Nine,
        at("2026-10-04T09:00:00Z"),
        Some(at("2026-10-05T09:00:00Z")),
        at("2026-10-06T12:00:00Z")
      ) ==> Resume.Supersede(at("2026-10-06T09:00:00Z"), Some(at("2026-10-07T09:00:00Z")))
    }
  }
}
