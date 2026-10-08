package grit.kit.run

import java.time.Instant

import scala.concurrent.duration.DurationInt

import grit.core.clock.SetClock

import utest.*

/** [[Kit.cadence]]: when the serving loop's looks and pick rounds are due, by its clock. */
object CadenceTests extends TestSuite {

  val tests = Tests {
    test("a round is due at once, then again only once its period has passed by the clock") {
      val start = Instant.parse("2030-01-01T00:00:00Z")
      val clock = new SetClock(start)
      val due = Kit.cadence(1.minute, clock)
      val first = due()
      clock.at = start.plusSeconds(59)
      val early = due()
      clock.at = start.plusSeconds(60)
      val next = due()
      (first, early, next) ==> (Some(start), None, Some(start.plusSeconds(60)))
    }
  }
}
