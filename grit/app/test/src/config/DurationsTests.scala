package grit.app.config

import scala.concurrent.duration.DurationInt

import utest.*

object DurationsTests extends TestSuite {

  val tests = Tests {
    test("a whole number and a unit of seconds, minutes, hours or days") {
      Vector("30s", " 3m", "24 h ", "30d").map(Durations.read) ==>
        Vector(Right(30.seconds), Right(3.minutes), Right(24.hours), Right(30.days))
    }

    test("anything else says what a duration looks like") {
      val why = "not a duration: write a whole number and s, m, h or d, as 30s or 3m"
      Vector("30", "m", "1.5h", "3w", "-3m", "").map(Durations.read) ==> Vector.fill(6)(Left(why))
    }
  }
}
