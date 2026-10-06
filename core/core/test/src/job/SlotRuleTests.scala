package grit.core.job

import java.time.{DayOfWeek, Instant, LocalTime, ZoneId}

import scala.concurrent.duration.*

import utest.*

/** [[SlotRule.after]] and [[Grace]]: when a schedule's slots fall. Every expected instant was
  * read off `TZ=… date`, not computed by java.time.
  */
object SlotRuleTests extends TestSuite {

  private def at(text: String): Instant = Instant.parse(text)
  private val Paris = ZoneId.of("Europe/Paris")
  private val NewYork = ZoneId.of("America/New_York")
  private val Auckland = ZoneId.of("Pacific/Auckland")

  val tests = Tests {
    test("a grace is zero or more") {
      Grace.of(90.minutes).map(Grace.value) ==> Some(90.minutes)
      Grace.of(Duration.Zero).map(Grace.value) ==> Some(Grace.value(Grace.Zero))
      Grace.of((-1).second) ==> None
    }

    test("a once slot is after an instant only when strictly later") {
      val once = SlotRule.Once(at("2026-10-07T09:00:00Z"), Grace.Zero)
      once.after(at("2026-10-07T08:59:59Z")) ==> Some(at("2026-10-07T09:00:00Z"))
      once.after(at("2026-10-07T09:00:00Z")) ==> None
      once.after(at("2026-10-07T10:00:00Z")) ==> None
    }

    test("a daily slot falls at its local time in its zone, strictly after the instant") {
      val paris = SlotRule.Daily(LocalTime.of(9, 0), Paris)
      paris.after(at("2026-10-06T06:00:00Z")) ==> Some(at("2026-10-06T07:00:00Z"))
      paris.after(at("2026-10-06T07:00:00Z")) ==> Some(at("2026-10-07T07:00:00Z"))
      SlotRule.Daily(LocalTime.of(9, 0), NewYork).after(at("2026-10-06T06:00:00Z")) ==>
        Some(at("2026-10-06T13:00:00Z"))
    }

    test("a local time a clock change skips falls later by the gap's length") {
      // Paris springs forward at 02:00 on 2026-03-29: 02:30 does not exist that night.
      SlotRule.Daily(LocalTime.of(2, 30), Paris).after(at("2026-03-28T23:00:00Z")) ==>
        Some(at("2026-03-29T01:30:00Z"))
    }

    test("a local time a clock change repeats falls at its first instance only") {
      // Paris falls back at 03:00 on 2026-10-25: 02:30 CEST is 00:30Z, 02:30 CET is 01:30Z.
      val rule = SlotRule.Daily(LocalTime.of(2, 30), Paris)
      rule.after(at("2026-10-24T23:00:00Z")) ==> Some(at("2026-10-25T00:30:00Z"))
      rule.after(at("2026-10-25T00:30:00Z")) ==> Some(at("2026-10-26T01:30:00Z"))
    }

    test("a weekday slot skips the weekend, by the day in its own zone") {
      SlotRule.Weekdays(LocalTime.of(9, 0), ZoneId.of("UTC")).after(at("2026-10-09T10:00:00Z")) ==>
        Some(at("2026-10-12T09:00:00Z"))
      // 2026-10-10T18:00Z is Saturday in UTC and Sunday 07:00 in Auckland (NZDT, +13); Monday
      // 08:00 there is still Sunday in UTC.
      SlotRule.Weekdays(LocalTime.of(8, 0), Auckland).after(at("2026-10-10T18:00:00Z")) ==>
        Some(at("2026-10-11T19:00:00Z"))
      SlotRule.Weekdays(LocalTime.of(9, 0), ZoneId.of("UTC")).after(at("2026-10-06T10:00:00Z")) ==>
        Some(at("2026-10-07T09:00:00Z"))
    }

    test("a weekly slot falls on its day, a week after the last") {
      val rule = SlotRule.Weekly(DayOfWeek.WEDNESDAY, LocalTime.of(17, 0), NewYork)
      rule.after(at("2026-10-06T12:00:00Z")) ==> Some(at("2026-10-07T21:00:00Z"))
      rule.after(at("2026-10-07T21:00:00Z")) ==> Some(at("2026-10-14T21:00:00Z"))
    }
  }
}
