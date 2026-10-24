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
  private def every(d: FiniteDuration): SlotRule =
    SlotRule.Every(
      Interval.of(d).getOrElse(throw new java.lang.AssertionError(s"$d is an interval"))
    )

  val tests = Tests {
    test("a grace is zero or more") {
      Grace.of(90.minutes).map(Grace.value) ==> Some(90.minutes)
      Grace.of(Duration.Zero).map(Grace.value) ==> Some(Grace.value(Grace.Zero))
      Grace.of((-1).second) ==> None
    }

    test("an interval is a minute or more") {
      Interval.of(1.minute).map(Interval.value) ==> Some(1.minute)
      Interval.of(90.seconds).map(Interval.value) ==> Some(90.seconds)
      Vector(Duration.Zero, 59.seconds, (-5).minutes).map(Interval.of) ==> Vector(None, None, None)
    }

    test("Every(5 min) after 10:02 is 10:05") {
      every(5.minutes).after(at("2026-10-07T10:02:00Z")) ==> Some(at("2026-10-07T10:05:00Z"))
      every(5.minutes).first(at("2026-10-07T10:02:00Z")) ==> Some(at("2026-10-07T10:05:00Z"))
      // Multiples since the epoch, not since the hour: `date -u` puts 10:02 two minutes past a
      // multiple of 7.
      every(7.minutes).after(at("2026-10-07T10:02:00Z")) ==> Some(at("2026-10-07T10:07:00Z"))
    }

    test("Every's next slot before the epoch is the first multiple after it") {
      every(5.minutes).after(at("1969-12-31T23:58:00Z")) ==> Some(at("1970-01-01T00:00:00Z"))
      every(5.minutes).after(at("1969-12-31T23:50:00Z")) ==> Some(at("1969-12-31T23:55:00Z"))
    }

    test("a slot exactly on a multiple is not after itself") {
      every(5.minutes).after(at("2026-10-07T10:05:00Z")) ==> Some(at("2026-10-07T10:10:00Z"))
      every(30.minutes).between(at("2026-10-07T10:00:00Z"), at("2026-10-07T11:00:00Z")).toVector ==>
        Vector(at("2026-10-07T10:30:00Z"), at("2026-10-07T11:00:00Z"))
    }

    test("a once slot is after an instant only when strictly later") {
      val once = SlotRule.Once(at("2026-10-07T09:00:00Z"), Grace.Zero)
      once.after(at("2026-10-07T08:59:59Z")) ==> Some(at("2026-10-07T09:00:00Z"))
      once.after(at("2026-10-07T09:00:00Z")) ==> None
      once.after(at("2026-10-07T10:00:00Z")) ==> None
    }

    test(
      "an ended once rule re-arms at another instant or as a recurrence; not at the same instant, a grace alone, or from a recurrence"
    ) {
      val nine = SlotRule.Once(at("2026-10-07T09:00:00Z"), Grace.Zero)
      val daily = SlotRule.Daily(LocalTime.of(9, 0), Paris)
      val longer = Grace.of(1.hour).getOrElse(sys.error("1 h"))
      Vector(
        nine.rearms(nine),
        SlotRule.Once(at("2026-10-07T11:00:00Z"), Grace.Zero).rearms(nine),
        SlotRule.Once(at("2026-10-07T09:00:00Z"), longer).rearms(nine),
        daily.rearms(nine),
        nine.rearms(daily),
        SlotRule.Weekdays(LocalTime.of(9, 0), Paris).rearms(daily)
      ) ==> Vector(false, true, false, true, false, false)
    }
    test(
      "a new schedule's first slot is a once slot's own instant, however far past; a recurrence's first after now"
    ) {
      val now = at("2026-10-07T12:00:00Z")
      SlotRule.Once(at("2026-09-01T09:00:00Z"), Grace.Zero).first(now) ==>
        Some(at("2026-09-01T09:00:00Z"))
      SlotRule.Once(at("2026-10-08T09:00:00Z"), Grace.Zero).first(now) ==>
        Some(at("2026-10-08T09:00:00Z"))
      SlotRule.Daily(LocalTime.of(9, 0), ZoneId.of("UTC")).first(now) ==>
        Some(at("2026-10-08T09:00:00Z"))
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

    test("between is the slots after its start and at or before its end, in order") {
      val nine = SlotRule.Daily(LocalTime.of(9, 0), ZoneId.of("UTC"))
      nine.between(at("2026-10-07T09:00:00Z"), at("2026-10-10T09:00:00Z")).toVector ==> Vector(
        at("2026-10-08T09:00:00Z"),
        at("2026-10-09T09:00:00Z"),
        at("2026-10-10T09:00:00Z")
      )
      val once = SlotRule.Once(at("2026-10-07T09:00:00Z"), Grace.Zero)
      once.between(at("2026-10-07T08:00:00Z"), at("2026-10-08T09:00:00Z")).toVector ==>
        Vector(at("2026-10-07T09:00:00Z"))
    }
  }
}
