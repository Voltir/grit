package grit.core.job

import java.time.{DayOfWeek, Instant, LocalTime, ZoneId}

import scala.concurrent.duration.*

import utest.*

/** [[SlotRuleJson]] and [[ReportJson]]: a schedule row's `rule` and `report`. */
object StoredFormsTests extends TestSuite {

  private def grace(d: FiniteDuration): Grace =
    Grace.of(d).getOrElse(throw new java.lang.AssertionError(s"$d is a grace"))

  val tests = Tests {
    // Pins of the stored form: a schedule row holds it, and is read back at every pass of the
    // clock edge; a changed form would make every stored schedule unreadable.
    test("each slot rule is written in its stored form and read back") {
      val all = Vector(
        SlotRule.Once(Instant.parse("2026-10-07T09:00:00Z"), grace(1.hour)) ->
          """{"kind":"once","at":"2026-10-07T09:00:00Z","grace":"PT1H"}""",
        SlotRule.Once(Instant.parse("2026-10-07T09:00:00.250Z"), Grace.Zero) ->
          """{"kind":"once","at":"2026-10-07T09:00:00.250Z","grace":"PT0S"}""",
        SlotRule.Daily(LocalTime.of(9, 0), ZoneId.of("Europe/Paris")) ->
          """{"kind":"daily","at":"09:00","zone":"Europe/Paris"}""",
        SlotRule.Weekdays(LocalTime.of(8, 30, 15), ZoneId.of("UTC")) ->
          """{"kind":"weekdays","at":"08:30:15","zone":"UTC"}""",
        SlotRule.Weekly(DayOfWeek.WEDNESDAY, LocalTime.of(17, 0), ZoneId.of("America/New_York")) ->
          """{"kind":"weekly","day":"wednesday","at":"17:00","zone":"America/New_York"}"""
      )
      all.map((r, _) => SlotRuleJson.write(r).render()) ==> all.map(_._2)
      all.map((_, j) => SlotRuleJson.read(ujson.read(j))) ==> all.map((r, _) => Right(r))
    }

    test("a stored rule that is not one is refused, saying why") {
      val bad = Vector(
        """[]""" -> "a slot rule is not an object",
        """{"at":"09:00"}""" -> "a slot rule has no kind",
        """{"kind":"hourly"}""" -> "no slot rule hourly",
        """{"kind":"once","grace":"PT1H"}""" -> "a slot rule has no at",
        """{"kind":"once","at":"tomorrow","grace":"PT1H"}""" -> "a slot rule's at is no instant: tomorrow",
        """{"kind":"once","at":"2026-10-07T09:00:00Z","grace":"-PT1H"}""" ->
          "a slot rule's grace is no grace: -PT1H",
        """{"kind":"daily","at":"9am","zone":"UTC"}""" -> "a slot rule's at is no local time: 9am",
        """{"kind":"daily","at":"09:00","zone":"Mars/Olympus"}""" ->
          "a slot rule's zone is no zone: Mars/Olympus",
        """{"kind":"weekly","day":"someday","at":"09:00","zone":"UTC"}""" ->
          "a slot rule's day is no day: someday"
      )
      bad.map((j, _) => SlotRuleJson.read(ujson.read(j))) ==> bad.map((_, why) => Left(why))
    }

    test("each report is written in its stored form and read back") {
      val all = Vector(
        Report.Kept -> """{"kind":"kept"}""",
        Report.Posted("C123/1712.3") -> """{"kind":"posted","address":"C123/1712.3"}"""
      )
      all.map((r, _) => ReportJson.write(r).render()) ==> all.map(_._2)
      all.map((_, j) => ReportJson.read(ujson.read(j))) ==> all.map((r, _) => Right(r))
    }

    test("a stored report that is not one is refused, saying why") {
      val bad = Vector(
        """"kept"""" -> "a report is not an object",
        """{"kind":"posted"}""" -> "a report has no address",
        """{"kind":"emailed"}""" -> "no report emailed"
      )
      bad.map((j, _) => ReportJson.read(ujson.read(j))) ==> bad.map((_, why) => Left(why))
    }

    // A pin of the stored form: a schedule row's `ended` column holds the word.
    test("each ending is stored as its word and read back; no other word is one") {
      Ending.values.toVector.map(_.word) ==>
        Vector("ran", "missed", "failed", "cancelled", "undeclared")
      Ending.values.toVector.map(e => Ending.read(e.word)) ==> Ending.values.toVector.map(Some(_))
      Ending.read("Ran") ==> None
    }
  }
}
