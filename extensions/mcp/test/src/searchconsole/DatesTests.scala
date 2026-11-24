package grit.mcp.searchconsole

import java.time.{Instant, LocalDate}

import utest.*

/** [[Dates]]' refusals, each at its edge, and the Pacific day an instant falls on. */
object DatesTests extends TestSuite {

  private val today = LocalDate.parse("2026-11-24")

  private def of(
      start: LocalDate,
      end: LocalDate,
      by: Grouping
  ): Either[String, (LocalDate, LocalDate)] =
    Dates.of(start, end, by, today).map(d => (d.start, d.end))

  val tests = Tests {
    test("a start after the end is refused, naming both") {
      of(today, today.minusDays(1), Grouping.Query) ==>
        Left("The start, 2026-11-24, is after the end, 2026-11-23.")
    }

    test("an end after today is refused, naming today") {
      of(today, today.plusDays(1), Grouping.Query) ==>
        Left(
          "2026-11-25 is after today, 2026-11-24 in Pacific time: Search Console has no later day."
        )
    }

    test("by query, 93 days apart are taken and 94 refused") {
      val end = today.minusDays(1)
      (
        of(end.minusDays(93), end, Grouping.Query),
        of(end.minusDays(94), end, Grouping.Query)
      ) ==> (
        Right((end.minusDays(93), end)),
        Left(
          "Grouped by query, at most 93 days apart can be read at once; 2026-08-21 to 2026-11-23 are 94."
        )
      )
    }

    test("by date, sixteen months are taken and a day more refused, by any grouping") {
      val earliest = today.minusMonths(16)
      (
        of(earliest, today, Grouping.Date),
        of(earliest.minusDays(1), today, Grouping.Date),
        of(earliest.minusDays(1), earliest.minusDays(1), Grouping.Device)
      ) ==> (
        Right((earliest, today)),
        Left("Search Console keeps 16 months; its earliest day is 2025-07-24."),
        Left("Search Console keeps 16 months; its earliest day is 2025-07-24.")
      )
    }

    test("by hour, nine days before today is taken and ten refused") {
      (
        of(today.minusDays(9), today, Grouping.Hour),
        of(today.minusDays(10), today, Grouping.Hour)
      ) ==> (
        Right((today.minusDays(9), today)),
        Left("Hourly numbers go back to 2026-11-15, nine days before today.")
      )
    }

    test("today is the Pacific date: 07:59 UTC is still yesterday") {
      (
        Dates.today(Instant.parse("2026-11-24T07:59:00Z")),
        Dates.today(Instant.parse("2026-11-24T08:00:00Z"))
      ) ==> (LocalDate.parse("2026-11-23"), today)
    }
  }
}
