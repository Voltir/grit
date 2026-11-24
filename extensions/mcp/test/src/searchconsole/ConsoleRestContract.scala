package grit.mcp.searchconsole

import java.time.{LocalDate, OffsetDateTime}

import utest.*

/** What [[ConsoleRest]] needs of Search Console's answers for an account that is a user of one
  * site with searches today: run over [[FakeConsole]] by [[ConsoleRestRecordedTests]], and
  * against Google by [[ConsoleLive]]. Every assertion compares counts, dates and booleans, so a
  * failure prints no query and no page.
  */
trait ConsoleRestContract extends TestSuite {

  /** Search Console's API as the account reaches it. */
  protected def rest: ConsoleRest^

  /** A site the account reads, with searches in the last three days. */
  protected def site: SiteUrl

  /** Today, Pacific. */
  protected def today: LocalDate

  private def traffic(from: LocalDate, by: Grouping, rank: Rank, most: Int): Traffic =
    Dates
      .of(from, today, by, today)
      .left
      .map(why => new java.lang.AssertionError(why))
      .flatMap(dates =>
        rest
          .traffic(site, dates, None, SearchType.Web, rank, most)
          .left
          .map(e => new java.lang.AssertionError(e.message))
      )
      .fold(e => throw e, identity)

  private def falling(numbers: Vector[Long]): Boolean = numbers == numbers.sorted.reverse

  val tests = Tests {
    test("the site is among the account's sites, at a permission that reads") {
      rest.sites().map(_.find(_.url == site).map(_.permission.reads)) ==> Right(Some(true))
    }

    test(
      "a range ending today has a total for each day with data, and says from which day it is early"
    ) {
      val from = today.minusDays(2)
      val t = traffic(from, Grouping.Query, Rank.Clicks, 3)
      val dates = t.days.map(_.date)
      (
        dates.nonEmpty,
        dates.forall(d => !d.isBefore(from) && !d.isAfter(today)),
        dates == dates.sorted,
        t.early.exists(d => !d.isBefore(from) && !d.isAfter(today))
      ) ==> (true, true, true, true)
    }

    test(
      "a range's top queries by clicks are at most as many as asked, in Google's click order"
    ) {
      val t = traffic(today.minusDays(2), Grouping.Query, Rank.Clicks, 3)
      (t.rows.nonEmpty, t.rows.size <= 3, falling(t.rows.map(_.metrics.clicks))) ==>
        (true, true, true)
    }

    test("a range's top queries hold at most its days' impressions") {
      val t = traffic(today.minusDays(2), Grouping.Query, Rank.Clicks, 3)
      t.rows.map(_.metrics.impressions).sum <= t.days.map(_.metrics.impressions).sum ==> true
    }

    test("Discover by query is refused as asked") {
      val refused = Dates
        .of(today.minusDays(2), today, Grouping.Query, today)
        .left
        .map(why => new java.lang.AssertionError(why))
        .fold(e => throw e, identity)
      rest.traffic(site, refused, None, SearchType.Discover, Rank.Clicks, 3) match {
        case Left(ConsoleError.Refused(_)) => ()
        // The variant alone, so the failure prints no query.
        case Left(ConsoleError.Google(failure)) =>
          throw new java.lang.AssertionError(s"Google(${failure.productPrefix}), not Refused")
        case Right(_) => throw new java.lang.AssertionError("answered, not Refused")
      }
    }

    test("ranked by impressions, rows fall in impressions") {
      val t = traffic(today.minusDays(2), Grouping.Query, Rank.Impressions, 2)
      (t.rows.nonEmpty, t.rows.size <= 2, falling(t.rows.map(_.metrics.impressions))) ==>
        (true, true, true)
    }

    test("by date, the rows are the days ranked by clicks") {
      val t = traffic(today.minusDays(2), Grouping.Date, Rank.Clicks, 100)
      (
        t.rows.map(_.key).sorted == t.days.map(_.date.toString),
        falling(t.rows.map(_.metrics.clicks))
      ) ==> (true, true)
    }

    test("by hour, each row's key is an hour within the dates") {
      val from = today.minusDays(1)
      val t = traffic(from, Grouping.Hour, Rank.Clicks, 100)
      val days = t.rows.map(r => scala.util.Try(OffsetDateTime.parse(r.key).toLocalDate).toOption)
      (
        t.rows.nonEmpty,
        days.forall(_.exists(d => !d.isBefore(from) && !d.isAfter(today))),
        falling(t.rows.map(_.metrics.clicks))
      ) ==> (true, true, true)
    }
  }
}
