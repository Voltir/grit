package grit.mcp.searchconsole

import java.time.temporal.ChronoUnit
import java.time.{Instant, LocalDate, ZoneId}

/** What a property's rows are grouped by. */
enum Grouping derives CanEqual {
  case Query, Page, Country, Device, Date, Hour
}

enum Rank derives CanEqual {
  case Clicks, Impressions
}

/** Which of Google's results are counted. Discover has no queries and gives no position. */
enum SearchType derives CanEqual {
  case Web, Image, Video, News, Discover
}

/** The one filter a request may carry. */
enum Filter derives CanEqual {

  /** Rows whose `on` matches `expression` by `test`; `Equals` is case-sensitive for queries and
    * pages, `Contains` never is.
    */
  case Matching(on: Filter.On, test: Filter.Test, expression: Filter.Expression)

  /** Rows of searches Google classes as naming the brand (`branded`) or not. Google classes
    * them only from 2026-02-21, and only for a property with enough searches: for other dates,
    * or a smaller property, no rows.
    */
  case Branded(branded: Boolean)
}

object Filter {

  enum On derives CanEqual {
    case Query, Page, Country, Device
  }

  enum Test derives CanEqual {
    case Contains, Equals, NotContains
  }

  /** Text of 1 to 4096 characters. */
  opaque type Expression = String

  object Expression {

    /** `text` as an expression, or why not, in plain words. */
    def of(text: String): Either[String, Expression] =
      if (text.isEmpty) Left("A filter needs some text to match.")
      else if (text.length > Longest)
        Left(s"A filter's text is at most $Longest characters; this is ${text.length}.")
      else Right(text)

    /** Google's longest filter expression: 4096 characters. */
    private val Longest: Int = 4096

    def value(e: Expression): String = e

    given CanEqual[Expression, Expression] = CanEqual.derived
  }
}

/** Pacific days from `start` to `end` inclusive, read grouped `by`: none after today, none older
  * than 16 months, at most [[Dates.Most]] apart unless by date, none older than nine days before
  * today by hour.
  */
opaque type Dates = (LocalDate, LocalDate, Grouping)

object Dates {

  /** The most days apart a range may be, but by date: 93. */
  val Most: Int = 93

  /** The range, or why not, in plain words, today being `today`. */
  def of(
      start: LocalDate,
      end: LocalDate,
      by: Grouping,
      today: LocalDate
  ): Either[String, Dates] = {
    val earliest = today.minusMonths(Months.toLong)
    val apart = ChronoUnit.DAYS.between(start, end)
    if (start.isAfter(end)) Left(s"The start, $start, is after the end, $end.")
    else if (end.isAfter(today))
      Left(s"$end is after today, $today in Pacific time: Search Console has no later day.")
    else if (start.isBefore(earliest))
      Left(s"Search Console keeps $Months months; its earliest day is $earliest.")
    else if (by == Grouping.Hour && start.isBefore(today.minusDays(HoursBack.toLong)))
      Left(
        s"Hourly numbers go back to ${today.minusDays(HoursBack.toLong)}, nine days before today."
      )
    else if (by != Grouping.Date && apart > Most)
      Left(
        s"Grouped by ${by.toString.toLowerCase}, at most $Most days apart can be read at once; " +
          s"$start to $end are $apart."
      )
    else Right((start, end, by))
  }

  /** How many months back Search Console keeps: 16. */
  private val Months: Int = 16

  /** How many days before today hourly numbers go back: 9, Google's "up to 10 days" read so as
    * never to ask for a day it may lack.
    */
  private val HoursBack: Int = 9

  /** The Pacific date at `now`: the day Search Console counts it in. */
  def today(now: Instant): LocalDate = LocalDate.ofInstant(now, Pacific)

  private val Pacific: ZoneId = ZoneId.of("America/Los_Angeles")

  extension (d: Dates) {
    def start: LocalDate = d._1
    def end: LocalDate = d._2
    def by: Grouping = d._3
  }

  given CanEqual[Dates, Dates] = CanEqual.derived
}

/** Search numbers: `position` is the mean rank, absent where Google gives none (Discover). */
final case class Metrics(clicks: Long, impressions: Long, ctr: Double, position: Option[Double])
    derives CanEqual

final case class Day(date: LocalDate, metrics: Metrics) derives CanEqual

/** One row: its key under the grouping asked (an hour as an ISO offset date-time), and its
  * numbers.
  */
final case class Row(key: String, metrics: Metrics) derives CanEqual

/** A site's traffic over some dates: each day's totals with data (rare queries included,
  * filtered as the rows were); the first day whose numbers may still change; the top rows; how
  * many rows were asked of Google; and whether those rows were re-ranked from a read of
  * Google's click order that came back full, so a row not read may rank above them.
  */
final case class Traffic(
    days: Vector[Day],
    early: Option[LocalDate],
    rows: Vector[Row],
    asked: Int,
    reranked: Boolean
) derives CanEqual

/** Why Search Console could not be read. */
enum ConsoleError derives CanEqual {
  case NoToken(why: String)

  /** The account may not read the site, or it does not exist (403, 404). */
  case Hidden

  /** Search Console's API is not enabled in the account's Google Cloud project. */
  case Off

  /** Google refused the question as asked (400): `reasons`, Google's. */
  case Refused(reasons: Vector[String])

  case Busy(status: Int)
  case Unreachable(why: String)
  case Malformed(why: String)

  /** What the model is told. */
  def message: String = this match {
    case NoToken(_) => "Search Console did not accept grit's access. Try again later."
    case Hidden => "grit cannot read that property's Search Console data."
    case Off =>
      "Search Console's API is not turned on in the Google Cloud project grit's service " +
        "account belongs to. Someone who administers that project can turn on the Google " +
        "Search Console API in its APIs & Services page; grit can read the site once it is on."
    case Refused(_) =>
      "Search Console refused that question as asked (Discover has no queries, for one). " +
        "Try another grouping, filter or search type."
    case Busy(_) => "Search Console is busy. Try again in a minute."
    case Unreachable(_) => "Search Console could not be reached. Try again later."
    case Malformed(_) => "Search Console answered in a way grit could not read."
  }

  /** What the operator's log is told, beside the model's words. */
  def logged: Option[String] = this match {
    case NoToken(why) => Some(why)
    case Hidden => None
    case Off => Some("the Search Console API is not enabled in the account's project")
    case Refused(reasons) => Some(s"Search Console answered 400: ${reasons.mkString(", ")}")
    case Busy(status) => Some(s"Search Console answered $status")
    case Unreachable(why) => Some(why)
    case Malformed(why) => Some(why)
  }
}
