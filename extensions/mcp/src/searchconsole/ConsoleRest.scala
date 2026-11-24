package grit.mcp.searchconsole

import java.net.{URI, URLEncoder}
import java.nio.charset.StandardCharsets.UTF_8
import java.time.LocalDate
import java.time.temporal.ChronoUnit

import grit.core.clock.Clock
import grit.mcp.client.Authorizer
import grit.mcp.google.{GoogleFailure, GoogleRequest, GoogleRest}

/** Search Console's v3 API at `api`, read-only, as `authorizer` authorises at `clock`'s time;
  * ranked by impressions, at most `fetched` rows are read to re-rank.
  */
final class ConsoleRest(
    api: URI,
    authorizer: Authorizer,
    clock: Clock,
    fetched: Int = ConsoleRest.Fetched
) {

  private val google = new GoogleRest[ConsoleError](authorizer, clock, ConsoleRest.refined)

  /** The properties the account is a user of; none when it is a user of none. A property Google
    * names in a form [[SiteUrl]] does not take is left out, and a permission Google names
    * beyond its four is read as [[Permission.Unverified]].
    */
  def sites(): Either[ConsoleError, Vector[Site]] =
    failed(google.json(GoogleRequest(ConsoleRest.Name, api, "sites"))).flatMap(ConsoleRest.sitesOf)

  /** `site`'s traffic over `dates` grouped as they say, fresh data included, `search`'s results
    * alone, each count filtered by `filter`: its top `most` rows (cut to 1..`fetched`) by `rank`.
    */
  def traffic(
      site: SiteUrl,
      dates: Dates,
      filter: Option[Filter],
      search: SearchType,
      rank: Rank,
      most: Int
  ): Either[ConsoleError, Traffic] = {
    val path = s"sites/${URLEncoder.encode(SiteUrl.value(site), UTF_8)}/searchAnalytics/query"
    val days = ChronoUnit.DAYS.between(dates.start, dates.end).toInt + 1
    val wanted = most.max(1).min(fetched)
    // By date and by hour every row is read and ranked here: Google orders days by date, and
    // documents no order for hours. Otherwise Google ranks by clicks alone.
    val whole = dates.by == Grouping.Date || dates.by == Grouping.Hour
    val asked =
      if (dates.by == Grouping.Date) days
      else if (dates.by == Grouping.Hour) days * ConsoleRest.HoursADay
      else if (rank == Rank.Impressions) fetched
      else wanted
    def ask(by: Grouping, limit: Int): Either[ConsoleError, ujson.Value] =
      failed(
        google.json(
          GoogleRequest(
            ConsoleRest.Name,
            api,
            path,
            json = Some(ConsoleRest.query(dates, by, filter, search, limit))
          )
        )
      )
    for {
      byDate <- ask(Grouping.Date, days)
      dayRows <- ConsoleRest.rowsOf(byDate)
      totals <- ConsoleRest.daysOf(dayRows)
      early <- ConsoleRest.earlyOf(byDate)
      grouped <-
        if (dates.by == Grouping.Date) Right(dayRows)
        else ask(dates.by, asked).flatMap(ConsoleRest.rowsOf)
    } yield {
      val ranked =
        if (rank == Rank.Impressions)
          grouped.sortBy(r => (-r.metrics.impressions, -r.metrics.clicks))
        else if (whole) grouped.sortBy(r => (-r.metrics.clicks, -r.metrics.impressions))
        else grouped
      Traffic(
        days = totals,
        early = early,
        rows = ranked.take(wanted),
        asked = asked,
        reranked = rank == Rank.Impressions && !whole && grouped.size >= asked
      )
    }
  }

  /** `answer`'s failure as Search Console's. */
  private def failed[A](answer: Either[GoogleFailure | ConsoleError, A]): Either[ConsoleError, A] =
    answer.left.map {
      case shared: GoogleFailure => ConsoleError.Google(shared)
      case own: ConsoleError => own
    }
}

object ConsoleRest {

  /** Search Console's v3 API. */
  val Api: String = "https://www.googleapis.com/webmasters/v3/"

  /** The scope grit's token asks for: reading every property the account is a user of. */
  val ReadOnly: Set[String] = Set("https://www.googleapis.com/auth/webmasters.readonly")

  /** The most rows read to re-rank by impressions: 1,000, Google's default page. */
  val Fetched: Int = 1000

  /** The API's name in a failure's words. */
  private val Name: String = "Search Console"

  /** The most hourly rows a day may have: 25, for the day the clocks go back. */
  private val HoursADay: Int = 25

  /** Search Console's own reading of a refusal, before the table its APIs share. */
  private val refined: (Int, Vector[String]) -> Option[ConsoleError] = (status, reasons) =>
    Option.when(status == 400)(ConsoleError.Refused(reasons))

  /** An answer of a shape this reader does not read: `why`. */
  private def malformed(why: String): ConsoleError =
    ConsoleError.Google(GoogleFailure.Malformed(why))

  /** The body of a query over `dates` grouped `by`, its first `limit` rows. */
  private[searchconsole] def query(
      dates: Dates,
      by: Grouping,
      filter: Option[Filter],
      search: SearchType,
      limit: Int
  ): ujson.Obj = {
    val body = ujson.Obj(
      "startDate" -> dates.start.toString,
      "endDate" -> dates.end.toString,
      "dimensions" -> ujson.Arr(dimension(by)),
      "type" -> searchKey(search),
      // Fresh data, and by hour the hourly breakdown, which Google requires for it.
      "dataState" -> (if (by == Grouping.Hour) "hourly_all" else "all"),
      "rowLimit" -> limit
    )
    filter.foreach(f =>
      body("dimensionFilterGroups") =
        ujson.Arr(ujson.Obj("groupType" -> "and", "filters" -> ujson.Arr(filterOf(f))))
    )
    body
  }

  private def dimension(by: Grouping): String = by match {
    case Grouping.Query => "query"
    case Grouping.Page => "page"
    case Grouping.Country => "country"
    case Grouping.Device => "device"
    case Grouping.Date => "date"
    case Grouping.Hour => "hour"
  }

  private def searchKey(search: SearchType): String = search match {
    case SearchType.Web => "web"
    case SearchType.Image => "image"
    case SearchType.Video => "video"
    case SearchType.News => "news"
    case SearchType.Discover => "discover"
  }

  private def filterOf(filter: Filter): ujson.Obj = filter match {
    case Filter.Matching(on, test, expression) =>
      ujson.Obj(
        "dimension" -> (on match {
          case Filter.On.Query => "query"
          case Filter.On.Page => "page"
          case Filter.On.Country => "country"
          case Filter.On.Device => "device"
        }),
        "operator" -> (test match {
          case Filter.Test.Contains => "contains"
          case Filter.Test.Equals => "equals"
          case Filter.Test.NotContains => "notContains"
        }),
        "expression" -> Filter.Expression.value(expression)
      )
    case Filter.Branded(branded) =>
      ujson.Obj(
        "dimension" -> "brandedClassification",
        "operator" -> "equals",
        "expression" -> (if (branded) "BRANDED" else "NON_BRANDED")
      )
  }

  private def sitesOf(v: ujson.Value): Either[ConsoleError, Vector[Site]] =
    v.objOpt.toRight(ConsoleRest.malformed("sites: not an object")).flatMap { o =>
      // An account that is a user of no property is answered {}, with no list at all.
      o.get("siteEntry").fold(Right(Vector.empty)) { entries =>
        entries.arrOpt
          .toRight(ConsoleRest.malformed("siteEntry: not a list"))
          .map(_.toVector.flatMap { e =>
            val entry = e.objOpt
            entry
              .flatMap(_.get("siteUrl"))
              .flatMap(_.strOpt)
              .flatMap(raw => SiteUrl.of(raw).toOption)
              .map { url =>
                val level = entry.flatMap(_.get("permissionLevel")).flatMap(_.strOpt)
                Site(url, level.fold(Permission.Unverified)(permission))
              }
          })
      }
    }

  private def permission(level: String): Permission = level match {
    case "siteOwner" => Permission.Owner
    case "siteFullUser" => Permission.Full
    case "siteRestrictedUser" => Permission.Restricted
    case _ => Permission.Unverified
  }

  private def rowsOf(v: ujson.Value): Either[ConsoleError, Vector[Row]] =
    v.objOpt.flatMap(_.get("rows")) match {
      // A range with no data is answered no rows at all.
      case None => Right(Vector.empty)
      case Some(rows) =>
        rows.arrOpt
          .toRight(ConsoleRest.malformed("rows: not a list"))
          .flatMap(_.toVector.foldLeft[Either[ConsoleError, Vector[Row]]](Right(Vector.empty)) {
            (acc, r) => acc.flatMap(kept => rowOf(r).map(kept :+ _))
          })
    }

  private def rowOf(r: ujson.Value): Either[ConsoleError, Row] = {
    val o = r.objOpt
    def num(k: String) = o.flatMap(_.get(k)).flatMap(_.numOpt)
    (for {
      key <- o.flatMap(_.get("keys")).flatMap(_.arrOpt).flatMap(_.headOption).flatMap(_.strOpt)
      clicks <- num("clicks")
      impressions <- num("impressions")
      ctr <- num("ctr")
    } yield Row(key, Metrics(clicks.toLong, impressions.toLong, ctr, num("position"))))
      .toRight(ConsoleRest.malformed("a row without keys, clicks, impressions or ctr"))
  }

  private def daysOf(rows: Vector[Row]): Either[ConsoleError, Vector[Day]] =
    rows.foldLeft[Either[ConsoleError, Vector[Day]]](Right(Vector.empty)) { (acc, r) =>
      acc.flatMap(kept =>
        scala.util
          .Try(LocalDate.parse(r.key))
          .toOption
          .toRight(ConsoleRest.malformed("a by-date row whose key is not a date"))
          .map(d => kept :+ Day(d, r.metrics))
      )
    }

  private def earlyOf(v: ujson.Value): Either[ConsoleError, Option[LocalDate]] =
    v.objOpt
      .flatMap(_.get("metadata"))
      .flatMap(_.objOpt)
      .flatMap(_.get("firstIncompleteDate")) match {
      case None => Right(None)
      case Some(d) =>
        d.strOpt
          .flatMap(s => scala.util.Try(LocalDate.parse(s)).toOption)
          .map(Some(_))
          .toRight(ConsoleRest.malformed("firstIncompleteDate: not a date"))
    }
}
