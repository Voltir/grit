package grit.mcp.searchconsole

import java.nio.charset.StandardCharsets.UTF_8

import grit.core.clock.{Clock, SetClock}
import grit.mcp.google.FakeGoogleServer

/** Search Console's v3 API over a [[FakeGoogleServer]], as its reference says it answers:
  *
  *   - `sites` with `sites-none` (Google's answer to an account that is a user of no property)
  *     until [[sharing]], then `made-sites`;
  *   - a query of [[FakeConsole.Sites]]' sites, path-encoded, grouped by date with
  *     `made-by-date`'s rows within the dates, oldest first, and its `firstIncompleteDate` only
  *     when the data state is `all` and the date is within them; by query, under any search
  *     type but Discover, with `made-by-query`, by hour (`hourly_all` alone) with `made-by-hour`'s hours within the
  *     dates, and by page under Discover with `made-discover-by-page`; by page, country or
  *     device with the rows of its search type's fixture (Discover's or `made-by-query`) given
  *     that grouping's keys in order, as many rows as it has keys; each cut to its `rowLimit`;
  *     Discover by query, or any other question, 400 `badRequest`.
  *
  * Filters are carried, not applied: a request's body is what a test reads of them.
  */
final class FakeConsole private (google: FakeGoogleServer) {

  google.answering("sites", 200, FakeConsole.text("sites-none"))
  FakeConsole.Sites.foreach(site =>
    google.responding(FakeConsole.queryPath(site))(r => FakeConsole.queried(ujson.read(r.body)))
  )

  /** A reader over this fake, reading `clock`, re-ranking at most `fetched` rows. */
  def rest(clock: Clock, fetched: Int = ConsoleRest.Fetched): ConsoleRest^{clock} =
    new ConsoleRest(google.base, FakeGoogleServer.authorizer(), clock, fetched)

  /** From now, `sites` answers `made-sites`. */
  def sharing(): Unit = google.answering("sites", 200, FakeConsole.text("made-sites"))

  /** The API's base, as [[ConsoleEdge.serving]] takes it. */
  def api: java.net.URI = google.base

  /** From now, `sites` is answered `status` with `body`. */
  def answeringSites(status: Int, body: String): Unit = google.answering("sites", status, body)

  /** From now, every query of `site` is answered `status` with `body`. */
  def failing(site: SiteUrl, status: Int, body: String): Unit =
    google.answering(FakeConsole.queryPath(site), status, body)

  /** The raw paths requested, in order. */
  def paths: Vector[String] = google.requests.map(_.path)

  /** Every query's body, in the order received. */
  def queries: Vector[ujson.Value] =
    google.requests.filter(_.method == "POST").map(r => ujson.read(r.body))

  def stop(): Unit = google.stop()
}

object FakeConsole {

  def start(): FakeConsole = new FakeConsole(FakeGoogleServer.start())

  /** Noon in Pacific time on 2026-11-24, the last day the fixtures hold: a clock their answers
    * stay true at, whatever the real date.
    */
  def clock(): SetClock = new SetClock(java.time.Instant.parse("2026-11-24T20:00:00Z"))

  /** The sites it answers queries of. */
  val Sites: Vector[SiteUrl] =
    Vector("sc-domain:example.com", "https://www.example.com/").map(site)

  /** `start` to `end` (YYYY-MM-DD) grouped `by`, a range a test names rightly, taken on
    * 2026-11-24, the day [[clock]] reads.
    */
  def dates(start: String, end: String, by: Grouping): Dates =
    Dates
      .of(
        java.time.LocalDate.parse(start),
        java.time.LocalDate.parse(end),
        by,
        java.time.LocalDate.parse("2026-11-24")
      )
      .fold(e => throw new java.lang.AssertionError(e), identity)

  /** `raw` as a site, which a test names rightly. */
  def site(raw: String): SiteUrl =
    SiteUrl.of(raw).fold(e => throw new java.lang.AssertionError(e), identity)

  /** The path a query of `site` is sent to, as Google takes it. */
  def queryPath(site: SiteUrl): String =
    s"sites/${java.net.URLEncoder.encode(SiteUrl.value(site), UTF_8)}/searchAnalytics/query"

  /** `searchconsole/{name}.json` as written. */
  def text(name: String): String = {
    val in = Option(getClass.getResourceAsStream(s"/searchconsole/$name.json"))
      .getOrElse(throw new java.lang.AssertionError(s"no fixture searchconsole/$name.json"))
    try new String(in.readAllBytes(), UTF_8)
    finally in.close()
  }

  private val Refused =
    """{"error":{"code":400,"message":"Request contains an invalid argument.","errors":[{"reason":"badRequest"}],"status":"INVALID_ARGUMENT"}}"""

  /** The keys a grouping other than a fixture's own gives its rows, in order. */
  private val Rekeyed: Map[String, Vector[String]] = Map(
    "page" -> Vector(
      "https://www.example.com/shoes/trail",
      "https://www.example.com/",
      "https://www.example.com/boots/waterproof",
      "https://www.example.com/sale"
    ),
    "country" -> Vector("usa", "gbr", "can", "deu"),
    "device" -> Vector("MOBILE", "DESKTOP", "TABLET")
  )

  private def queried(q: ujson.Value): (Int, String) = {
    val o = q.obj
    def str(k: String) = o.get(k).flatMap(_.strOpt)
    val dims = o.get("dimensions").flatMap(_.arrOpt).map(_.toVector.flatMap(_.strOpt))
    val limit = o.get("rowLimit").flatMap(_.numOpt).map(_.toInt).getOrElse(1000)
    def within(rows: Vector[ujson.Value], start: String, end: String) =
      rows.filter(r => r("keys")(0).str.take(10) >= start && r("keys")(0).str.take(10) <= end)
    def rows(name: String) = ujson.read(text(name))("rows").arr.toVector
    (str("type"), str("dataState"), str("startDate"), str("endDate"), dims) match {
      case (Some(_), Some(state), Some(start), Some(end), Some(Vector("date")))
          if state == "all" || state == "final" =>
        val all = ujson.read(text("made-by-date"))
        val answer = ujson.Obj(
          "rows" -> ujson.Arr.from(within(rows("made-by-date"), start, end).take(limit)),
          "responseAggregationType" -> "byProperty"
        )
        val early = all("metadata")("firstIncompleteDate").str
        if (state == "all" && early >= start && early <= end)
          answer("metadata") = ujson.Obj("firstIncompleteDate" -> early)
        (200, ujson.write(answer))
      case (Some("web"), Some("hourly_all"), Some(start), Some(end), Some(Vector("hour"))) =>
        val hours = within(rows("made-by-hour"), start, end).take(limit)
        (200, ujson.write(ujson.Obj("rows" -> ujson.Arr.from(hours))))
      case (Some("discover"), _, _, _, Some(Vector("query"))) => (400, Refused)
      case (Some(search), Some("all"), Some(_), Some(_), Some(Vector(by)))
          if Rekeyed.contains(by) || by == "query" =>
        val (made, own) =
          if (search == "discover") ("made-discover-by-page", "page")
          else ("made-by-query", "query")
        val grouped =
          if (by == own) rows(made)
          else
            rows(made).zip(Rekeyed.getOrElse(by, Vector.empty)).map { (r, key) =>
              ujson.Obj.from(r.obj.toSeq :+ ("keys" -> ujson.Arr(key)))
            }
        (200, ujson.write(ujson.Obj("rows" -> ujson.Arr.from(grouped.take(limit)))))
      case _ => (400, Refused)
    }
  }
}
