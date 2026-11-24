package grit.mcp.searchconsole

import java.time.LocalDate

import grit.core.clock.Clock

import utest.*

/** What only the fake shows: the requests [[ConsoleRest]] makes, how it reads each permission,
  * a re-ranked read and Discover's rows, and each failure Google answers.
  */
object ConsoleRestRequestTests extends TestSuite {

  private val fake = FakeConsole.start()

  override def utestAfterAll(): Unit = fake.stop()

  private val today = LocalDate.parse("2026-11-24")
  private val domain = FakeConsole.site("sc-domain:example.com")
  private val prefix = FakeConsole.site("https://www.example.com/")

  private def dates(by: Grouping): Dates =
    Dates
      .of(today.minusDays(2), today, by, today)
      .fold(e => throw new java.lang.AssertionError(e), identity)

  private def read(
      by: Grouping,
      rank: Rank = Rank.Clicks,
      most: Int = 25,
      filter: Option[Filter] = None,
      search: SearchType = SearchType.Web,
      fetched: Int = ConsoleRest.Fetched,
      site: SiteUrl = domain
  ): Either[ConsoleError, Traffic] =
    fake.rest(Clock.system(), fetched).traffic(site, dates(by), filter, search, rank, most)

  /** The bodies of the queries `run` sends. */
  private def sent(run: => Any): Vector[ujson.Value] = {
    val before = fake.queries.size
    run
    fake.queries.drop(before)
  }

  private def keys(t: Either[ConsoleError, Traffic]) = t.map(_.rows.map(_.key))

  val tests = Tests {
    test("an account that is a user of no property has no sites") {
      fake.rest(Clock.system()).sites() ==> Right(Vector.empty)
    }

    test(
      "each permission Google names is read as its own, an unknown one as Unverified, and a property in no form a site takes is left out"
    ) {
      fake.sharing()
      fake.rest(Clock.system()).sites() ==> Right(
        Vector(
          Site(domain, Permission.Restricted),
          Site(prefix, Permission.Owner),
          Site(FakeConsole.site("https://shop.example.com/"), Permission.Full),
          Site(FakeConsole.site("http://old.example.com/"), Permission.Unverified),
          Site(FakeConsole.site("sc-domain:new.example.com"), Permission.Unverified)
        )
      )
    }

    test("the site is path-encoded in the query's address") {
      val before = fake.paths.size
      read(Grouping.Date)
      read(Grouping.Date, site = prefix)
      fake.paths.drop(before) ==> Vector(
        "sites/sc-domain%3Aexample.com/searchAnalytics/query",
        "sites/https%3A%2F%2Fwww.example.com%2F/searchAnalytics/query"
      )
    }

    test("the filter goes on the totals request and the grouped request alike") {
      val boots =
        Filter.Expression.of("boots").fold(e => throw new java.lang.AssertionError(e), identity)
      val filters = sent {
        read(
          Grouping.Query,
          filter = Some(Filter.Matching(Filter.On.Query, Filter.Test.Contains, boots))
        )
        read(Grouping.Device, filter = Some(Filter.Branded(false)))
      }.map(q => (q("dimensions")(0).str, q.obj.get("dimensionFilterGroups").map(ujson.write(_))))
      val query =
        """[{"groupType":"and","filters":[{"dimension":"query","operator":"contains","expression":"boots"}]}]"""
      val branded =
        """[{"groupType":"and","filters":[{"dimension":"brandedClassification","operator":"equals","expression":"NON_BRANDED"}]}]"""
      filters ==> Vector(
        ("date", Some(query)),
        ("query", Some(query)),
        ("date", Some(branded)),
        ("device", Some(branded))
      )
    }

    test("both requests ask for fresh data, and by hour the grouped one asks hourly_all") {
      sent {
        read(Grouping.Query)
        read(Grouping.Hour)
      }.map(q => (q("dimensions")(0).str, q("dataState").str)) ==> Vector(
        ("date", "all"),
        ("query", "all"),
        ("date", "all"),
        ("hour", "hourly_all")
      )
    }

    test(
      "ranked by impressions, a full read of fetched rows is re-ranked and flagged, a short one unflagged; by clicks only most rows are asked, in Google's order"
    ) {
      def flags(t: Either[ConsoleError, Traffic]) = t.map(t => (t.asked, t.reranked))
      val full = read(Grouping.Query, Rank.Impressions, most = 2, fetched = 3)
      val short = read(Grouping.Query, Rank.Impressions, most = 2, fetched = 10)
      val clicks = read(Grouping.Query, Rank.Clicks, most = 2)
      (flags(full), keys(full), flags(short), keys(short), flags(clicks), keys(clicks)) ==> (
        Right((3, true)),
        Right(Vector("waterproof hiking boots", "trail running shoes")),
        Right((10, false)),
        Right(Vector("waterproof hiking boots", "trail running shoes")),
        Right((2, false)),
        Right(Vector("trail running shoes", "example outfitters"))
      )
    }

    test("by date, the days are the rows, ranked by clicks, from one request alone") {
      val bodies = sent {
        val t = read(Grouping.Date, most = 2)
        (keys(t), t.map(_.early)) ==> (
          Right(Vector("2026-11-23", "2026-11-22")),
          Right(Some(LocalDate.parse("2026-11-23")))
        )
      }
      bodies.size ==> 1
    }

    test("a Discover row without a position is read with none") {
      read(Grouping.Page, search = SearchType.Discover).map(_.rows.map(_.metrics)) ==>
        Right(Vector(Metrics(31, 900, 0.0344444444444444, None)))
    }

    test("a question Google refuses (Discover by query) is Refused, with Google's reasons") {
      read(Grouping.Query, search = SearchType.Discover) ==>
        Left(ConsoleError.Refused(Vector("badRequest")))
    }

    test("an API not enabled in the project is Off, and its message says who can turn it on") {
      fake.failing(
        prefix,
        403,
        """{"error":{"code":403,"status":"PERMISSION_DENIED","details":[{"@type":"type.googleapis.com/google.rpc.ErrorInfo","reason":"SERVICE_DISABLED"}]}}"""
      )
      val off = read(Grouping.Query, site = prefix)
      (
        off,
        off.left.map(_.message.contains("Someone who administers that project can turn on"))
      ) ==>
        (Left(ConsoleError.Off), Left(true))
    }

    test("a 403 for a reason other than a limit is Hidden") {
      fake.failing(
        prefix,
        403,
        """{"error":{"code":403,"errors":[{"reason":"forbidden"}],"status":"PERMISSION_DENIED"}}"""
      )
      read(Grouping.Query, site = prefix) ==> Left(ConsoleError.Hidden)
    }
  }
}
