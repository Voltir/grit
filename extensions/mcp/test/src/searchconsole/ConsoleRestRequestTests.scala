package grit.mcp.searchconsole

import java.time.LocalDate

import grit.core.clock.Clock
import grit.mcp.google.GoogleFailure

import utest.*

/** What only the fake shows: the requests [[ConsoleRest]] makes, how it reads each permission,
  * a re-ranked read and Discover's rows, and each failure Google answers.
  */
object ConsoleRestRequestTests extends TestSuite {

  private val domain = FakeConsole.site("sc-domain:example.com")
  private val prefix = FakeConsole.site("https://www.example.com/")

  private def withFake[A](body: FakeConsole => A): A = {
    val fake = FakeConsole.start()
    try body(fake)
    finally fake.stop()
  }

  private def read(
      fake: FakeConsole,
      by: Grouping,
      rank: Rank = Rank.Clicks,
      most: Int = 25,
      filter: Option[Filter] = None,
      search: SearchType = SearchType.Web,
      fetched: Int = ConsoleRest.Fetched,
      site: SiteUrl = domain,
      start: String = "2026-11-22",
      end: String = "2026-11-24"
  ): Either[ConsoleError, Traffic] =
    fake
      .rest(Clock.system(), fetched)
      .traffic(site, FakeConsole.dates(start, end, by), filter, search, rank, most)

  /** The bodies of the queries `run` sends to `fake`, and what it came to. */
  private def sent[A](fake: FakeConsole)(run: => A): (Vector[ujson.Value], A) = {
    val before = fake.queries.size
    val ran = run
    (fake.queries.drop(before), ran)
  }

  /** Each body's grouping and the rows it asks for, if it asks a number. */
  private def limits(bodies: Vector[ujson.Value]): Vector[(String, Option[Int])] =
    bodies.map(q => (q("dimensions")(0).str, q.obj.get("rowLimit").flatMap(_.numOpt).map(_.toInt)))

  private def keys(t: Either[ConsoleError, Traffic]) = t.map(_.rows.map(_.key))

  val tests = Tests {
    test("an account that is a user of no property has no sites") {
      withFake(_.rest(Clock.system()).sites()) ==> Right(Vector.empty)
    }

    test(
      "each permission Google names is read as its own, an unknown one as Unverified, and a property in no form a site takes is left out"
    ) {
      withFake { fake =>
        fake.sharing()
        fake.rest(Clock.system()).sites()
      } ==> Right(
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
      withFake { fake =>
        read(fake, Grouping.Date)
        read(fake, Grouping.Date, site = prefix)
        fake.paths
      } ==> Vector(
        "sites/sc-domain%3Aexample.com/searchAnalytics/query",
        "sites/https%3A%2F%2Fwww.example.com%2F/searchAnalytics/query"
      )
    }

    test("the filter goes on the totals request and the grouped request alike") {
      val boots =
        Filter.Expression.of("boots").fold(e => throw new java.lang.AssertionError(e), identity)
      val (bodies, (byQuery, byDevice)) = withFake { fake =>
        sent(fake) {
          (
            read(
              fake,
              Grouping.Query,
              filter = Some(Filter.Matching(Filter.On.Query, Filter.Test.Contains, boots))
            ),
            read(fake, Grouping.Device, filter = Some(Filter.Branded(false)))
          )
        }
      }
      val filters = bodies.map(q =>
        (q("dimensions")(0).str, q.obj.get("dimensionFilterGroups").map(ujson.write(_)))
      )
      val query =
        """[{"groupType":"and","filters":[{"dimension":"query","operator":"contains","expression":"boots"}]}]"""
      val branded =
        """[{"groupType":"and","filters":[{"dimension":"brandedClassification","operator":"equals","expression":"NON_BRANDED"}]}]"""
      (byQuery.isRight, byDevice.isRight, filters) ==> (
        true,
        true,
        Vector(
          ("date", Some(query)),
          ("query", Some(query)),
          ("date", Some(branded)),
          ("device", Some(branded))
        )
      )
    }

    test("both requests ask for fresh data, and by hour the grouped one asks hourly_all") {
      withFake { fake =>
        sent(fake) {
          read(fake, Grouping.Query)
          read(fake, Grouping.Hour)
        }._1
      }.map(q => (q("dimensions")(0).str, q("dataState").str)) ==> Vector(
        ("date", "all"),
        ("query", "all"),
        ("date", "all"),
        ("hour", "hourly_all")
      )
    }

    test("by hour, every hour of the range is asked, 25 a day, and read ranked by clicks") {
      val (bodies, hours) = withFake { fake =>
        sent(fake) {
          read(fake, Grouping.Hour, most = 100, start = "2026-11-23", end = "2026-11-24")
        }
      }
      (limits(bodies), keys(hours), hours.map(_.asked)) ==> (
        Vector(("date", Some(2)), ("hour", Some(50))),
        Right(
          Vector(
            "2026-11-23T10:00:00-08:00",
            "2026-11-23T09:00:00-08:00",
            "2026-11-24T08:00:00-08:00"
          )
        ),
        Right(50)
      )
    }

    test(
      "ranked by impressions, rows are re-ranked from a read of `fetched` rows, flagged when that read came back full"
    ) {
      def flags(t: Either[ConsoleError, Traffic]) = t.map(t => (t.asked, t.reranked))
      val (bodies, (full, short, clicks)) = withFake { fake =>
        sent(fake) {
          (
            read(fake, Grouping.Query, Rank.Impressions, most = 2, fetched = 2),
            read(fake, Grouping.Query, Rank.Impressions, most = 2, fetched = 10),
            read(fake, Grouping.Query, Rank.Clicks, most = 2)
          )
        }
      }
      (
        limits(bodies),
        flags(full),
        keys(full),
        flags(short),
        keys(short),
        flags(clicks),
        keys(clicks)
      ) ==> (
        Vector(
          ("date", Some(3)),
          ("query", Some(2)),
          ("date", Some(3)),
          ("query", Some(10)),
          ("date", Some(3)),
          ("query", Some(2))
        ),
        Right((2, true)),
        Right(Vector("trail running shoes", "example outfitters")),
        Right((10, false)),
        Right(Vector("waterproof hiking boots", "trail running shoes")),
        Right((2, false)),
        Right(Vector("trail running shoes", "example outfitters"))
      )
    }

    test("by date, the days are the rows, ranked by clicks, from one request alone") {
      val (bodies, t) = withFake(fake => sent(fake)(read(fake, Grouping.Date, most = 2)))
      (bodies.size, keys(t), t.map(_.early)) ==> (
        1,
        Right(Vector("2026-11-23", "2026-11-22")),
        Right(Some(LocalDate.parse("2026-11-23")))
      )
    }

    test("a Discover row without a position is read with none") {
      withFake(read(_, Grouping.Page, search = SearchType.Discover))
        .map(_.rows.map(_.metrics)) ==>
        Right(Vector(Metrics(31, 900, 0.0344444444444444, None)))
    }

    test("a question Google refuses (Discover by query) is Refused, with Google's reasons") {
      withFake(read(_, Grouping.Query, search = SearchType.Discover)) ==>
        Left(ConsoleError.Refused(Vector("badRequest")))
    }

    test("an API not enabled in the project is Off, and its message says who can turn it on") {
      val off = withFake { fake =>
        fake.failing(
          prefix,
          403,
          """{"error":{"code":403,"status":"PERMISSION_DENIED","details":[{"@type":"type.googleapis.com/google.rpc.ErrorInfo","reason":"SERVICE_DISABLED"}]}}"""
        )
        read(fake, Grouping.Query, site = prefix)
      }
      (off, off.left.map(_.message)) ==> (
        Left(ConsoleError.Google(GoogleFailure.Off)),
        Left(
          "Search Console's API is not turned on in the Google Cloud project grit's service " +
            "account belongs to. Someone who administers that project can turn on the Google " +
            "Search Console API in its APIs & Services page; grit can read the site once it is on."
        )
      )
    }

    test("a 403 for a reason other than a limit is Hidden") {
      withFake { fake =>
        fake.failing(
          prefix,
          403,
          """{"error":{"code":403,"errors":[{"reason":"forbidden"}],"status":"PERMISSION_DENIED"}}"""
        )
        read(fake, Grouping.Query, site = prefix)
      } ==> Left(ConsoleError.Google(GoogleFailure.Hidden))
    }
  }
}
