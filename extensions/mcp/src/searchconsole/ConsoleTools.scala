package grit.mcp.searchconsole

import java.time.LocalDate
import java.time.temporal.ChronoUnit
import java.util.Locale

import grit.core.clock.Clock
import grit.core.edge.{Route, ToolRequest}
import grit.core.place.Service
import grit.core.tool.{
  Args,
  ArgsError,
  Believed,
  Field,
  Gate,
  Hosted,
  Outcome,
  Retry,
  Tool,
  ToolName,
  ToolSet,
  ToolSpec,
  Toolbox,
  Trimmed
}
import grit.core.visibility.{Integrity, Provenance, Readers, ResultPart}
import grit.edge.desk.{Run, Tools}
import grit.prose.form.{Block, Cell, Column, Doc, Text}
import grit.prose.markdown.Markdown

/** The requests a Search Console edge is sent: [[ConsoleTools.Query]] of the one property
  * `site`, read over `rest` with today's Pacific date from `clock`, answered as
  * [[ConsoleTools.answer]] says, or [[Outcome.Failed]] with a refused range's words or a
  * [[ConsoleError]]'s; `said` hears each failure's cause ([[ConsoleError.logged]]).
  */
// `clock` is a parameter, rather than a capture-set parameter over `rest`, so the class's
// captures are in its signature (docs/capture-checking.md, "A capset parameter a class's body
// alone uses").
final class ConsoleTools(clock: Clock, rest: ConsoleRest^{clock}, service: Service, site: SiteUrl)
    extends Tools {

  def run(route: Route, request: ToolRequest, said: String => Unit): Outcome = {
    val query: Tool.Offered^{clock, said} = ConsoleTools.Query.over { asking =>
      asking.at(Dates.today(clock.now())) match {
        case Left(why) => Outcome.Failed(why)
        case Right(asked) =>
          rest.traffic(
            site,
            asked.dates,
            asked.filter,
            asked.search,
            asked.rank,
            asked.rows
          ) match {
            case Left(e) =>
              e.logged.foreach(why => said(s"search_console_query: $why"))
              Outcome.Failed(e.message)
            case Right(traffic) => ConsoleTools.answer(service, site, asked, traffic)
          }
      }
    }
    Toolbox.of[caps.CapSet^{clock, said}](query) match {
      case Right(box) => Run.request(request, box)
      // The one tool cannot repeat a name.
      case Left(repeated) =>
        Outcome.Failed(s"two Search Console tools are named ${ToolName.value(repeated.name)}.")
    }
  }
}

object ConsoleTools {

  /** A call's arguments as read: its days, each absent when the call names none, and the rest
    * of [[Asked]].
    */
  final case class Asking(
      start: Option[LocalDate],
      end: Option[LocalDate],
      by: Grouping,
      filter: Option[Filter],
      search: SearchType,
      rank: Rank,
      rows: Int
  ) {

    /** The question put to Google, today being `today`: each absent day defaulted (the last day
      * yesterday, or today when the first day is; the first day six before the last), or why
      * not, in [[Dates.of]]'s words.
      */
    def at(today: LocalDate): Either[String, Asked] = {
      val yesterday = today.minusDays(1)
      val last = end.getOrElse(start.filter(_.isAfter(yesterday)).getOrElse(yesterday))
      val first = start.getOrElse(last.minusDays(Before))
      Dates.of(first, last, by, today).map(Asked(_, filter, search, rank, rows))
    }
  }

  /** What Google is asked: `rows` top rows of `dates`, grouped as they say, by `rank`. */
  final case class Asked(
      dates: Dates,
      filter: Option[Filter],
      search: SearchType,
      rank: Rank,
      rows: Int
  )

  /** The longest range, in days, whose days an answer lists one by one: 14. */
  val DaysListed: Int = 14

  /** How many days before the last a range starts when the call names no first day. */
  private val Before: Long = 6

  /** How many rows an answer holds when the call names no count. */
  private val RowsDefault: Int = 25

  /** The words a field takes, each for its value: the first `first`. */
  private final class Words[A](first: (String, A), rest: (String, A)*) {
    private val all: Vector[(String, A)] = first +: rest.toVector

    def field(meaning: String): Field[Option[String]] =
      Field.oneOf(meaning, first._1, rest.map(_._1)*).optional

    /** `word`'s value, `absent` when the call named none. */
    def read(name: String, word: Option[String], absent: A): Either[ArgsError, A] =
      word match {
        case None => Right(absent)
        case Some(w) =>
          all
            .collectFirst { case (k, a) if k == w => a }
            .toRight(
              ArgsError.Refused(name, s"$name takes one of ${all.map(_._1).mkString(", ")}.")
            )
      }
  }

  private val Groupings = new Words(
    "query" -> Grouping.Query,
    "page" -> Grouping.Page,
    "country" -> Grouping.Country,
    "device" -> Grouping.Device,
    "date" -> Grouping.Date,
    "hour" -> Grouping.Hour
  )

  private val Ranks = new Words("clicks" -> Rank.Clicks, "impressions" -> Rank.Impressions)

  private val Searches = new Words(
    "web" -> SearchType.Web,
    "image" -> SearchType.Image,
    "video" -> SearchType.Video,
    "news" -> SearchType.News,
    "discover" -> SearchType.Discover
  )

  private val Ons = new Words(
    "query" -> Filter.On.Query,
    "page" -> Filter.On.Page,
    "country" -> Filter.On.Country,
    "device" -> Filter.On.Device
  )

  private val Tests = new Words(
    "contains" -> Filter.Test.Contains,
    "equals" -> Filter.Test.Equals,
    "not_contains" -> Filter.Test.NotContains
  )

  private def day(field: String, raw: Option[String]): Either[ArgsError, Option[LocalDate]] =
    raw match {
      case None => Right(None)
      case Some(s) =>
        scala.util
          .Try(LocalDate.parse(s.trim))
          .toOption
          .map(Some(_))
          .toRight(ArgsError.Refused(field, s"$s is not a day written YYYY-MM-DD."))
    }

  private def filterOf(
      on: Option[String],
      test: Option[String],
      expression: Option[String],
      branded: Option[Boolean]
  ): Either[ArgsError, Option[Filter]] =
    (on, test, expression, branded) match {
      case (None, None, None, None) => Right(None)
      case (None, None, None, Some(b)) => Right(Some(Filter.Branded(b)))
      case (Some(o), Some(t), Some(e), None) =>
        for {
          field <- Ons.read("on", Some(o), Filter.On.Query)
          how <- Tests.read("test", Some(t), Filter.Test.Contains)
          text <- Filter.Expression.of(e).left.map(ArgsError.Refused("expression", _))
        } yield Some(Filter.Matching(field, how, text))
      case (_, _, _, Some(_)) =>
        Left(
          ArgsError.Refused("branded", "Give either on, test and expression, or branded, not both.")
        )
      case _ => Left(ArgsError.Refused("on", "A filter needs on, test and expression together."))
    }

  /** `search_console_query`. */
  val Query: Hosted[Asking] = new Hosted(
    ToolSpec(
      ToolName("search_console_query"),
      "Read how the site does in Google search, from Search Console: clicks and impressions " +
        "for each day of a range, and the top rows grouped by search query, page, country, " +
        "device, date or hour. Days are Pacific time; by default the seven days through " +
        "yesterday. The newest days' numbers are early and may still change, and Google leaves " +
        "out queries that very few people made; each answer says when either applies.",
      Args
        .of(
          (
            by = Groupings.field(
              "What the top rows are: search queries (the default), the site's pages, " +
                "countries, devices, days or hours."
            ),
            start = Field
              .text(
                "The first day, YYYY-MM-DD, Pacific time. Default: six days before the last " +
                  s"day. A range spans at most ${Dates.Most} days, or 16 months grouped by " +
                  "date; by hour it reaches back nine days."
              )
              .optional,
            end = Field
              .text(
                "The last day, YYYY-MM-DD, Pacific time. Default: yesterday, or today when the " +
                  "first day is today."
              )
              .optional,
            rows = Field
              .count(s"How many top rows, 1 to 100. Default: $RowsDefault.", 1, 100)
              .optional,
            rank = Ranks.field("Rank the top rows by clicks (the default) or by impressions."),
            on = Ons.field(
              "To count only some searches: what to match. Give test and expression with it."
            ),
            test = Tests.field(
              "How expression is matched. Equals is case-sensitive for queries and pages; " +
                "contains never is."
            ),
            expression = Field
              .text(
                "The text to match. A country is its three-letter code, such as usa; a device " +
                  "is desktop, mobile or tablet."
              )
              .optional,
            branded = Field
              .flag(
                "Instead of on, test and expression: true counts only searches that name the " +
                  "brand, false only those that don't. Google classes searches so only from " +
                  "2026-02-21, and only for sites with enough searches."
              )
              .optional,
            search = Searches.field(
              "Which results: web (the default), image, video, news or discover. Discover has " +
                "no search queries."
            )
          )
        )
        .refine(a =>
          for {
            by <- Groupings.read("by", a.by, Grouping.Query)
            rank <- Ranks.read("rank", a.rank, Rank.Clicks)
            search <- Searches.read("search", a.search, SearchType.Web)
            start <- day("start", a.start)
            end <- day("end", a.end)
            filter <- filterOf(a.on, a.test, a.expression, a.branded)
          } yield Asking(start, end, by, filter, search, rank, a.rows.getOrElse(RowsDefault))
        ),
      Retry.Rerun
    ),
    Gate.Free,
    a =>
      s"by ${a.by.toString.toLowerCase(Locale.ROOT)}, ${a.start.fold("six days before")(_.toString)} to " +
        a.end.fold("yesterday")(_.toString),
    Believed.At(Integrity.Untrusted)
  )

  /** The one tool, as an edge advertises it. */
  val Offered: ToolSet = ToolSet.of(Vector(Query.entry)).getOrElse(ToolSet.Empty)

  /** `traffic`, read for `asked` of `site`, as the model reads it: a head of grit's words
    * stating the range's totals, each day's when the range is at most [[DaysListed]] days (but
    * by date, whose rows are its days), which days are early, the filter, the share of
    * impressions the rows hold (not by page, date or hour), that Google leaves rare queries out
    * (by query, when the rows add up to less), that it has more rows (when there are as many as
    * asked) and that a re-ranking is partial, ending with a markdown table's header when there
    * are rows; then each row its own part at `service`'s place, believed at the cap declared
    * there, one line of that table: its key (a query's or page's written as a JSON string),
    * clicks, impressions, click-through and, when any row has one, average position.
    */
  def answer(service: Service, site: SiteUrl, asked: Asked, traffic: Traffic): Outcome.Done = {
    val by = asked.dates.by
    val (one, many) = named(by)
    val clicks = traffic.days.map(_.metrics.clicks).sum
    val impressions = traffic.days.map(_.metrics.impressions).sum
    def marked(day: Option[LocalDate]): String =
      if (day.exists(d => traffic.early.exists(e => !d.isBefore(e)))) " (early)" else ""
    val span = ChronoUnit.DAYS.between(asked.dates.start, asked.dates.end) + 1
    val daily =
      if (by == Grouping.Date) Vector.empty
      else if (span > DaysListed) Vector("Group by date for each day's numbers.")
      else
        traffic.days.map(d =>
          s"${d.date}: ${d.metrics.clicks} clicks, ${d.metrics.impressions} impressions" +
            marked(Some(d.date))
        )
    val fresh = traffic.early.toVector.map(d =>
      s"The numbers from $d on are early: Google may still change them over the next few days."
    )
    val shown = traffic.rows.map(_.metrics.impressions).sum
    val ranked = asked.rank match {
      case Rank.Clicks => "clicks"
      case Rank.Impressions => "impressions"
    }
    // Google aggregates pages apart from the property, so their share can pass 100%; days and
    // hours hold every impression by construction.
    val shares = by match {
      case Grouping.Query | Grouping.Country | Grouping.Device => impressions > 0
      case Grouping.Page | Grouping.Date | Grouping.Hour => false
    }
    val top =
      if (traffic.rows.isEmpty) s"Google lists no $many for these days."
      else {
        val alone = traffic.rows.size == 1
        val held =
          if (shares)
            s", ${if (alone) "holds" else "hold"} $shown of the $impressions impressions " +
              s"(${math.round(shown * 100.0 / impressions)}%)"
          else ""
        if (alone) s"The top $one by $ranked, below$held."
        else s"The top ${traffic.rows.size} $many by $ranked, one a row of the table below$held."
      }
    val hidden =
      if (by == Grouping.Query && traffic.rows.nonEmpty && shown < impressions)
        Vector(
          "Google leaves out queries that very few people made, for their privacy, so the " +
            "queries it lists add up to less than the totals."
        )
      else Vector.empty
    val more =
      if (by == Grouping.Date) {
        if (traffic.days.size > traffic.rows.size)
          Vector("The range has more days than these; the totals above count them all.")
        else Vector.empty
      } else if (traffic.rows.nonEmpty && traffic.rows.size >= asked.rows)
        Vector(s"Google has more $many than these.")
      else Vector.empty
    val partial =
      if (traffic.reranked)
        Vector(
          s"Google ranks $many by clicks, so these were ranked by impressions among the " +
            s"${traffic.asked} with the most clicks: a $one with many impressions and few " +
            "clicks may be missing."
        )
      else Vector.empty
    val positioned = traffic.rows.exists(_.metrics.position.nonEmpty)
    val columns =
      (Vector(one.capitalize, "Clicks", "Impressions", "Click-through") ++
        Option.when(positioned)("Average position")).map(c => Column(Text.plain(c)))
    // Lines as Markdown writes a table's, so a pipe in a cell is escaped and a line break is a
    // space: each row is one line, and no cell ends early.
    def table(rows: Vector[Vector[String]]): Vector[String] =
      Markdown
        .write(Doc(Vector(Block.table(columns, rows.map(_.map(c => Cell.Words(Text.plain(c))))))))
        .split("\n")
        .toVector
    val header = if (traffic.rows.isEmpty) Vector.empty else table(Vector.empty)
    val head = (Vector(
      s"${searched(asked.search)} for ${SiteUrl.value(site)}, ${asked.dates.start} to " +
        s"${asked.dates.end}, Pacific time: $clicks clicks and $impressions impressions in all."
    ) ++ asked.filter.map(f =>
      s"Only searches ${counted(f)} are counted, in the totals and the rows alike."
    ) ++ daily ++ fresh ++ Vector(top) ++ hidden ++ more ++ partial ++ header).mkString("\n")
    // What searchers typed, and the pages a site holds, are JSON strings, so no quote or line
    // break in them reads as grit's.
    val quoted = by match {
      case Grouping.Query | Grouping.Page => true
      case Grouping.Country | Grouping.Device | Grouping.Date | Grouping.Hour => false
    }
    val parts = traffic.rows.map { r =>
      val key = if (quoted) ujson.write(ujson.Str(r.key)) else r.key
      val day = by match {
        case Grouping.Date | Grouping.Hour =>
          scala.util.Try(LocalDate.parse(r.key.take(10))).toOption
        case Grouping.Query | Grouping.Page | Grouping.Country | Grouping.Device => None
      }
      ResultPart(
        // A row is Google's count over many searches, with no one author to vouch for: it
        // vouches nothing below the top, so the cap at the service's place alone decides what
        // it is believed at (Visibility.believed meets the two).
        Provenance(service.place, Integrity.Authority),
        Readers.AtPlace,
        table(Vector(s"$key${marked(day)}" +: numbers(r.metrics, positioned))).drop(2).mkString(" ")
      )
    }
    val trimmed = Trimmed.of(head, parts)
    Outcome.Done(trimmed.head, parts = trimmed.parts)
  }

  /** What one row grouped `by` is, and what several are. */
  private def named(by: Grouping): (String, String) = by match {
    case Grouping.Query => ("query", "queries")
    case Grouping.Page => ("page", "pages")
    case Grouping.Country => ("country", "countries")
    case Grouping.Device => ("device", "devices")
    case Grouping.Date => ("day", "days")
    case Grouping.Hour => ("hour", "hours")
  }

  private def searched(search: SearchType): String = search match {
    case SearchType.Web => "Google web search"
    case SearchType.Image => "Google image search"
    case SearchType.Video => "Google video search"
    case SearchType.News => "Google search's News tab"
    case SearchType.Discover => "Google Discover"
  }

  /** Which searches `filter` counts, after "Only searches". */
  private def counted(filter: Filter): String = filter match {
    case Filter.Branded(true) => "that name the brand"
    case Filter.Branded(false) => "that do not name the brand"
    case Filter.Matching(on, test, expression) =>
      val field = on match {
        case Filter.On.Query => "query"
        case Filter.On.Page => "page"
        case Filter.On.Country => "country"
        case Filter.On.Device => "device"
      }
      val how = test match {
        case Filter.Test.Contains => "contains"
        case Filter.Test.Equals => "is"
        case Filter.Test.NotContains => "does not contain"
      }
      s"whose $field $how ${ujson.write(ujson.Str(Filter.Expression.value(expression)))}"
  }

  /** A row's cells after its key: clicks, impressions, click-through, and its average position
    * when `positioned` (empty when it has none).
    */
  private def numbers(m: Metrics, positioned: Boolean): Vector[String] =
    Vector(
      m.clicks.toString,
      m.impressions.toString,
      "%.1f%%".formatLocal(Locale.ROOT, m.ctr * 100)
    ) ++ Option.when(positioned)(m.position.fold("")("%.1f".formatLocal(Locale.ROOT, _)))
}
