package grit.mcp.searchconsole

import java.net.URI
import java.time.{Instant, LocalDate}

import grit.core.clock.Clock
import grit.core.edge.Variable
import grit.mcp.client.{Authorizer, Credential, KeyFiles}
import grit.mcp.drive.DriveLive
import grit.mcp.google.{GoogleRequest, GoogleRest}

/** [[ConsoleRestContract]] against Search Console itself, read-only (no model call, no spend),
  * and nothing else read:
  *
  * {{{
  * GRIT_DRIVE_SA_KEY=<the key file's absolute path> GRIT_CONSOLE_SITE=<the site, as Google names it> \
  *   ./mill grit.mcp.test.runMain grit.mcp.searchconsole.ConsoleLive
  * }}}
  *
  * The service account the key names must be a user of the site. Before the contract it prints
  * the answers' shapes alone: how many sites, the site's permission, and over the last three
  * Pacific days how many days and rows, the first early day, and the field names of the by-date
  * answer and of its metadata. It never prints the key, the site, a query or a page, failures
  * included. Exits 1 when a variable is unset, the key or the site is refused, or a test fails.
  */
object ConsoleLive {

  /** The site the contract reads. */
  val Site: Variable = Variable("GRIT_CONSOLE_SITE")

  def main(args: Array[String]): Unit = {
    val env = sys.env
    val opened = for {
      raw <- env.get(Variable.value(Site)).toRight(s"${Variable.value(Site)} is unset")
      site <- SiteUrl.of(raw).left.map(_ => s"${Variable.value(Site)} names no site")
      _ <- env
        .get(Variable.value(DriveLive.Key))
        .toRight(s"${Variable.value(DriveLive.Key)} is unset")
      authorizer <- Credential
        .open(Credential.serviceAccount(DriveLive.Key, ConsoleRest.ReadOnly), env, KeyFiles.Local)
        .left
        .map(_.message)
    } yield (site, authorizer)
    opened match {
      case Left(why) =>
        System.err.println(s"ConsoleLive could not run: $why")
        sys.exit(1)
      case Right((site, authorizer)) =>
        val clock = Clock.system()
        val today = Dates.today(Instant.now())
        shapes(site, authorizer, clock, today)
        val contract = new Live(authorizer, site, today)
        val run = utest.TestRunner.runAndPrint(contract.tests, "ConsoleLive")
        val leaves = run.leaves.toVector
        val failed = leaves.count(_.value.isFailure)
        println(s"ConsoleLive: ${leaves.size - failed} passed, $failed failed")
        if (failed > 0) sys.exit(1)
    }
  }

  /** Prints what [[ConsoleLive]] says it prints, and nothing more. */
  private def shapes(
      site: SiteUrl,
      authorizer: Authorizer,
      clock: Clock,
      today: LocalDate
  ): Unit = {
    val api = URI.create(ConsoleRest.Api)
    val rest = new ConsoleRest(api, authorizer, clock)
    rest.sites() match {
      case Right(sites) =>
        println(
          s"ConsoleLive: ${sites.size} sites; the site's permission: " +
            sites.find(_.url == site).fold("not listed")(_.permission.toString)
        )
      case Left(e) => println(s"ConsoleLive: sites failed: ${e.message}")
    }
    Dates.of(today.minusDays(2), today, Grouping.Query, today).foreach { dates =>
      rest.traffic(site, dates, None, SearchType.Web, Rank.Clicks, 25) match {
        case Right(t) =>
          println(
            s"ConsoleLive: ${dates.start} to ${dates.end}: ${t.days.size} days, ${t.rows.size} " +
              s"query rows of ${t.asked} asked, early from ${t.early.fold("no day")(_.toString)}"
          )
        case Left(e) => println(s"ConsoleLive: traffic failed: ${e.message}")
      }
      val path =
        s"sites/${java.net.URLEncoder.encode(SiteUrl.value(site), java.nio.charset.StandardCharsets.UTF_8)}/searchAnalytics/query"
      val body = ConsoleRest.query(dates, Grouping.Date, None, SearchType.Web, 3)
      GoogleRest(authorizer, clock).json(
        GoogleRequest("Search Console", api, path, json = Some(body))
      ) match {
        case Right(answer) =>
          val fields = answer.objOpt.fold(Vector.empty[String])(_.keys.toVector.sorted)
          val metadata = answer.objOpt
            .flatMap(_.get("metadata"))
            .flatMap(_.objOpt)
            .fold(Vector.empty[String])(_.keys.toVector.sorted)
          println(
            s"ConsoleLive: the by-date answer's fields: ${fields.mkString(", ")}; " +
              s"its metadata's: ${if (metadata.isEmpty) "none" else metadata.mkString(", ")}"
          )
        case Left(_) => println("ConsoleLive: the by-date answer could not be read")
      }
    }
  }

  /** The contract over the site the variables name. */
  private final class Live(authorizer: Authorizer, siteUrl: SiteUrl, day: LocalDate)
      extends ConsoleRestContract {
    protected def rest: ConsoleRest^ =
      new ConsoleRest(URI.create(ConsoleRest.Api), authorizer, Clock.system())
    protected def site: SiteUrl = siteUrl
    protected def today: LocalDate = day
  }
}
