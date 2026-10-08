package grit.app.main

import grit.core.clock.Clock
import grit.core.edge.{EdgeRefusal, EdgeStores, ServedEdge, Variable}
import grit.core.id.EdgeName
import grit.core.identity.Domain
import grit.core.place.Scope as PlaceScope
import grit.core.spend.DailyCap
import grit.kit.deployment.Offered
import grit.slack.edge.{SlackAccounts, SlackCommand, SlackEdge}
import grit.slack.event.TeamId

import utest.*

/** What the reference deployment reads for `grit serve` and `grit backfill`, before it
  * touches the database or Slack.
  */
object ServeChoiceTests extends TestSuite {

  private val Grit: SlackCommand =
    SlackCommand.of("/grit").fold(e => throw new java.lang.AssertionError(e), identity)

  /** An edge that is never opened here. */
  private object Quiet extends ServedEdge {
    def name: EdgeName = EdgeName("slack")
    def needs: Vector[Variable] = Vector.empty
    def answersAsks: Boolean = false
    def open(
        stores: EdgeStores^,
        env: Map[String, String],
        clock: Clock^,
        log: String => Unit
    ): Either[EdgeRefusal, ServedEdge.Open^{stores, clock, log, caps.any}] =
      Left(EdgeRefusal.Refused("never opened here"))
  }

  val tests = Tests {
    test(
      "serving an edge declares scope room and caps a day at $1.00 when their variables are unset; the chat does neither"
    ) {
      def of(edges: Vector[ServedEdge]) =
        Main
          .deployment(Map.empty, Offered.Read, edges, Vector.empty, java.time.ZoneOffset.UTC)
          .map(d => (d.lifecycle.locality.scope, d.budget.cap))
      (of(Vector(Quiet)), of(Vector.empty)) ==> (
        Right((PlaceScope.Room, DailyCap.of("1.00").toOption)),
        Right((grit.core.period.LifecycleSettings.Default.locality.scope, None))
      )
    }

    test("the domains GRIT_CLAIMED_DOMAINS lists are the deployment's own; unset, it claims none") {
      def claimed(env: Map[String, String]) =
        Main
          .deployment(env, Offered.Read, Vector(Quiet), Vector.empty, java.time.ZoneOffset.UTC)
          .map(_.identities.domains.map(Domain.value))
      (claimed(Map("GRIT_CLAIMED_DOMAINS" -> "example.com,other.org")), claimed(Map.empty)) ==>
        (Right(Set("example.com", "other.org")), Right(Set()))
    }

    test(
      "serving Slack installed in a team trusts the Slack attester for that team's accounts; with no team, nothing is trusted"
    ) {
      def trusted(slackIn: Option[TeamId]) =
        Main
          .deployment(
            Map.empty,
            Offered.Read,
            Vector(SlackEdge.serving(Grit, Set.empty)),
            Vector.empty,
            java.time.ZoneOffset.UTC,
            slackIn
          )
          .map(_.identities.realmsOf(SlackAccounts.Attester))
      (trusted(Some(TeamId("T0FAKE"))), trusted(None)) ==>
        (SlackAccounts.realm(TeamId("T0FAKE")).map(Set(_)), Right(Set.empty))
    }

    test(
      "the slash command answered is GRIT_SLACK_COMMAND, /grit when unset, refused naming the variable when Slack would not take it"
    ) {
      (
        Main.slackCommand(Map.empty).map(_.name),
        Main.slackCommand(Map("GRIT_SLACK_COMMAND" -> " /acme ")).map(_.name),
        Main.slackCommand(Map("GRIT_SLACK_COMMAND" -> "acme"))
      ) ==> (
        Right("/grit"),
        Right("/acme"),
        Left("GRIT_SLACK_COMMAND: acme: a slash command is / then 1 to 32 of a-z, 0-9, - and _")
      )
    }

    test("a listened channel that is not a channel id is refused, naming it") {
      Main.listening(Map("GRIT_SLACK_LISTEN" -> "C123ABC456, #general")) ==>
        Left(
          "GRIT_SLACK_LISTEN: #general is not a channel id (C… or G…, as Slack's channel details show it)"
        )
    }

    test(
      "the days backfill reads are GRIT_BACKFILL_DAYS, 2 when unset, refused unless a whole number above zero, or when no channel is listened in"
    ) {
      val listen = Map("GRIT_SLACK_LISTEN" -> "C123ABC456")
      (
        Main.backfillDays(listen),
        Main.backfillDays(listen + ("GRIT_BACKFILL_DAYS" -> " 7 ")),
        Main.backfillDays(listen + ("GRIT_BACKFILL_DAYS" -> "0")),
        Main.backfillDays(listen + ("GRIT_BACKFILL_DAYS" -> "1.5")),
        Main.backfillDays(Map.empty)
      ) ==> (
        Right(2),
        Right(7),
        Left("GRIT_BACKFILL_DAYS is a whole number of days above zero, not '0'"),
        Left("GRIT_BACKFILL_DAYS is a whole number of days above zero, not '1.5'"),
        Left("GRIT_SLACK_LISTEN names no channel: there is nothing to backfill")
      )
    }

    test("with GITHUB_MCP_TOKEN unset, serve serves no GitHub edge") {
      Main.github(Map("GRIT_GITHUB_TOOLS" -> "get_me")) ==> Right(None)
    }

    test(
      "with it set, serve serves GitHub's read-only MCP endpoint, offering the ten tools, and Slack's conversations work there"
    ) {
      val env = Map("GITHUB_MCP_TOKEN" -> "never-read")
      (
        Main.githubServer(env).map(s => (s.name, s.endpoint.toString, s.token, s.allow)),
        Main
          .github(env)
          .map(
            _.map((edge, link) =>
              (
                EdgeName.value(edge.name),
                edge.needs,
                link.within.written,
                link.service.place.written
              )
            )
          )
      ) ==> (
        Right(
          (
            "github",
            "https://api.githubcopilot.com/mcp/readonly",
            Variable("GITHUB_MCP_TOKEN"),
            Set(
              "get_file_contents",
              "list_commits",
              "get_commit",
              "search_code",
              "issue_read",
              "list_issues",
              "search_issues",
              "pull_request_read",
              "list_pull_requests",
              "search_pull_requests"
            )
          )
        ),
        Right(Some(("github", Vector(Variable("GITHUB_MCP_TOKEN")), "slack:", "service:github")))
      )
    }

    test(
      "GRIT_GITHUB_MCP_URL and GRIT_GITHUB_TOOLS replace the defaults; a URL MCP refuses, or a list naming no tool, is refused naming its variable"
    ) {
      val env = Map("GITHUB_MCP_TOKEN" -> "never-read")
      def server(more: (String, String)*) =
        Main.githubServer(env ++ more).map(s => (s.endpoint.toString, s.allow))
      (
        server(
          "GRIT_GITHUB_MCP_URL" -> "http://127.0.0.1:8082/mcp",
          "GRIT_GITHUB_TOOLS" -> " get_me, issue_read "
        ),
        server("GRIT_GITHUB_MCP_URL" -> "http://example.com/mcp"),
        server("GRIT_GITHUB_TOOLS" -> " , ")
      ) ==> (
        Right(("http://127.0.0.1:8082/mcp", Set("get_me", "issue_read"))),
        Left(
          "GRIT_GITHUB_MCP_URL: github's endpoint must be https (http only on a loopback host): 'http://example.com/mcp'"
        ),
        Left("GRIT_GITHUB_TOOLS names no tool")
      )
    }
  }
}
