package grit.app.main

import grit.core.clock.Clock
import grit.core.edge.{EdgeRefusal, EdgeStores, ServedEdge, Variable}
import grit.core.id.EdgeName
import grit.core.identity.{Account, Domain}
import grit.core.place.{Namespace, Place, Reaches, Scope as PlaceScope, Service, WorksIn}
import grit.core.spend.DailyCap
import grit.core.visibility.{
  Accounted,
  Compartments,
  Group,
  Integrity,
  Itemised,
  Label,
  Party,
  Provenance,
  Room
}
import grit.kit.deployment.Offered
import grit.mcp.client.Credential
import grit.mcp.wire.McpCalled
import grit.slack.edge.{Backfill, ReplyNote, SlackAccounts, SlackCommand, SlackEdge}
import grit.slack.event.{TeamId, UserId}

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

    test(
      "serve declares the knowledge plugin's sync when GRIT_PLUGINS names it with GitHub's repository, and refuses to start, naming the variable, without it"
    ) {
      val github = Map(
        "GRIT_PLUGINS" -> "knowledge",
        "GITHUB_MCP_TOKEN" -> "token",
        "GRIT_GITHUB_OWNER" -> "octocat",
        "GRIT_GITHUB_REPO" -> "Hello-World",
        "GRIT_GITHUB_REF" -> "main"
      )
      def declared(env: Map[String, String]) =
        Main
          .deployment(env, Offered.Read, Vector(Quiet), Vector.empty, java.time.ZoneOffset.UTC)
          .map(_.declared.map((_, d) => grit.core.id.ScheduleKey.value(d.key)))
      (declared(github), declared(github - "GRIT_GITHUB_OWNER")) ==> (
        Right(Vector("github-daily")),
        Left(
          "GRIT_PLUGINS: knowledge reads the repository GRIT_GITHUB_OWNER names, and it is unset or blank"
        )
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
            Vector(SlackEdge.serving(Grit, Backfill.Default, ReplyNote.Off)),
            Vector.empty,
            java.time.ZoneOffset.UTC,
            slackIn
          )
          .map(_.identities.realmsOf(SlackAccounts.Attester))
      (trusted(Some(TeamId("T0FAKE"))), trusted(None)) ==>
        (SlackAccounts.realm(TeamId("T0FAKE")).map(Set(_)), Right(Set.empty))
    }

    test(
      "Slack's posting service's results are believed at member and GitHub's at untrusted, with admins named or not"
    ) {
      val github = Service.of("github").fold(sys.error, identity)
      def outputs(env: Map[String, String], slackIn: Option[TeamId]) =
        Main
          .deployment(
            env,
            Offered.Read,
            Vector(SlackEdge.serving(Grit, Backfill.Default, ReplyNote.Off)),
            Vector.empty,
            java.time.ZoneOffset.UTC,
            slackIn
          )
          .map(d =>
            Vector(SlackEdge.PostsAt, github)
              .map(s => (s.name, Integrity.written(d.visibility.outputOf(s))))
          )
      val declared = Right(Vector(("slack", "member"), ("github", "untrusted")))
      (
        outputs(Map("GRIT_ADMINS" -> "U012AB34"), Some(TeamId("T0FAKE"))),
        outputs(Map.empty, None)
      ) ==> (declared, declared)
    }

    test(
      "GRIT_ADMINS names Slack users by id, comma-separated, none when unset; refused naming the variable at one that is not a user's id"
    ) {
      (
        Main.admins(Map("GRIT_ADMINS" -> " U012AB34, W056CD78 ")),
        Main.admins(Map.empty),
        Main.admins(Map("GRIT_ADMINS" -> "U012AB34, @nick")),
        Main.admins(Map("GRIT_ADMINS" -> "C012AB34"))
      ) ==> (
        Right(Vector(UserId("U012AB34"), UserId("W056CD78"))),
        Right(Vector.empty),
        Left(
          "GRIT_ADMINS: @nick is not a Slack user's id (U… or W…, as their profile's Copy member ID gives it)"
        ),
        Left(
          "GRIT_ADMINS: C012AB34 is not a Slack user's id (U… or W…, as their profile's Copy member ID gives it)"
        )
      )
    }

    test(
      "serving Slack, GRIT_ADMINS declares the administrators group of the team's accounts; unset, or with no Slack, there are none"
    ) {
      def declared(env: Map[String, String], slackIn: Option[TeamId]) =
        Main
          .deployment(
            env,
            Offered.Read,
            Vector(SlackEdge.serving(Grit, Backfill.Default, ReplyNote.Off)),
            Vector.empty,
            java.time.ZoneOffset.UTC,
            slackIn
          )
          .map(d => (d.visibility.administrators, d.visibility.groups))
      val admins = Map("GRIT_ADMINS" -> "U012AB34")
      (
        declared(admins, Some(TeamId("T0FAKE"))),
        declared(Map.empty, Some(TeamId("T0FAKE"))),
        declared(admins, None)
      ) ==> (
        Right(
          (
            Some(Main.Administrators),
            Vector(
              Group(
                Main.Administrators,
                Set[Account](
                  SlackAccounts
                    .account(TeamId("T0FAKE"), UserId("U012AB34"))
                    .fold(e => throw new java.lang.AssertionError(e), identity)
                )
              )
            )
          )
        ),
        Right((None, Vector.empty)),
        Right((None, Vector.empty))
      )
    }

    test(
      "with no administrators named, serve says no one can lower a label, label a public channel or set a baseline"
    ) {
      (Main.unadministered(Vector.empty), Main.unadministered(Vector(UserId("U012AB34")))) ==> (
        Some(
          "GRIT_ADMINS is unset: no one can lower a room's label, label a public channel or set a room's baseline"
        ),
        None
      )
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

    test(
      "the reply note is GRIT_SLACK_REPLY_NOTE, strength when unset, each word its note, any other refused naming the words"
    ) {
      Vector(None, Some("off"), Some(" strength-cost "), Some("cost"))
        .map(v => Main.replyNote(v.fold(Map.empty)(w => Map("GRIT_SLACK_REPLY_NOTE" -> w)))) ==>
        Vector(
          Right(ReplyNote.Strength),
          Right(ReplyNote.Off),
          Right(ReplyNote.StrengthAndCost),
          Left("GRIT_SLACK_REPLY_NOTE is one of off, strength, strength-cost, not 'cost'")
        )
    }

    test(
      "the days backfill reads are GRIT_BACKFILL_DAYS, 2 when unset, refused unless a whole number above zero"
    ) {
      (
        Main.backfillDays(Map.empty),
        Main.backfillDays(Map("GRIT_BACKFILL_DAYS" -> " 7 ")),
        Main.backfillDays(Map("GRIT_BACKFILL_DAYS" -> "0")),
        Main.backfillDays(Map("GRIT_BACKFILL_DAYS" -> "1.5"))
      ) ==> (
        Right(2),
        Right(7),
        Left("GRIT_BACKFILL_DAYS is a whole number of days above zero, not '0'"),
        Left("GRIT_BACKFILL_DAYS is a whole number of days above zero, not '1.5'")
      )
    }

    test(
      "serve bounds a join's backfill by GRIT_BACKFILL_DAYS, GRIT_BACKFILL_MESSAGES and GRIT_BACKFILL_JOINS_PER_DAY, the default's where unset, each refused by name unless a whole number above zero"
    ) {
      (
        Main.backfill(Map.empty),
        Main
          .backfill(
            Map(
              "GRIT_BACKFILL_DAYS" -> "3",
              "GRIT_BACKFILL_MESSAGES" -> " 50 ",
              "GRIT_BACKFILL_JOINS_PER_DAY" -> "4"
            )
          )
          .map(b => (b.days, b.messages, b.joinsPerDay)),
        Main.backfill(Map("GRIT_BACKFILL_MESSAGES" -> "0")),
        Main.backfill(Map("GRIT_BACKFILL_JOINS_PER_DAY" -> "ten"))
      ) ==> (
        Right(Backfill.Default),
        Right((3, 50, 4)),
        Left("GRIT_BACKFILL_MESSAGES is a whole number above zero, not '0'"),
        Left("GRIT_BACKFILL_JOINS_PER_DAY is a whole number above zero, not 'ten'")
      )
    }

    test(
      "github's caps hold the member repositories at member, and the rest of GitHub at untrusted; a repository not owner/name is refused naming the variable"
    ) {
      def capped(env: Map[String, String]) =
        Main
          .deployment(env, Offered.Read, Vector.empty, Vector.empty, java.time.ZoneOffset.UTC)
          .map(d =>
            Vector(
              "service:github/octo/widgets",
              "service:github/octo/gadgets",
              "service:github/octo/other",
              "service:github"
            ).map(p => Place.read(p).toOption.flatMap(d.visibility.capOf).map(Integrity.written))
          )
      (
        capped(Map("GRIT_GITHUB_MEMBER_REPOS" -> " Octo/Widgets , octo/gadgets ,")),
        capped(Map.empty),
        capped(Map("GRIT_GITHUB_MEMBER_REPOS" -> "widgets"))
      ) ==> (
        Right(Vector(Some("member"), Some("member"), Some("untrusted"), Some("untrusted"))),
        Right(Vector.fill(4)(Some("untrusted"))),
        Left("GRIT_GITHUB_MEMBER_REPOS: a repository is {owner}/{name}, not 'widgets'")
      )
    }

    test("the GitHub server reads its answers' items by their authors") {
      val issue = ujson.Obj(
        "issue" -> ujson.Obj(
          "author_association" -> "MEMBER",
          "html_url" -> "https://github.com/octo/widgets/issues/1"
        )
      )
      val result =
        ujson.Obj("content" -> ujson.Arr(ujson.Obj("type" -> "text", "text" -> ujson.write(issue))))
      val called = McpCalled("issue_read", ujson.Obj("method" -> "get"), result)
      Main
        .githubServer(Map("GITHUB_MCP_TOKEN" -> "never-read"))
        .map(_.accounts.items(called)) ==> Right(
        Itemised.Whole(
          Accounted.of(
            Vector(
              Provenance(
                Place.read("service:github/octo/widgets").fold(sys.error, identity),
                Integrity.Member
              )
            )
          )
        )
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
        Main.githubServer(env).map(s => (s.name, s.endpoint.toString, s.credential, s.allow)),
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
            Credential.static(Variable("GITHUB_MCP_TOKEN")),
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

    test(
      "with GRIT_DRIVE_SA_KEY set, serve serves Drive at service:drive, reached from Slack's conversations, and its folders named by their links; an unclaimed domain, or a writer that is no address, is refused naming its variable; unset, no Drive edge"
    ) {
      val env = DriveEnv
      (
        Main
          .drive(env)
          .map(
            _.map(d =>
              (
                EdgeName.value(d.edge.name),
                d.edge.needs,
                d.reaches.within.written,
                d.reaches.service.place.written,
                d.names
                  .place("https://drive.google.com/drive/folders/0AbCdEfGhIjKlUk9PVA")
                  .map(_.written)
              )
            )
          ),
        Main.drive(env + ("GRIT_DRIVE_DOMAIN" -> "elsewhere.com")).map(_ => ()),
        Main
          .drive(env + ("GRIT_DRIVE_WRITERS" -> "bot@vendor.example, not-an-address"))
          .map(_ => ()),
        Main.drive(env - "GRIT_DRIVE_SA_KEY")
      ) ==> (
        Right(
          Some(
            (
              "drive",
              Vector(Variable("GRIT_DRIVE_SA_KEY")),
              "slack:",
              "service:drive",
              Some("service:drive/0AbCdEfGhIjKlUk9PVA")
            )
          )
        ),
        Left("GRIT_DRIVE_DOMAIN: Drive's domain elsewhere.com is not one the deployment claims"),
        Left(
          "GRIT_DRIVE_WRITERS: an email address is one @ with something on either side: not-an-address"
        ),
        Right(None)
      )
    }

    test(
      "serving GitHub and Drive, a Slack conversation works in GitHub and its addressed turns reach Drive beside it"
    ) {
      val conversation = Place.under(Namespace.Slack, Vector("T0123", "C0456"))
      val served = for {
        github <- Main.github(Map("GITHUB_MCP_TOKEN" -> "never-read"))
        drive <- Main.drive(DriveEnv)
      } yield {
        val (worksIn, reaches) = Main.links(github.map(_._2), drive)
        (
          WorksIn.of(worksIn, conversation).map(_.place.written),
          Reaches.of(reaches, conversation).map(_.place.written)
        )
      }
      served ==> Right((Some("service:github"), Vector("service:drive")))
    }

    test(
      "with GRIT_DRIVE_SA_KEY set, the deployment invites service:drive, labels it internal, believes it at member and clears Drive for its top label; unset, none of these"
    ) {
      val drive = Service.of("drive").fold(sys.error, identity)
      val folder = Place.read("service:drive/folder").fold(sys.error, identity)
      def declared(env: Map[String, String]) =
        Main
          .deployment(env, Offered.Read, Vector(Quiet), Vector.empty, java.time.ZoneOffset.UTC)
          .map { d =>
            val v = d.visibility
            (
              v.invites(folder),
              Label.written(v.rooms.label(Room(drive.place, None)).label),
              Integrity.written(v.outputOf(drive)),
              Label.written(v.clearances.of(Party.Service(drive)))
            )
          }
      (declared(DriveEnv), declared(Map.empty)) ==> (
        Right((true, "internal", "member", Label.written(Compartments.Shipped.top))),
        Right((false, "public", "untrusted", "public"))
      )
    }
  }

  /** A service account's key, as Google issues one, for a fresh RSA key pair. */
  private lazy val DriveKey: String = {
    val generator = java.security.KeyPairGenerator.getInstance("RSA")
    generator.initialize(2048)
    val pem = java.util.Base64
      .getMimeEncoder(64, "\n".getBytes(java.nio.charset.StandardCharsets.UTF_8))
      .encodeToString(generator.generateKeyPair().getPrivate.getEncoded)
    ujson.write(
      ujson.Obj(
        "type" -> "service_account",
        "client_email" -> "grit-drive@project.iam.example",
        "private_key" -> s"-----BEGIN PRIVATE KEY-----\n$pem\n-----END PRIVATE KEY-----\n",
        "token_uri" -> "https://oauth2.example/token"
      )
    )
  }

  /** Drive declared: its key, a claimed domain, and one writer that is no person. */
  private lazy val DriveEnv: Map[String, String] = Map(
    "GRIT_DRIVE_SA_KEY" -> DriveKey,
    "GRIT_CLAIMED_DOMAINS" -> "example.com",
    "GRIT_DRIVE_DOMAIN" -> "example.com",
    "GRIT_DRIVE_WRITERS" -> "bot@vendor.example"
  )
}
