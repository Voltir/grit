package grit.app.main

import grit.app.config.Claimed
import grit.app.config.Listed.csv
import grit.core.admin.PlaceNames
import grit.core.edge.{ServedEdge, Variable}
import grit.core.identity.{Domain, Email, Identities}
import grit.core.place.{Namespace, Place, Reaches, Service, WorksIn}
import grit.core.visibility.Integrity
import grit.mcp.client.{Credential, KeyFiles, McpServer}
import grit.mcp.drive.{DriveEdge, DriveReading, DriveRest}
import grit.mcp.edge.McpEdge
import grit.mcp.github.GitHubItems
import grit.mcp.scope.McpScope
import grit.slack.edge.{ReplyNote, SlackCommand}

/** The edges `grit serve` serves besides Slack's events: Slack's command and reply note, and
  * GitHub's and Drive's tools.
  */
private[main] object ServedEdges {

  /** The variable naming the slash command `grit serve` answers, as registered for its Slack
    * app.
    */
  private[main] val CommandVar = "GRIT_SLACK_COMMAND"

  /** The slash command `env` says `grit serve` answers ([[CommandVar]]), `/grit` when unset;
    * why not, naming the variable, when Slack would not register it.
    */
  private[main] def slackCommand(env: Map[String, String]): Either[String, SlackCommand] =
    SlackCommand.of(env.getOrElse(CommandVar, "/grit")).left.map(why => s"$CommandVar: $why")

  /** The variable naming what `grit serve`'s Slack edge notes under a reply ([[ReplyNote]]). */
  private[main] val NoteVar = "GRIT_SLACK_REPLY_NOTE"

  /** The note `env` names ([[NoteVar]]), [[ReplyNote.Default]] when unset; why not, naming the
    * variable and the words, when it names none.
    */
  private[main] def replyNote(env: Map[String, String]): Either[String, ReplyNote] =
    env.get(NoteVar) match {
      case None => Right(ReplyNote.Default)
      case Some(raw) =>
        ReplyNote
          .named(raw.trim)
          .toRight(
            s"$NoteVar is one of ${ReplyNote.values.map(_.word).mkString(", ")}, not '$raw'"
          )
    }

  /** The variable whose token `grit serve` sends GitHub's MCP server; unset, it serves no
    * GitHub edge.
    */
  private[main] val GithubTokenVar = "GITHUB_MCP_TOKEN"

  /** The variable naming GitHub's MCP endpoint, [[GithubUrl]] when unset. */
  private[main] val GithubUrlVar = "GRIT_GITHUB_MCP_URL"

  /** GitHub's hosted MCP server, listing only the tools that read. */
  private[main] val GithubUrl = "https://api.githubcopilot.com/mcp/readonly"

  /** The variable naming, comma-separated as `{owner}/{name}`, the GitHub repositories whose
    * items are believed up to member: each repository named here is one where every account
    * that can write is a realm member, and whose default branch takes only their merges. Unset,
    * it names none.
    */
  private[main] val GithubMemberReposVar = "GRIT_GITHUB_MEMBER_REPOS"

  /** The variable naming the GitHub tools offered, comma-separated; [[GithubTools]] when unset. */
  private[main] val GithubToolsVar = "GRIT_GITHUB_TOOLS"

  /** The GitHub tools offered when [[GithubToolsVar]] is unset: files, commits, code search,
    * issues and pull requests, kept to these so their schemas stay small in every call.
    */
  private[main] val GithubTools: Set[String] = Set(
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

  /** GitHub's MCP server as `env` declares it: named `github`, at [[GithubUrlVar]], its token
    * [[GithubTokenVar]], allowing the tools [[GithubToolsVar]] names, its scope
    * [[McpScope.Open]]: every allowed tool offered and every call sent, reaching whatever the
    * token reaches; its answers' items read by [[GitHubItems.accounts]]; why not, naming the variable, when the URL is not one [[McpServer.of]]
    * takes or the list names no tool.
    */
  private[main] def githubServer(env: Map[String, String]): Either[String, McpServer] = {
    val allow =
      if (env.contains(GithubToolsVar)) csv(env, GithubToolsVar).toSet else GithubTools
    for {
      _ <- Either.cond(allow.nonEmpty, (), s"$GithubToolsVar names no tool")
      github <- Service.of("github")
      server <- McpServer
        .of(
          "github",
          env.getOrElse(GithubUrlVar, GithubUrl),
          Credential.static(grit.core.edge.Variable(GithubTokenVar)),
          McpScope.Open,
          allow,
          GitHubItems.accounts(github)
        )
        .left
        .map(why => s"$GithubUrlVar: $why")
    } yield server
  }

  /** Where every Slack conversation lies: the place a link from all of them is declared at. */
  private[main] val SlackRoot: Place = Place.under(Namespace.Slack, Vector.empty)

  /** What `grit serve` serves of GitHub: its edge, and the link that makes every Slack
    * conversation work in its service.
    */
  private[main] final case class Github(edge: ServedEdge, worksIn: WorksIn)

  /** The GitHub edge `grit serve` serves when `env` sets [[GithubTokenVar]] ([[McpEdge]], over
    * [[githubServer]]); `None` when the token is unset; why not as [[githubServer]] says.
    */
  private[main] def github(env: Map[String, String]): Either[String, Option[Github]] =
    if (!env.contains(GithubTokenVar)) Right(None)
    else
      for {
        service <- Service.of("github")
        server <- githubServer(env)
        edge <- McpEdge.serving(service, Vector(server))
      } yield Some(Github(edge, WorksIn(SlackRoot, service)))

  /** The variable naming the absolute path of the JSON key file of the service account Drive's
    * files are read as (`~` is not expanded); unset, `grit serve` serves no Drive edge.
    */
  private[main] val DriveKeyVar = "GRIT_DRIVE_SA_KEY"

  /** The variable naming the domain whose addresses are Drive's people: one
    * `GRIT_CLAIMED_DOMAINS` lists.
    */
  private[main] val DriveDomainVar = "GRIT_DRIVE_DOMAIN"

  /** The variable listing, comma-separated, the exact addresses of accounts that are no person
    * (another service's) whose changes to a file are believed at member; none when unset.
    */
  private[main] val DriveWritersVar = "GRIT_DRIVE_WRITERS"

  /** The service Drive's files are kept at: `service:drive`. */
  private[main] val DriveAt: Either[String, Service] = Service.of("drive")

  /** What `grit serve` serves of Drive: its edge, the link that offers its tools to every Slack
    * conversation's turns addressed to grit (beside the service a conversation works in, such as
    * GitHub's), and how a command names its folders.
    */
  private[main] final case class Drive(edge: ServedEdge, reaches: Reaches, names: PlaceNames)

  /** The links `grit serve` declares, from what it serves of GitHub and Drive: the
    * conversations working in a service, and those reaching one beside it.
    */
  private[main] def links(
      github: Option[Github],
      drive: Option[Drive]
  ): (Vector[WorksIn], Vector[Reaches]) =
    (github.map(_.worksIn).toVector, drive.map(_.reaches).toVector)

  /** The Drive edge `grit serve` serves when `env` sets [[DriveKeyVar]] ([[DriveEdge]], over
    * Drive's read-only scope): the files of every shared drive shared with the key's service
    * account, read as [[DriveReading]] says, its people those of [[DriveDomainVar]], its own
    * address the key's, and [[DriveWritersVar]]'s addresses believed at member; `None` when the
    * key is unset. The key is read from `files`, as [[Credential.open]] reads it. Why not,
    * naming the variable: the key's file unread or not a service account's key, the domain
    * unset or not one claimed, or a writer not an address, in the domain, or the key's own.
    */
  private[main] def drive(
      env: Map[String, String],
      files: KeyFiles
  ): Either[String, Option[Drive]] =
    if (!env.contains(DriveKeyVar)) Right(None)
    else {
      val credential = Credential.serviceAccount(Variable(DriveKeyVar), DriveRest.ReadOnly)
      for {
        service <- DriveAt
        self <- Credential
          .account(credential, env, files)
          .left
          .map(_.message)
          .flatMap(_.toRight(s"$DriveKeyVar names no service account"))
        domain <- env
          .get(DriveDomainVar)
          .toRight(s"$DriveDomainVar is unset")
          .flatMap(Domain.of)
          .left
          .map(why => s"$DriveDomainVar: $why")
        writers <- csv(env, DriveWritersVar)
          .foldLeft[Either[String, Vector[Email]]](Right(Vector.empty))((kept, w) =>
            kept.flatMap(ws => Email.of(w).map(ws :+ _))
          )
          .left
          .map(why => s"$DriveWritersVar: $why")
        claimed <- Claimed.fromEnv(env)
        identities <- Identities.of(Vector.empty, claimed).left.map(_.message)
        reading <- DriveReading
          .of(service, domain, identities, self, writers.map(_ -> Integrity.Member).toMap)
          .left
          .map {
            case r: DriveReading.Misdeclared.Unclaimed => s"$DriveDomainVar: ${r.message}"
            case r => s"$DriveWritersVar: ${r.message}"
          }
        edge <- DriveEdge.serving(credential, reading)
      } yield Some(
        Drive(edge, Reaches(SlackRoot, service), reading.names)
      )
    }
}
