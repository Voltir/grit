package grit.app.main

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.app.chat.{ChatHost, ChatScreen, Replies}
import grit.app.config.Listed.csv
import grit.app.config.{Budgets, Claimed, Durations, Lifecycle, Prefs}
import grit.app.look.Theme
import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.assembly.retrieval.RetrievalAssembler
import grit.core.admin.PlaceNames
import grit.core.clock.{Clock, Fresh}
import grit.core.edge.{ServedEdge, Variable}
import grit.core.id.{CorpusName, PluginName, PrincipalId, ScheduleKey, SourceId, TurnRef}
import grit.core.identity.{Account, Domain, Email, Identities, Vouching}
import grit.core.job.SlotRule
import grit.core.message.{Message, Tokens}
import grit.core.model.{Ladder, ModelId, Policy}
import grit.core.period.LifecycleSettings
import grit.core.persona.Persona
import grit.core.place.{Directory, Namespace, Place, Reaches, Service, WorksIn}
import grit.core.plugin.Plugin
import grit.core.prompt.Fragment
import grit.core.speech.Speaking
import grit.core.store.Origin
import grit.core.tool.ToolName
import grit.core.tool.ToolSet
import grit.core.triage.Corpus
import grit.core.visibility.{
  Cap,
  Cleared,
  Compartments,
  Group,
  GroupName,
  Integrity,
  Label,
  Labelled,
  Level,
  Party,
  RoomLabels,
  Visibility
}
import grit.dbos.engine.{Engine, EngineLock, Link, NotTaken, Unopened}
import grit.digest.Digest
import grit.edge.{PlaceFragments, Server}
import grit.host.{LocalEdits, LocalInstructions, LocalMachine, LocalShell, LocalWorkspace}
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Placer}
import grit.kit.environment.{DotEnv, Secrets}
import grit.kit.run.{Kit, Launch}
import grit.knowledge.drive.DriveCards
import grit.knowledge.github.GitHubCards
import grit.knowledge.plugin.{
  CardSource,
  Knowledge,
  StandingDocuments,
  StrandDocuments,
  TopicDocuments
}
import grit.knowledge.sync.SyncSchedule
import grit.mcp.client.{Credential, KeyFiles, McpServer}
import grit.mcp.drive.{DriveEdge, DriveReading, DriveRest, DriveTools}
import grit.mcp.edge.McpEdge
import grit.mcp.github.GitHubItems
import grit.mcp.scope.McpScope
import grit.models.{
  JevClassifier,
  JevConfig,
  ModelRole,
  OpenRouterConfig,
  Seed,
  StubClassifier,
  StubModels,
  StubProvider
}
import grit.remind.Reminders
import grit.slack.edge.{Backfill, ReplyNote, SlackAccounts, SlackCommand, SlackEdge}
import grit.slack.event.{TeamId, UserId}
import grit.tools.Coding
import grit.tui.runtime.app.{Host, Mailbox}
import grit.tui.runtime.loop.Runtime
import grit.turn.TurnLoop

// doc-length: the program's manual, naming every variable and mode it reads
/** grit, against the Postgres named by `GRIT_DATABASE_*` (see [[DbConfig]]). The model is
  * OpenRouter's when `OPENROUTER_API_KEY` is set: each role's model, budget and upstream as
  * the seed catalog's policy ([[Seed]]) says, overridden for the run by the role's variables
  * ([[ModelRole]], [[OpenRouterConfig.policy]]), and its tool schemas strict when the turn's
  * pair is known to enforce them. The stub otherwise, answering turns after `GRIT_STUB_DELAY_MS`
  * (default 0). Each turn's window fits in `GRIT_WINDOW_TOKENS`
  * estimated tokens (default [[LinearAssembler.DefaultBudget]]) and is chosen by
  * `GRIT_ASSEMBLER`: `retrieval` (the default), the recent turns that fit in
  * `GRIT_TAIL_TOKENS` (default [[RetrievalAssembler.DefaultTail]]) plus the earlier turns a
  * written query finds ([[RetrievalAssembler]]); or `linear`, the recent turns that fit. Every variable
  * may come from a `.env` file instead ([[DotEnv]]). Each message is placed on the
  * conversation's agenda by Jev when `JEV_API_KEY` is set ([[JevConfig]]); without it, by
  * the stub classifier when `GRIT_STUB_TOPICS=1` (for the gate), and otherwise by none,
  * which leaves each message in the item it is in. The TUI starts in the theme
  * `GRIT_THEME` names, or else the one last chosen with `/theme` ([[Prefs]]). Each turn's
  * model is offered the tools `GRIT_TOOLS` names ([[Offered]]) in at most
  * `GRIT_TOOL_ROUNDS` model calls (default [[DefaultToolRounds]], at least 2), the last with
  * tools off. The coding tools are hosted (ADR 0017): offered only when an edge serves the
  * conversation's directory, and run by that edge. The TUI is the edge for the directory it
  * runs in, with its instruction files; a command it runs sees only the environment
  * `LocalShell` passes. The engine sweeps every `GRIT_SWEEP` (default
  * [[DefaultSweep]]), asking the same classifier as the placer whether anyone is waiting on
  * each quiet period ([[Settle]]), closing each period whose deadline has come ([[Close]]), its
  * closing written by the summary role and gated by that classifier, and posting each closed
  * period to the plugins `GRIT_PLUGINS` turns on ([[pluginChoice]], [[Posting]]; with
  * Digest on, each turn's model is offered `recent_activity`; with the reminders on,
  * `remind_me`, `reminders` and `cancel_reminder`; with knowledge on, a card of each Markdown
  * file, and of each directory holding one, of the GitHub repository `GRIT_GITHUB_OWNER`/`GRIT_GITHUB_REPO` at
  * `GRIT_GITHUB_REF`, and of each Doc, Sheet and text file of Drive's shared drives at each
  * label `GRIT_DRIVE_CARD_LABELS` lists, synced daily, [[knowledge]]; with standing on, one
  * document of each close's person- and tool-established Standing lines but reminders'). When a period is asked about
  * and closes, what its balance holds, and which other places' open periods a window draws
  * on, are data in the database: on every start the engine writes them from
  * `GRIT_IDLE`, `GRIT_SETTLE`, `GRIT_RESOLVE_AT`, `GRIT_ASKS`, `GRIT_RETENTION`,
  * `GRIT_LEDGER`, `GRIT_BALANCE`, `GRIT_SCOPE` (under `grit serve`, `room` when unset:
  * [[Lifecycle.ServeScope]]) and `GRIT_WEIGHT` ([[Lifecycle.fromEnv]]), and logs them; `/set`
  * (or SQL) changes them from the next sweep and turn on, until the next start. The email
  * domains `GRIT_CLAIMED_DOMAINS` lists ([[Claimed]]) are the deployment's own; serving
  * Slack, it trusts Slack to say who the people of the workspace its bot token is installed in
  * are ([[SlackEdge.installedIn]]), and no one otherwise; `grit serve` and `grit backfill` end
  * what its identities no longer trust as they start, and the chat and a run with arguments
  * end nothing ([[ownEngine]]).
  *
  * One grit runs the engine of a database (ADR 0015): a second, in either mode, attaches to
  * it: its TUI serves its own directory and shows its conversations, the header saying
  * `attached`, and the status line `engine gone` while no engine runs; its messages are
  * then kept, and their turns run when one does.
  *
  *   - **No arguments: the chat TUI**, over the conversation `GRIT_SESSION` names
  *     (default `default`) in the directory grit runs in: the same name in another
  *     directory is another conversation. Logs go to `GRIT_LOG` (default `grit-tui.log` in the temp
  *     directory), never to the screen.
  *   - **`serve` alone: `grit serve`**, the engine of the database and the Slack edge in its
  *     process ([[Kit.serve]], [[SlackEdge.serving]]; ADR 0023), over Socket Mode with `SLACK_BOT_TOKEN` and
  *     `SLACK_APP_TOKEN`, until stopped. It hears every channel, public or private, its bot is
  *     invited to, what is not said to it included, until the bot is removed. When the bot joins a
  *     channel it hears what was said there before, never answering it: the newest
  *     `GRIT_BACKFILL_MESSAGES` (default 100) of the `GRIT_BACKFILL_DAYS` (default 2) before
  *     the join, for at most `GRIT_BACKFILL_JOINS_PER_DAY` (default 10) joins a day, and none
  *     while the day's spend is over `GRIT_DAILY_USD` ([[backfill]], [[Backfill]]). It answers the slash command
  *     `GRIT_SLACK_COMMAND` names (default `/grit`, as registered for the Slack app) to its
  *     asker alone ([[SlackEdge.command]]), and notes under a reply answered above standard what
  *     `GRIT_SLACK_REPLY_NOTE` says ([[replyNote]]: `off`, `strength` by default, or
  *     `strength-cost`); the Slack users `GRIT_ADMINS` names (ids,
  *     comma-separated) administer ([[visibility]]), and with none it says at start that no one
  *     can lower a room's label, label a public channel or set a room's baseline. Its tools are `read`'s, as in a run with arguments:
  *     nothing in Slack answers a gated call yet, so `GRIT_TOOLS=all` is refused. It never
  *     attaches: another grit holding the database's engine stops it. Give it a database of
  *     its own (`GRIT_DATABASE_URL`): everyone in the workspace sees what that database holds.
  *     With `GITHUB_MCP_TOKEN` set it also serves GitHub's read-only tools at
  *     `service:github`, and every Slack conversation works there ([[github]]): from the MCP
  *     server at `GRIT_GITHUB_MCP_URL` (default [[GithubUrl]]), the tools `GRIT_GITHUB_TOOLS`
  *     names (comma-separated; default [[GithubTools]]). It refuses to start when that server
  *     cannot be reached, refuses the token, or offers none of those tools. With
  *     `GRIT_DRIVE_SA_KEY` set, the path of a service account's key file, it also serves
  *     Drive's files at `service:drive` ([[drive]]), offered to every Slack conversation's
  *     turns addressed to grit, its folders named in the slash command by their links; it
  *     refuses to start when the key's file cannot be read or Drive refuses the key.
  *   - **`backfill` alone, or with `--yes`: `grit backfill`**, run before `grit serve` on its
  *     database ([[Kit.catchUp]], [[SlackEdge.backfill]]; ADR 0020): what each channel its bot is in said over
  *     the last `GRIT_BACKFILL_DAYS` (default 2) that grit has not recorded, heard at the time
  *     it was said, and closed as it would have closed. It prints each channel's estimate and the
  *     label it is heard at, and asks before hearing anything (`--yes` does not ask); nothing caps what it spends. It
  *     takes serve's tokens and variables, and, like serve, never attaches. Threads still
  *     inside idle are left for serve.
  *   - **Other arguments: each is a message**, answered by one turn and printed. Repeat a
  *     message to watch a redelivery come back as the same turn; run again to watch
  *     finished turns replay without calling the model. Nothing here answers a gated call,
  *     so `GRIT_TOOLS=all` is refused.
  */
object Main {

  /** The argument runs share one conversation, apart from any TUI session. */
  private val RunOrigin: Origin = Origin.Task("m0", "main")

  def main(args: Array[String]): Unit = {
    val tui = args.isEmpty
    // `grit serve` alone: the engine and the Slack edge (a lone message "serve" is this).
    val serving = args.sameElements(Array("serve"))
    // `grit backfill`, and `--yes` to hear without asking.
    val backfilling =
      args.sameElements(Array("backfill")) || args.sameElements(Array("backfill", "--yes"))
    val slack = serving || backfilling
    // `.env` in the working directory (GRIT_ENV_FILE to name another), under the real
    // environment: a variable set in both takes the environment's value.
    val envFile = java.nio.file.Path.of(sys.env.getOrElse("GRIT_ENV_FILE", ".env"))
    val env = exitOnLeft(DotEnv.load(envFile, sys.env))
    val log = env.getOrElse(
      "GRIT_LOG",
      java.nio.file.Path.of(System.getProperty("java.io.tmpdir"), "grit-tui.log").toString
    )
    // Before anything loads slf4j (DBOS does): a log line on stderr would paint over the
    // screen.
    if (tui) { val _ = System.setProperty("org.slf4j.simpleLogger.logFile", log) }
    // The Slack SDK logs request bodies at debug: never below info.
    if (slack) { val _ = System.setProperty("org.slf4j.simpleLogger.log.com.slack.api", "info") }

    val offered = exitOnLeft(toolChoice(env, chat = tui))
    // `grit serve` also serves GitHub's tools when GITHUB_MCP_TOKEN is set, Slack's
    // conversations working there.
    val githubEdge = if (serving) exitOnLeft(github(env)) else None
    // And Drive's, when GRIT_DRIVE_SA_KEY is set, its folders named in Slack's commands.
    val driveEdge = if (serving) exitOnLeft(drive(env, KeyFiles.Local)) else None
    val edges: Vector[ServedEdge] =
      // Slack's slash command, GRIT_SLACK_COMMAND, as registered for its app.
      (if (slack)
         Vector(
           SlackEdge.serving(
             exitOnLeft(slackCommand(env)).naming(
               PlaceNames.of(driveEdge.map(_.names).toVector)
             ),
             exitOnLeft(backfill(env)),
             exitOnLeft(replyNote(env))
           )
         )
       else Vector.empty) ++ githubEdge.map(_.edge) ++ driveEdge.map(_.edge)
    // Days begin at this machine's midnight (OpenRouter's own daily figure is UTC's).
    // Serving Slack, the workspace its bot token is installed in is trusted to say who its
    // people are.
    val slackIn =
      if (slack) Some(exitOnLeft(SlackEdge.installedIn(env).left.map(_.message))) else None
    val (worksIn, reaches) = links(githubEdge, driveEdge)
    val deployment =
      exitOnLeft(
        Main.deployment(
          env,
          offered,
          edges,
          worksIn,
          java.time.ZoneId.systemDefault(),
          slackIn,
          reaches
        )
      )
    val secrets = exitOnLeft(Secrets.of(env, deployment).left.map(_.message))
    val config = secrets.database
    val budget = deployment.assembly match {
      case Assembly.Retrieval(window, _) => window
      case Assembly.Linear(window) => window
    }
    // The checkout the turn's tools read is the one grit runs in, its links resolved, so a
    // directory is one place however it was reached.
    val root = exitOnLeft(
      scala.util
        .Try(java.nio.file.Path.of("").toAbsolutePath.toRealPath())
        .toEither
        .left
        .map(e => s"the working directory cannot be read: $e")
    )
    val directory = exitOnLeft(Directory.of(root.toString))
    val session = env.getOrElse("GRIT_SESSION", "default")
    val origin = if (tui) Origin.Tui(directory, session) else RunOrigin
    // The file tools the turn is offered, as its prompt's reach describes them: built over
    // the checkout here only to be described; the engine builds its own.
    val hosted = exitOnLeft(
      (offered match {
        case Offered.Read => Coding.readOnly(new LocalWorkspace(root)).map(_.set)
        case Offered.All =>
          Coding
            .all(new LocalWorkspace(root), new LocalEdits(root), new LocalShell(root, Map.empty))
            .map(_.set)
      }).left.map(d => s"the coding tools offer ${ToolName.value(d.name)} twice")
    )
    val instructions = PlaceFragments.of(new LocalInstructions().around(directory), CharEstimate)
    val prefsFile = Prefs.path(env)
    val startTheme = exitOnLeft(theme(env, prefsFile.fold(Prefs.empty)(Prefs.load)))
    // The stub answers the turn after GRIT_STUB_DELAY_MS, a slow model to watch for free.
    val stubDelay = exitOnLeft(millis(env, StubDelayVar))
    val modelName = secrets.openRouter.fold(StubProvider.Model)(_ =>
      ModelId.value(deployment.policy.turn.first.assignment.ref.model)
    )
    // A run with arguments prints each model call, so a replayed turn is visibly one that
    // did not call.
    val run = Launch.Run(announced = !tui, stubDelay)

    // This process, as the engine's row and this edge's registration name it.
    val identity = LocalMachine.identity()
    // The process's own time and fresh values, read here at its root and passed down.
    val clock: Clock^ = Clock.system() // clock-check: the reference deployment's composition root
    val fresh: Fresh^ = Fresh.random()
    val failure: Option[String] =
      if (serving) {
        // Who may lower a label or label a public channel, said before serving starts.
        unadministered(exitOnLeft(admins(env))).foreach(line => System.err.println(s"[main] $line"))
        // Which roles a labelled room cannot call, said before serving starts.
        if (env.contains(OpenRouterConfig.KeyVar))
          uncleared(deployment.policy).foreach(line => System.err.println(s"[main] $line"))
        Kit.serve(deployment, env).left.map(_.message).swap.toOption
      } else if (backfilling)
        backfillDays(env)
          .flatMap { days =>
            Kit
              .catchUp(
                deployment,
                SlackEdge.backfill(days),
                env,
                question =>
                  args.contains("--yes") || {
                    print(s"$question [y/N] ")
                    Option(scala.io.StdIn.readLine())
                      .exists(a => Set("y", "yes").contains(a.trim.toLowerCase))
                  },
                println
              )
              .left
              .map(_.message)
          }
          .swap
          .toOption
      else if (tui) {
        // The lock first, before anything paints (ADR 0015): held, this grit is the engine;
        // refused, it attaches to the one that holds it, and serves its own directory.
        val (mode, opener) = EngineLock.take(config) match {
          case Right(lock) =>
            val engine = new ChatHost.Opener {
              // The screen paints first; the engine opens behind it, on the host's thread.
              def open(): Link^ = {
                val started =
                  Engine.start(
                    config,
                    lock,
                    deployment.runsUnder,
                    identity,
                    deployment.budget,
                    deployment.visibility,
                    clock
                  ) match {
                    case Right(e) => e
                    case Left(refused) => sys.error(refused.message(clock.now()))
                  }
                try {
                  val running = ownEngine(started, deployment, secrets, run)
                  serveHere(running, Place.of(directory), hosted, instructions, offered)
                  running
                } catch {
                  case e: Throwable =>
                    // DBOS's threads are non-daemon: an engine that is not handed on is closed.
                    started.close()
                    throw e
                }
              }
            }
            ("engine", engine)
          case Left(NotTaken.Held(holder)) =>
            val attached = new ChatHost.Opener {
              def open(): Link^ = {
                val link =
                  Link.attach(
                    config,
                    deployment.runsUnder,
                    identity,
                    deployment.budget,
                    deployment.visibility,
                    clock
                  )
                serveHere(link, Place.of(directory), hosted, instructions, offered)
                link
              }
            }
            (
              holder.fold("attached")(h => s"attached · engine on ${h.machine} pid ${h.pid}"),
              attached
            )
          case Left(refused) =>
            System.err.println(s"[main] ${refused.message(clock.now())}")
            sys.exit(1)
        }
        val host = new ChatHost(
          origin,
          opener,
          CharEstimate,
          clock,
          fresh,
          Some(java.nio.file.Path.of(log))
        )
        // Closing the host stops following and closes the engine, however far it got.
        try
          Runtime.run(
            new ChatScreen.App(
              modelName,
              startTheme,
              budget,
              s"$session · ${written(root)} · $mode"
            ),
            keeping(host, prefsFile)
          )
        finally host.close()
        None
      } else
        Kit.open(config, deployment, clock) match {
          case Left(Unopened.Lock(NotTaken.Held(_))) =>
            // Another grit runs the engine: its turns are sent to it, as a TUI's are.
            val link =
              Link.attach(
                config,
                deployment.runsUnder,
                identity,
                deployment.budget,
                deployment.visibility,
                clock
              )
            try say(link, args.toList)
            finally link.close()
          case Left(refused) => Some(refused.message(clock.now()))
          case Right(engine) =>
            try say(ownEngine(engine, deployment, secrets, run), args.toList)
            finally {
              // DBOS's threads are non-daemon: a throw that skips this leaves the JVM, and
              // mill, waiting forever.
              engine.close()
            }
        }

    if (tui) println(s"[main] log: $log")
    failure.foreach { message =>
      System.err.println(s"[main] $message")
      sys.exit(1)
    }
  }

  /** `engine`, the database's engine this process holds for the chat or a run with arguments,
    * with `deployment`'s workflows launched ([[Kit.launch]]). It ends no attestation, whatever
    * `deployment` trusts: only `grit serve` and `grit backfill` do, since they alone serve the
    * Slack attester, and a chat's environment may claim other domains than theirs. Throws as
    * [[Kit.launch]] does.
    */
  private[main] def ownEngine(
      engine: Engine^,
      deployment: Deployment,
      secrets: Secrets,
      run: Launch.Run
  ): Engine^{engine} =
    Kit.launch(engine, deployment, secrets, run)

  /** This process's edge (ADR 0017), registered through `engine` for `place`: it offers
    * `hosted` and the directory's `instructions` there, and runs the requests addressed to it
    * with the tools `offered` picks, each on a virtual thread, until the link closes. A
    * registration that fails is logged, and the turns are then served by no edge here.
    */
  private def serveHere(
      engine: Link^,
      place: Place,
      hosted: ToolSet,
      instructions: Vector[Fragment],
      offered: Offered
  ): Unit = {
    val log = org.slf4j.LoggerFactory.getLogger("grit.edge")
    engine.register(PrincipalId.Local, Set(place)) match {
      case Left(e) => log.warn(s"this edge could not register: ${e.why}")
      case Right(desk) =>
        desk
          .advertise(place, hosted, instructions)
          .left
          .foreach(e => log.warn(s"not advertised: ${e.why}"))
        // The instruction files are read again every AdvertEvery, and advertised when they
        // changed, so an edit reaches the next turn without a restart.
        place.directory.foreach { dir =>
          val _ = Thread.ofVirtual().name("grit-advert").start { () =>
            var sent = instructions
            while (true) {
              Thread.sleep(AdvertEvery.toMillis)
              val now = PlaceFragments.of(new LocalInstructions().around(dir), CharEstimate)
              if (now != sent)
                desk.advertise(place, hosted, now) match {
                  case Right(()) => sent = now
                  case Left(e) => log.warn(s"not advertised: ${e.why}")
                }
            }
          }
        }
        new Server(
          desk,
          new LocalTools(offered),
          run => { val _ = Thread.ofVirtual().name("grit-tool").start(() => run()) },
          said => log.info(said)
        ).serve()
    }
  }

  /** How often this process's edge reads its directory's instruction files again: 5 s. */
  private val AdvertEvery: FiniteDuration = 5.seconds

  /** Each message answered by one turn, printed. */
  private def say(engine: Link^, messages: List[String]): Option[String] = {
    val started = messages.map { text =>
      for {
        // The text is its own source id, so repeating a message redelivers it.
        turn <- engine.inbox.ingest(
          RunOrigin,
          SourceId(text),
          Message.User(text),
          Account.Local
        )
        _ <- engine.inbox.startTurn(turn)
      } yield turn
    }
    started.collectFirst { case Left(error) => error } match {
      case Some(error) => Some(ChatHost.notSent(error))
      case None =>
        started.collect { case Right(turn) => turn }.distinct.foreach { turn =>
          val outcome = engine.awaitTurn(turn)
          val reply = Replies.of(engine, turn).fold(identity, _.getOrElse("(no reply)"))
          println(s"[main] ${describe(turn)} -> $outcome: $reply")
        }
        None
    }
  }

  private val BudgetVar = "GRIT_WINDOW_TOKENS"

  private val StubDelayVar = "GRIT_STUB_DELAY_MS"

  private val TailVar = "GRIT_TAIL_TOKENS"

  private val AssemblerVar = "GRIT_ASSEMBLER"

  /** The token count in `variable`, or `default` when it is unset. */
  /** The milliseconds in `variable`, or 0 when it is unset. */
  private def millis(env: Map[String, String], variable: String): Either[String, Long] =
    env.get(variable) match {
      case None => Right(0L)
      case Some(raw) =>
        raw.trim.toLongOption
          .filter(_ >= 0)
          .toRight(s"$variable is not a non-negative whole number")
    }

  private def tokens(
      env: Map[String, String],
      variable: String,
      default: Tokens
  ): Either[String, Tokens] =
    env.get(variable) match {
      case None => Right(default)
      case Some(raw) =>
        raw.trim.toLongOption
          .filter(_ >= 0)
          .map(Tokens(_))
          .toRight(s"$variable is not a non-negative whole number")
    }

  private val SweepVar = "GRIT_SWEEP"

  /** How often the engine sweeps: closing due periods. */
  private[main] val DefaultSweep: FiniteDuration = 30.seconds

  /** The sweep's interval from `GRIT_SWEEP`, a duration ([[Durations]]) of at least a
    * second; [[DefaultSweep]] when unset.
    */
  private[main] def sweepEvery(env: Map[String, String]): Either[String, FiniteDuration] =
    env.get(SweepVar) match {
      case None => Right(DefaultSweep)
      case Some(raw) =>
        Durations
          .read(raw)
          .left
          .map(why => s"$SweepVar: $why")
          .filterOrElse(_ >= 1.second, s"$SweepVar must be at least a second")
    }

  private val PluginsVar = "GRIT_PLUGINS"

  /** The plugins `GRIT_PLUGINS` turns on, a comma-separated list of names; none when unset.
    * Those there are: `digest` ([[Digest]]), `remind` ([[Reminders]]), `knowledge`
    * ([[knowledge]], under `visibility`, its days beginning in `zone`), `strands`
    * ([[strands]], likewise), `topics` ([[topics]], likewise, reading the knowledge plugin's
    * cards when it is listed, which it must be before `topics`) and `standing` ([[StandingDocuments]], leaving out what a reminder
    * tool's result established).
    */
  private[main] def pluginChoice(
      env: Map[String, String],
      visibility: Visibility,
      zone: java.time.ZoneId
  ): Either[String, Vector[Plugin]] =
    csv(env, PluginsVar).distinct
      .foldLeft[Either[String, Vector[Plugin]]](Right(Vector.empty)) { (acc, raw) =>
        acc.flatMap { done =>
          PluginName.of(raw).left.map(why => s"$PluginsVar: $why").flatMap { name =>
            raw match {
              case "digest" => Right(done :+ new Digest(name))
              case "remind" => Right(done :+ new Reminders(name))
              case "knowledge" =>
                knowledge(name, env, visibility, zone)
                  .map(done :+ _)
                  .left
                  .map(why => s"$PluginsVar: $why")
              case "strands" =>
                strands(name, env, visibility, zone)
                  .map(done :+ _)
                  .left
                  .map(why => s"$PluginsVar: $why")
              case "topics" =>
                val cards = done.collectFirst { case k: Knowledge => k }
                if (cards.isEmpty && csv(env, PluginsVar).contains("knowledge"))
                  Left(
                    s"$PluginsVar: topics reads the knowledge plugin's cards, so knowledge comes before it"
                  )
                else
                  topics(name, env, visibility, zone, cards)
                    .map(done :+ _)
                    .left
                    .map(why => s"$PluginsVar: $why")
              case "standing" =>
                // A reminder's line goes stale at its date, and the reminders keep it: left out
                // whether or not they are on, since a closing can carry one from when they were.
                Right(
                  done :+ new StandingDocuments(
                    name,
                    Reminders.tools.map(_.described.spec.name).toSet
                  )
                )
              case other =>
                Left(
                  s"$PluginsVar: no plugin $other; there are digest, remind, knowledge, strands, topics and standing"
                )
            }
          }
        }
      }

  /** The labels the strands plugin's schedules are declared at, written forms, comma-separated.
    */
  private val StrandLabelsVar = "GRIT_STRAND_LABELS"

  /** The strands plugin `name` ([[StrandDocuments]]), its runs daily at 04:30 in `zone`, one
    * schedule at each label [[StrandLabelsVar]] names; or why not: the variable unset, blank,
    * or naming no label, or [[StrandDocuments.of]]'s refusal under `visibility`.
    */
  private[main] def strands(
      name: PluginName,
      env: Map[String, String],
      visibility: Visibility,
      zone: java.time.ZoneId
  ): Either[String, StrandDocuments] =
    for {
      labels <- labelsOf(env, StrandLabelsVar, "strands")
      plugin <- StrandDocuments
        .of(name, labels, java.time.LocalTime.of(4, 30), zone, visibility)
        .left
        .map(why => s"strands: $why")
    } yield plugin

  /** The labels the topics plugin's schedules are declared at, written forms, comma-separated.
    */
  private val TopicLabelsVar = "GRIT_TOPIC_LABELS"

  /** The last day the topics plugin's nightly runs seed, `yyyy-mm-dd` ([[TopicDocuments.of]]'s
    * `seedUntil`); unset, none do.
    */
  private val TopicSeedVar = "GRIT_TOPICS_SEED_UNTIL"

  /** The topics plugin `name` ([[TopicDocuments]]), its runs daily at 05:00 in `zone`, after
    * the knowledge plugin's syncs and the strands', one schedule at each label
    * [[TopicLabelsVar]] names, reading `cards`' cards when given, seeding through the day
    * [[TopicSeedVar]] names; or why not: the labels' variable unset, blank, or naming no label,
    * the seed's not `yyyy-mm-dd`, or [[TopicDocuments.of]]'s refusal under `visibility`.
    */
  private[main] def topics(
      name: PluginName,
      env: Map[String, String],
      visibility: Visibility,
      zone: java.time.ZoneId,
      cards: Option[Knowledge]
  ): Either[String, TopicDocuments] =
    for {
      labels <- labelsOf(env, TopicLabelsVar, "topics")
      seed <- env.get(TopicSeedVar).map(_.trim).filter(_.nonEmpty) match {
        case None => Right(None)
        case Some(d) =>
          scala.util
            .Try(java.time.LocalDate.parse(d))
            .toOption
            .map(Some(_))
            .toRight(s"topics: $TopicSeedVar is yyyy-mm-dd: $d")
      }
      plugin <- TopicDocuments
        .of(name, labels, java.time.LocalTime.of(5, 0), zone, visibility, cards, seed)
        .left
        .map(why => s"topics: $why")
    } yield plugin

  /** The labels `variable` names, for the plugin `what`; or why not: unset, blank, or a label
    * that does not read.
    */
  private def labelsOf(
      env: Map[String, String],
      variable: String,
      what: String
  ): Either[String, Vector[Label]] =
    Some(csv(env, variable))
      .filter(_.nonEmpty)
      .toRight(s"$what keeps documents at the labels $variable names, and it is unset or blank")
      .flatMap(
        _.foldLeft[Either[String, Vector[Label]]](Right(Vector.empty))((acc, w) =>
          acc.flatMap(done => Label.read(w).map(done :+ _).left.map(why => s"$variable: $why"))
        )
      )

  /** The variable naming the owner of the repository the knowledge plugin reads. */
  private val GithubOwnerVar = "GRIT_GITHUB_OWNER"

  /** The variable naming the repository the knowledge plugin reads, under its owner. */
  private val GithubRepoVar = "GRIT_GITHUB_REPO"

  /** The variable naming the ref (a branch, tag or commit) the knowledge plugin reads at. */
  private val GithubRefVar = "GRIT_GITHUB_REF"

  /** The knowledge plugin `name` as `env` declares it ([[Knowledge]]), each source synced daily
    * at 03:00 in `zone`, at most 30 summaries a run. With [[GithubTokenVar]] set, the
    * repository [[GithubOwnerVar]]/[[GithubRepoVar]] at [[GithubRefVar]], its markdown files
    * (`md`), at most 600 cards, read through the GitHub tools [[github]]'s edge serves (`grit
    * serve` with [[GithubTokenVar]] set; a run while none serves them stops at its first call,
    * and its reply says so), kept at public, the corpus `github`, and drawn on where a
    * conversation's scope holds `service:github` (`GRIT_SCOPE`). With [[DriveKeyVar]] set,
    * the shared drives grit is a member of, as [[DriveCards]] reads them through the Drive
    * tools [[drive]]'s edge serves, at most 600 cards at each label [[DriveCardLabelsVar]]
    * names, the corpus `drive`, drawn on where a scope holds `service:drive`; Drive's place is
    * internal, so no label below it is accepted. Why not: neither variable set; a part of the
    * repository unset or blank, or [[DriveCardLabelsVar]] unset, blank or naming no label,
    * naming its variable; or [[DriveCards.of]]'s or [[Knowledge.of]]'s refusal under
    * `visibility`.
    */
  private[main] def knowledge(
      name: PluginName,
      env: Map[String, String],
      visibility: Visibility,
      zone: java.time.ZoneId
  ): Either[String, Knowledge] = {
    for {
      _ <- Either.cond(
        env.contains(GithubTokenVar) || env.contains(DriveKeyVar),
        (),
        s"knowledge reads GitHub or Drive, and neither $GithubTokenVar nor $DriveKeyVar is set"
      )
      key <- ScheduleKey.of("daily")
      daily <- SyncSchedule.of(key, SlotRule.Daily(java.time.LocalTime.of(3, 0), zone), 30)
      github <-
        if (env.contains(GithubTokenVar)) githubCards(env, daily).map(Vector(_))
        else Right(Vector.empty)
      drive <-
        if (env.contains(DriveKeyVar)) driveCards(env, daily).map(Vector(_))
        else Right(Vector.empty)
      plugin <- Knowledge
        .of(name, github ++ drive, visibility)
        .left
        .map(why => s"knowledge: $why")
    } yield plugin
  }

  /** The repository [[GithubOwnerVar]]/[[GithubRepoVar]] at [[GithubRefVar]] as
    * [[knowledge]] says, synced on `daily`; or why not, as it says.
    */
  private def githubCards(
      env: Map[String, String],
      daily: SyncSchedule
  ): Either[String, CardSource] = {
    def named(variable: String): Either[String, String] =
      env
        .get(variable)
        .map(_.trim)
        .filter(_.nonEmpty)
        .toRight(s"knowledge reads the repository $variable names, and it is unset or blank")
    for {
      owner <- named(GithubOwnerVar)
      repo <- named(GithubRepoVar)
      ref <- named(GithubRefVar)
      service <- Service.of("github")
      corpus <- CorpusName.of("github")
      cards <- GitHubCards.of(
        Corpus(corpus, s"the GitHub repository $owner/$repo", Place.Everywhere, Some(service)),
        "github",
        owner,
        repo,
        ref,
        Vector(""),
        Set("md"),
        "Markdown documents",
        600,
        Some(Label.Public),
        service.place,
        Vector(daily)
      )
    } yield CardSource.GitHub(cards)
  }

  /** The shared drives grit is a member of, as [[knowledge]] says, synced on `daily` at each
    * label [[DriveCardLabelsVar]] names; or why not, as it says.
    */
  private def driveCards(
      env: Map[String, String],
      daily: SyncSchedule
  ): Either[String, CardSource] =
    for {
      written <- Some(csv(env, DriveCardLabelsVar))
        .filter(_.nonEmpty)
        .toRight(
          s"knowledge reads Drive at the labels $DriveCardLabelsVar names, and it is unset or blank"
        )
      labels <- written.foldLeft[Either[String, Vector[Label]]](Right(Vector.empty))((acc, w) =>
        acc.flatMap(done =>
          Label.read(w).map(done :+ _).left.map(why => s"$DriveCardLabelsVar: $why")
        )
      )
      service <- DriveAt
      corpus <- CorpusName.of("drive")
      cards <- DriveCards.of(
        Corpus(corpus, "the shared drives grit is a member of", Place.Everywhere, Some(service)),
        DriveTools.List.spec.name,
        DriveTools.Read.spec.name,
        labels,
        600,
        service.place,
        Vector(daily)
      )
    } yield CardSource.Drive(cards)

  private val StubPlacerVar = "GRIT_STUB_TOPICS"

  /** Jev when `JEV_API_KEY` is set (its value is [[Secrets]]'s to read); else the stub when
    * `GRIT_STUB_TOPICS` is `1`; else none, saying why.
    */
  private[main] def placer(env: Map[String, String]): Placer =
    if (env.contains(JevConfig.KeyVar)) Placer.Jev
    else if (env.get(StubPlacerVar).map(_.trim).contains("1")) Placer.Stub
    else Placer.Off(s"${JevConfig.KeyVar} is not set")

  /** The model policy: the seed catalog's ([[Seed]]), with the roles' variables laid over it
    * ([[OpenRouterConfig.policy]]) when OpenRouter's key is set.
    */
  private def policy(env: Map[String, String]): Either[String, Policy] =
    Seed.catalog.flatMap { seed =>
      if (!env.contains(OpenRouterConfig.KeyVar)) Right(seed.policy)
      else OpenRouterConfig.policy(env, seed.policy).left.map(_.message)
    }

  /** The reference deployment as `env` declares it, serving `edges` with the tools `offered`,
    * the conversations `worksIn` links working in their services and `reaches` links reaching
    * theirs, its days beginning in
    * `zone`. Serving any edge, it seeds scope `room` when `GRIT_SCOPE`
    * is unset ([[Lifecycle.ServeScope]]) and caps a day at [[Budgets.ServeDefault]] when
    * `GRIT_DAILY_USD` is. When Slack is served from the workspace `slackIn`
    * ([[SlackEdge.installedIn]]), it trusts the Slack attester for that workspace's accounts
    * ([[SlackAccounts.realm]]), and refuses unless one of `edges` is that attester; its
    * administrators are those `GRIT_ADMINS` names there ([[visibility]]). With
    * [[DriveKeyVar]] set, `service:drive` is shared ([[visibility]]), its results are believed
    * up to member, and Drive is cleared for the top label, since its files are already
    * Google's. The first
    * variable malformed, or the deployment refused, is the failure.
    */
  private[main] def deployment(
      env: Map[String, String],
      offered: Offered,
      edges: Vector[ServedEdge],
      worksIn: Vector[WorksIn],
      zone: java.time.ZoneId,
      slackIn: Option[TeamId] = None,
      reaches: Vector[Reaches] = Vector.empty
  ): Either[String, Deployment] = {
    val serving = edges.nonEmpty
    for {
      window <- tokens(env, BudgetVar, LinearAssembler.DefaultBudget)
      tail <- tokens(env, TailVar, RetrievalAssembler.DefaultTail)
      retrieving <- assemblerChoice(env)
      rounds <- toolRounds(env)
      models <- policy(env)
      sweep <- sweepEvery(env)
      lifecycle <- Lifecycle.fromEnv(
        env,
        if (serving) Lifecycle.ServeScope else LifecycleSettings.Default.locality.scope
      )
      spend <- Budgets.fromEnv(env, zone, if (serving) Budgets.ServeDefault else None)
      domains <- Claimed.fromEnv(env)
      trusted <- slackIn.fold[Either[String, Vector[Vouching]]](Right(Vector.empty))(team =>
        SlackAccounts
          .realm(team)
          .map(realm => Vector(Vouching(SlackAccounts.Attester, realm)))
          .left
          .map(why => s"the Slack team grit is installed in: $why")
      )
      identities <- Identities.of(trusted, domains).left.map(_.message)
      named <- admins(env)
      capped <- caps(env)
      drive <- DriveAt
      // Serving Drive, its place is shared, believed at member, and Drive cleared for the top.
      driving = env.contains(DriveKeyVar)
      visible <- visibility(
        named,
        slackIn,
        cleared(models) ++
          Option.when(driving)(Cleared(Party.Service(drive), Compartments.Shipped.top)),
        capped ++ Option.when(driving)(Cap(drive.place, Integrity.Member)),
        Option.when(driving)(drive.place).toVector
      )
      plugins <- pluginChoice(env, visible, zone)
      deployment <- Deployment
        .of(
          visibility = visible,
          edges = edges,
          worksIn = worksIn,
          reaches = reaches,
          plugins = plugins,
          policy = models,
          offer = Offer(offered, rounds),
          assembly = if (retrieving) Assembly.Retrieval(window, tail) else Assembly.Linear(window),
          placer = placer(env),
          lifecycle = lifecycle,
          budget = spend,
          // grit's own deployment never speaks where it was not addressed; a deployment that
          // does declares its limits (ADR 0022).
          speaking = Speaking.Off,
          sweep = sweep,
          // The reference deployment is grit itself, in a terminal and in Slack alike.
          persona = Persona.Grit,
          identities = identities
        )
        .left
        .map(_.message)
    } yield deployment
  }

  /** The variable naming the slash command `grit serve` answers, as registered for its Slack
    * app.
    */
  private val CommandVar = "GRIT_SLACK_COMMAND"

  /** The slash command `env` says `grit serve` answers ([[CommandVar]]), `/grit` when unset;
    * why not, naming the variable, when Slack would not register it.
    */
  private[main] def slackCommand(env: Map[String, String]): Either[String, SlackCommand] =
    SlackCommand.of(env.getOrElse(CommandVar, "/grit")).left.map(why => s"$CommandVar: $why")

  /** The variable naming what `grit serve`'s Slack edge notes under a reply ([[ReplyNote]]). */
  private val NoteVar = "GRIT_SLACK_REPLY_NOTE"

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

  /** The variable naming the Slack users who administer grit: their ids, comma-separated. */
  private val AdminsVar = "GRIT_ADMINS"

  /** The group [[AdminsVar]] declares, the deployment's administrators
    * ([[grit.core.visibility.Visibility.administrators]]).
    */
  private[main] val Administrators: GroupName =
    // A group's name by GroupName.of's rule, so the Left is never taken.
    GroupName.of("administrators").fold(why => throw new IllegalStateException(why), identity)

  /** The Slack users `env` names administrators ([[AdminsVar]]), none when unset; why not,
    * naming the variable, at the first that is not a Slack user's id (`U…` or `W…`).
    */
  private[main] def admins(env: Map[String, String]): Either[String, Vector[UserId]] =
    csv(env, AdminsVar)
      .foldLeft[Either[String, Vector[UserId]]](Right(Vector.empty)) { (acc, raw) =>
        acc.flatMap(ids =>
          Either.cond(
            raw.matches("[UW][A-Z0-9]+"),
            ids :+ UserId(raw),
            s"$AdminsVar: $raw is not a Slack user's id (U… or W…, as their profile's Copy member ID gives it)"
          )
        )
      }

  /** The reference deployment's visibility, clearing `parties`: with Slack served from the
    * workspace `slackIn` and `admins` named, the shipped one but for [[Administrators]],
    * declared as those users' accounts there (`slack:{team}/{id}`), who alone may lower a
    * room's label or label a public channel; otherwise [[Visibility.Shipped]], under which no
    * one may, with `parties` cleared. Either way what is said at its places is capped as `caps`
    * declare ([[caps]]), and each of `shared`, a service's place it shares only with the people
    * it names, is invited and labelled internal, so each room within it is unmapped until a
    * person labels it. A party named twice is refused.
    */
  private[main] def visibility(
      admins: Vector[UserId],
      slackIn: Option[TeamId],
      parties: Vector[Cleared],
      caps: Vector[Cap],
      shared: Vector[Place]
  ): Either[String, Visibility] = {
    val rooms = RoomLabels
      .of(shared.map(_ -> Label.at(Level.Internal)), Labelled.Mapped(Label.Public))
      .left
      .map(place => s"a shared place declared twice: ${place.written}")
    rooms.flatMap(rooms =>
      slackIn.filter(_ => admins.nonEmpty) match {
        case None =>
          Visibility
            .of(
              Compartments.Shipped,
              rooms,
              Vector.empty,
              Vector.empty,
              parties,
              caps = caps,
              invited = shared
            )
            .left
            .map(r => s"the deployment's clearances: $r")
        case Some(team) =>
          admins
            .foldLeft[Either[String, Set[Account]]](Right(Set.empty))((acc, user) =>
              acc.flatMap(as => SlackAccounts.account(team, user).map(as + _))
            )
            .left
            .map(why => s"$AdminsVar: $why")
            .flatMap(accounts =>
              Visibility
                .of(
                  Compartments.Shipped,
                  rooms,
                  Vector(Group(Administrators, accounts)),
                  Vector.empty,
                  parties,
                  administrators = Some(Administrators),
                  caps = caps,
                  invited = shared
                )
                .left
                .map(r => s"$AdminsVar: $r")
            )
      }
    )
  }

  /** The reference deployment's caps (ADR 0036) as `env` declares them: Slack's posting
    * service's results ([[SlackEdge.PostsAt]]) at `Member`, since each is its own edge's account
    * of a post it made; each repository [[GithubMemberReposVar]] names at `Member`; the rest of
    * GitHub at `Untrusted`, since its token may reach repositories anyone may write in. Any other
    * service's are `Untrusted`, undeclared, and no room is capped. Why not, naming the variable,
    * when it names a repository that is not `{owner}/{name}`.
    */
  private[main] def caps(env: Map[String, String]): Either[String, Vector[Cap]] =
    for {
      github <- Service.of("github")
      members <- csv(env, GithubMemberReposVar)
        .foldLeft[Either[String, Vector[Place]]](Right(Vector.empty))((kept, name) =>
          kept.flatMap(ps => GitHubItems.repository(github, name).map(ps :+ _))
        )
        .left
        .map(why => s"$GithubMemberReposVar: $why")
    } yield Vector(
      Cap(SlackEdge.PostsAt.place, Integrity.Member),
      Cap(github.place, Integrity.Untrusted)
    ) ++ members.distinct.map(Cap(_, Integrity.Member))

  /** The reference deployment's clearances (ADR 0031), each at [[Compartments.Shipped]]'s top:
    * every pair `policy` pins that names its upstream ([[Party.of]]; grit's own deployment
    * names one only through an operator's provider variable, and that choice is the trust),
    * the stub's pair, and the services of Jev and the stub classifier. Each party once, in
    * that order. A pair whose upstream OpenRouter picks is cleared for public alone.
    */
  private[main] def cleared(policy: Policy): Vector[Cleared] = {
    val pinned = policy.ladders
      .flatMap(_.rungs)
      .flatMap(r => Party.of(r.assignment.ref))
    val always = Party.of(StubModels.Ref).toVector ++ Vector(
      Party.Service(JevClassifier.Service),
      Party.Service(StubClassifier.Service)
    )
    (pinned ++ always).distinct.map(Cleared(_, Compartments.Shipped.top))
  }

  /** What `grit serve` says at start, served by OpenRouter under `policy`, of each role whose
    * first pin's upstream OpenRouter picks, so whose calls a labelled room cannot make: the
    * variable that would name its one upstream, and the clearance that pair then gets
    * ([[cleared]]); the dream summary's adds that it follows no other role's variables.
    */
  private[main] def uncleared(policy: Policy): Vector[String] = {
    def routed(l: Ladder) = Party.of(l.first.assignment.ref).isEmpty
    def name(role: ModelRole) = role match {
      case ModelRole.Turn => "turn"
      case ModelRole.Summary => "summary"
      case ModelRole.Query => "query"
      case ModelRole.Heard => "heard closing"
      case ModelRole.Dream => "dream summary"
    }
    ModelRole.values.toVector.collect {
      case role if routed(role.in(policy)) =>
        s"${role.upstreamVar(role.in(policy).first.grade)} is unset: the ${name(role)}'s first " +
          "model, whose upstream OpenRouter picks, is cleared for public rooms alone; naming its " +
          "one upstream there clears that pair for every label" +
          (if (role == ModelRole.Dream) s" (the ${name(role)} follows no other role's variables)"
           else "")
    }
  }

  /** What `grit serve` says at start when `admins` names no one; nothing when it names any. */
  private[main] def unadministered(admins: Vector[UserId]): Option[String] =
    Option.when(admins.isEmpty)(
      s"$AdminsVar is unset: no one can lower a room's label, label a public channel or set a " +
        "room's baseline"
    )

  /** The variable whose token `grit serve` sends GitHub's MCP server; unset, it serves no
    * GitHub edge.
    */
  private val GithubTokenVar = "GITHUB_MCP_TOKEN"

  /** The variable naming GitHub's MCP endpoint, [[GithubUrl]] when unset. */
  private val GithubUrlVar = "GRIT_GITHUB_MCP_URL"

  /** GitHub's hosted MCP server, listing only the tools that read. */
  private[main] val GithubUrl = "https://api.githubcopilot.com/mcp/readonly"

  /** The variable naming, comma-separated as `{owner}/{name}`, the GitHub repositories whose
    * items are believed up to member: each repository named here is one where every account
    * that can write is a realm member, and whose default branch takes only their merges. Unset,
    * it names none.
    */
  private val GithubMemberReposVar = "GRIT_GITHUB_MEMBER_REPOS"

  /** The variable naming the GitHub tools offered, comma-separated; [[GithubTools]] when unset. */
  private val GithubToolsVar = "GRIT_GITHUB_TOOLS"

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
  private val SlackRoot: Place = Place.under(Namespace.Slack, Vector.empty)

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
  private val DriveKeyVar = "GRIT_DRIVE_SA_KEY"

  /** The variable listing the labels the knowledge plugin cards Drive's files at: their
    * written forms ([[Label.written]], e.g. `internal` or `confidential+finance`),
    * comma-separated.
    */
  private val DriveCardLabelsVar = "GRIT_DRIVE_CARD_LABELS"

  /** The variable naming the domain whose addresses are Drive's people: one
    * `GRIT_CLAIMED_DOMAINS` lists.
    */
  private val DriveDomainVar = "GRIT_DRIVE_DOMAIN"

  /** The variable listing, comma-separated, the exact addresses of accounts that are no person
    * (another service's) whose changes to a file are believed at member; none when unset.
    */
  private val DriveWritersVar = "GRIT_DRIVE_WRITERS"

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

  /** The variable saying how many days back `grit backfill` reads, and `grit serve` hears
    * before each join of its bot ([[backfill]]).
    */
  private val DaysVar = "GRIT_BACKFILL_DAYS"

  /** The variable bounding how many messages `grit serve` hears before each join. */
  private val MessagesVar = "GRIT_BACKFILL_MESSAGES"

  /** The variable bounding how many joins a day `grit serve` hears what came before. */
  private val JoinsVar = "GRIT_BACKFILL_JOINS_PER_DAY"

  /** The days read when [[DaysVar]] is unset. */
  private[main] val DefaultDays: Int = Backfill.Default.days

  /** The days `grit backfill` reads ([[DaysVar]]); why not, naming the variable, when it is
    * not a whole number above zero.
    */
  private[main] def backfillDays(env: Map[String, String]): Either[String, Int] =
    above(env, DaysVar, DefaultDays, "a whole number of days above zero")

  /** How `grit serve` bounds what it hears before each join of its bot: [[DaysVar]],
    * [[MessagesVar]] and [[JoinsVar]], [[Backfill.Default]]'s where one is unset; why not,
    * naming the first variable that is not a whole number above zero.
    */
  private[main] def backfill(env: Map[String, String]): Either[String, Backfill] =
    for {
      days <- backfillDays(env)
      messages <- above(env, MessagesVar, Backfill.Default.messages, "a whole number above zero")
      joins <- above(env, JoinsVar, Backfill.Default.joinsPerDay, "a whole number above zero")
      bounds <- Backfill.of(days, messages, joins)
    } yield bounds

  /** `variable`'s whole number above zero in `env`, `default` when unset; why not, saying it
    * is `what`, when it is not one.
    */
  private def above(
      env: Map[String, String],
      variable: String,
      default: Int,
      what: String
  ): Either[String, Int] =
    env.get(variable) match {
      case None => Right(default)
      case Some(raw) =>
        raw.trim.toIntOption.filter(_ > 0).toRight(s"$variable is $what, not '$raw'")
    }

  private val ThemeVar = "GRIT_THEME"

  /** The theme `GRIT_THEME` names; unset, the one `kept` names, if grit still has it;
    * otherwise [[Theme.Default]].
    */
  private[main] def theme(env: Map[String, String], kept: Prefs): Either[String, Theme] =
    env.get(ThemeVar) match {
      case None => Right(kept.theme.flatMap(Theme.named).getOrElse(Theme.Default))
      case Some(name) =>
        Theme.named(name).toRight(s"$ThemeVar is none of ${Theme.all.map(_.key).mkString(", ")}")
    }

  /** `host`, and the screen's [[ChatScreen.Msg.KeepTheme]] written to the preferences at
    * `file` (nowhere without one), off the screen's thread; a write that fails says so in
    * the status line.
    */
  private def keeping(
      host: Host[ChatScreen.Msg]^,
      file: Option[java.nio.file.Path]
  ): Host[ChatScreen.Msg]^{host} =
    new Host[ChatScreen.Msg] {
      def receive(msg: ChatScreen.Msg, mailbox: Mailbox[ChatScreen.Msg]): Unit = msg match {
        case ChatScreen.Msg.KeepTheme(key) =>
          file.foreach { f =>
            val _ = Thread.ofVirtual().name("grit-prefs").start { () =>
              Prefs.save(f, Prefs.load(f).copy(theme = Some(key))).left.foreach { why =>
                mailbox.offer(ChatScreen.Msg.Noted(s"theme $key, not kept: $why"))
              }
            }
          }
        case other => host.receive(other, mailbox)
      }
    }

  private val ToolRoundsVar = "GRIT_TOOL_ROUNDS"

  /** The most model calls a turn makes, the last with tools off. */
  private[main] val DefaultToolRounds = 20

  /** The turn's budget of model calls from `GRIT_TOOL_ROUNDS`, a whole number of at least 2;
    * [[DefaultToolRounds]] when unset.
    */
  private[main] def toolRounds(env: Map[String, String]): Either[String, TurnLoop.Rounds] =
    env.get(ToolRoundsVar) match {
      case None => TurnLoop.Rounds.of(DefaultToolRounds)
      case Some(raw) =>
        raw.trim.toIntOption
          .toRight(s"$ToolRoundsVar is not a whole number")
          .flatMap(TurnLoop.Rounds.of(_).left.map(why => s"$ToolRoundsVar: $why"))
    }

  private val ToolsVar = "GRIT_TOOLS"

  /** The tools `GRIT_TOOLS` names, `read` or `all`, for the chat when `chat`, else for a run
    * with arguments. Unset is [[Offered.All]] in the chat, where a person approves each
    * call that changes something, and [[Offered.Read]] in a run, where nobody can; `all`
    * in a run is refused.
    */
  private[main] def toolChoice(
      env: Map[String, String],
      chat: Boolean
  ): Either[String, Offered] =
    env.get(ToolsVar).map(_.trim) match {
      case None => Right(if (chat) Offered.All else Offered.Read)
      case Some("read") => Right(Offered.Read)
      case Some("all") if chat => Right(Offered.All)
      case Some("all") =>
        Left(s"$ToolsVar=all needs the chat, which answers what a tool asks first")
      case Some(_) => Left(s"$ToolsVar is neither read nor all")
    }

  /** Whether `GRIT_ASSEMBLER` asks for retrieval; unset is retrieval. */
  private def assemblerChoice(env: Map[String, String]): Either[String, Boolean] =
    env.get(AssemblerVar).map(_.trim) match {
      case None | Some("retrieval") => Right(true)
      case Some("linear") => Right(false)
      case Some(_) => Left(s"$AssemblerVar is neither linear nor retrieval")
    }

  /** `dir` as the header shows it: under the home directory as `~/…`. */
  private def written(dir: java.nio.file.Path): String = {
    val home = sys.props.get("user.home").map(java.nio.file.Path.of(_))
    home
      .filter(h => dir.startsWith(h) && dir != h)
      .fold(dir.toString)(h => s"~/${h.relativize(dir)}")
  }

  private def describe(turn: TurnRef): String =
    grit.core.id.WorkflowId.value(turn.workflowId)

  private def exitOnLeft[A](result: Either[String, A]): A = result match {
    case Right(a) => a
    case Left(message) =>
      System.err.println(s"[main] $message")
      sys.exit(2)
  }
}
