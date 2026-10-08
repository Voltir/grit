package grit.app.main

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.app.chat.{ChatHost, ChatScreen, Replies}
import grit.app.config.{Budgets, Claimed, Durations, Lifecycle, Prefs}
import grit.app.look.Theme
import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.assembly.retrieval.RetrievalAssembler
import grit.core.clock.{Clock, Fresh}
import grit.core.edge.ServedEdge
import grit.core.id.{PluginName, PrincipalId, SourceId, TurnRef}
import grit.core.identity.{Account, Identities, Vouching}
import grit.core.message.{Message, Tokens}
import grit.core.model.{ModelId, Policy}
import grit.core.period.LifecycleSettings
import grit.core.persona.Persona
import grit.core.place.{Directory, Namespace, Place, Service, WorksIn}
import grit.core.plugin.Plugin
import grit.core.prompt.Fragment
import grit.core.speech.Speaking
import grit.core.store.Origin
import grit.core.tool.ToolName
import grit.core.tool.ToolSet
import grit.dbos.engine.{Engine, EngineLock, Link, NotTaken, Unopened}
import grit.digest.Digest
import grit.edge.{PlaceFragments, Server}
import grit.host.{LocalEdits, LocalInstructions, LocalMachine, LocalShell, LocalWorkspace}
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Topics}
import grit.kit.environment.{DotEnv, Secrets}
import grit.kit.run.{Kit, Launch}
import grit.mcp.client.McpServer
import grit.mcp.edge.McpEdge
import grit.mcp.scope.McpScope
import grit.models.{JevConfig, OpenRouterConfig, Seed, StubProvider}
import grit.remind.Reminders
import grit.slack.edge.{SlackAccounts, SlackCommand, SlackEdge}
import grit.slack.event.{ChannelId, TeamId}
import grit.tools.Coding
import grit.tui.runtime.app.{Host, Mailbox}
import grit.tui.runtime.loop.Runtime
import grit.turn.{Turn, TurnLoop}

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
  * may come from a `.env` file instead ([[DotEnv]]). Each message is placed among the
  * conversation's topics by Jev when `JEV_API_KEY` is set ([[JevConfig]]); without it, by
  * the stub classifier when `GRIT_STUB_TOPICS=1` (for the gate), and otherwise by none,
  * which leaves each message in the topic it is in. The TUI starts in the theme
  * `GRIT_THEME` names, or else the one last chosen with `/theme` ([[Prefs]]). Each turn's
  * model is offered the tools `GRIT_TOOLS` names ([[Offered]]) in at most
  * `GRIT_TOOL_ROUNDS` model calls (default [[DefaultToolRounds]], at least 2), the last with
  * tools off. The coding tools are hosted (ADR 0017): offered only when an edge serves the
  * conversation's directory, and run by that edge. The TUI is the edge for the directory it
  * runs in, with its instruction files; a command it runs sees only the environment
  * `LocalShell` passes. The engine sweeps every `GRIT_SWEEP` (default
  * [[DefaultSweep]]), asking the same classifier as the topics whether anyone is waiting on
  * each quiet period ([[Settle]]), closing each period whose deadline has come ([[Close]]), its
  * closing written by the summary role and gated by that classifier, and posting each closed
  * period to the plugins `GRIT_PLUGINS` turns on ([[pluginChoice]], [[Posting]]; with
  * Digest on, each turn's model is offered `recent_activity`; with the reminders on,
  * `remind_me`, `reminders` and `cancel_reminder`). When a period is asked about
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
  *     process ([[Kit.serve]], [[SlackEdge.serving]]; ADR 0019), over Socket Mode with `SLACK_BOT_TOKEN` and
  *     `SLACK_APP_TOKEN`, until stopped. In the channels, public or private, `GRIT_SLACK_LISTEN` names (ids,
  *     comma-separated; none by default) it also hears what is not said to it, and a private
  *     channel is served at all only when named there. It answers the slash command
  *     `GRIT_SLACK_COMMAND` names (default `/grit`, as registered for the Slack app) to its
  *     asker alone ([[SlackEdge.command]]). Its tools are `read`'s, as in a run with arguments:
  *     nothing in Slack answers a gated call yet, so `GRIT_TOOLS=all` is refused. It never
  *     attaches: another grit holding the database's engine stops it. Give it a database of
  *     its own (`GRIT_DATABASE_URL`): everyone in the workspace sees what that database holds.
  *     With `GITHUB_MCP_TOKEN` set it also serves GitHub's read-only tools at
  *     `service:github`, and every Slack conversation works there ([[github]]): from the MCP
  *     server at `GRIT_GITHUB_MCP_URL` (default [[GithubUrl]]), the tools `GRIT_GITHUB_TOOLS`
  *     names (comma-separated; default [[GithubTools]]). It refuses to start when that server
  *     cannot be reached, refuses the token, or offers none of those tools.
  *   - **`backfill` alone, or with `--yes`: `grit backfill`**, run before `grit serve` on its
  *     database ([[Kit.catchUp]], [[SlackEdge.backfill]]; ADR 0020): what each channel in `GRIT_SLACK_LISTEN` said over
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
    // `grit serve` and `grit backfill` serve Slack, in the channels GRIT_SLACK_LISTEN names.
    val listen = if (slack) exitOnLeft(listening(env)) else Set.empty[ChannelId]
    // `grit serve` also serves GitHub's tools when GITHUB_MCP_TOKEN is set, Slack's
    // conversations working there.
    val githubEdge = if (serving) exitOnLeft(github(env)) else None
    val edges: Vector[ServedEdge] =
      // Slack's slash command, GRIT_SLACK_COMMAND, as registered for its app.
      (if (slack) Vector(SlackEdge.serving(exitOnLeft(slackCommand(env)), listen))
       else Vector.empty) ++ githubEdge.map(_._1)
    // Days begin at this machine's midnight (OpenRouter's own daily figure is UTC's).
    // Serving Slack, the workspace its bot token is installed in is trusted to say who its
    // people are.
    val slackIn =
      if (slack) Some(exitOnLeft(SlackEdge.installedIn(env).left.map(_.message))) else None
    val deployment =
      exitOnLeft(
        Main.deployment(
          env,
          offered,
          edges,
          githubEdge.map(_._2).toVector,
          java.time.ZoneId.systemDefault(),
          slackIn
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
      ModelId.value(deployment.policy.turn.ref.model)
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
      if (serving) Kit.serve(deployment, env).left.map(_.message).swap.toOption
      else if (backfilling)
        backfillDays(env)
          .flatMap { days =>
            Kit
              .catchUp(
                deployment,
                SlackEdge.backfill(listen, days),
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
                    Turn.Epoch,
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
                    Turn.Epoch,
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
        Engine.open(
          config,
          Turn.Epoch,
          identity,
          deployment.budget,
          deployment.visibility,
          clock
        ) match {
          case Left(Unopened.Lock(NotTaken.Held(_))) =>
            // Another grit runs the engine: its turns are sent to it, as a TUI's are.
            val link =
              Link.attach(
                config,
                Turn.Epoch,
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
    * Those there are: `digest` ([[Digest]]) and `remind` ([[Reminders]]).
    */
  private[main] def pluginChoice(env: Map[String, String]): Either[String, Vector[Plugin]] =
    env
      .get(PluginsVar)
      .map(_.split(',').toVector.map(_.trim).filter(_.nonEmpty).distinct)
      .getOrElse(Vector.empty)
      .foldLeft[Either[String, Vector[Plugin]]](Right(Vector.empty)) { (acc, raw) =>
        acc.flatMap { done =>
          PluginName.of(raw).left.map(why => s"$PluginsVar: $why").flatMap { name =>
            raw match {
              case "digest" => Right(done :+ new Digest(name))
              case "remind" => Right(done :+ new Reminders(name))
              case other => Left(s"$PluginsVar: no plugin $other; there are digest and remind")
            }
          }
        }
      }

  private val StubTopicsVar = "GRIT_STUB_TOPICS"

  /** Jev when `JEV_API_KEY` is set (its value is [[Secrets]]'s to read); else the stub when
    * `GRIT_STUB_TOPICS` is `1`; else none, saying why.
    */
  private[main] def topics(env: Map[String, String]): Topics =
    if (env.contains(JevConfig.KeyVar)) Topics.Jev
    else if (env.get(StubTopicsVar).map(_.trim).contains("1")) Topics.Stub
    else Topics.Off(s"${JevConfig.KeyVar} is not set")

  /** The model policy: the seed catalog's ([[Seed]]), with the roles' variables laid over it
    * ([[OpenRouterConfig.policy]]) when OpenRouter's key is set.
    */
  private def policy(env: Map[String, String]): Either[String, Policy] =
    Seed.catalog.flatMap { seed =>
      if (!env.contains(OpenRouterConfig.KeyVar)) Right(seed.policy)
      else OpenRouterConfig.policy(env, seed.policy).left.map(_.message)
    }

  /** The reference deployment as `env` declares it, serving `edges` with the tools `offered`
    * and the conversations `worksIn` links working in their services, its days beginning in
    * `zone`. Serving any edge, it seeds scope `room` when `GRIT_SCOPE`
    * is unset ([[Lifecycle.ServeScope]]) and caps a day at [[Budgets.ServeDefault]] when
    * `GRIT_DAILY_USD` is. When Slack is served from the workspace `slackIn`
    * ([[SlackEdge.installedIn]]), it trusts the Slack attester for that workspace's accounts
    * ([[SlackAccounts.realm]]), and refuses unless one of `edges` is that attester. The first
    * variable malformed, or the deployment refused, is the failure.
    */
  private[main] def deployment(
      env: Map[String, String],
      offered: Offered,
      edges: Vector[ServedEdge],
      worksIn: Vector[WorksIn],
      zone: java.time.ZoneId,
      slackIn: Option[TeamId] = None
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
      plugins <- pluginChoice(env)
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
      deployment <- Deployment
        .of(
          edges = edges,
          worksIn = worksIn,
          plugins = plugins,
          policy = models,
          offer = Offer(offered, rounds),
          assembly = if (retrieving) Assembly.Retrieval(window, tail) else Assembly.Linear(window),
          topics = topics(env),
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

  /** The variable naming the channels grit listens in: their ids, comma-separated. Unset, it
    * listens in none.
    */
  private val ListenVar = "GRIT_SLACK_LISTEN"

  /** The channels `env` says grit listens in ([[ListenVar]]); why not, naming the first entry
    * that is not a channel id.
    */
  private[main] def listening(env: Map[String, String]): Either[String, Set[ChannelId]] =
    env
      .get(ListenVar)
      .toVector
      .flatMap(_.split(',').toVector.map(_.trim).filter(_.nonEmpty))
      .foldLeft[Either[String, Set[ChannelId]]](Right(Set.empty)) { (acc, raw) =>
        acc.flatMap(ids =>
          ChannelId
            .read(raw)
            .map(ids + _)
            .toRight(
              s"$ListenVar: $raw is not a channel id (C… or G…, as Slack's channel details show it)"
            )
        )
      }

  /** The variable whose token `grit serve` sends GitHub's MCP server; unset, it serves no
    * GitHub edge.
    */
  private val GithubTokenVar = "GITHUB_MCP_TOKEN"

  /** The variable naming GitHub's MCP endpoint, [[GithubUrl]] when unset. */
  private val GithubUrlVar = "GRIT_GITHUB_MCP_URL"

  /** GitHub's hosted MCP server, listing only the tools that read. */
  private[main] val GithubUrl = "https://api.githubcopilot.com/mcp/readonly"

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
    * token reaches; why not, naming the variable, when the URL is not one [[McpServer.of]]
    * takes or the list names no tool.
    */
  private[main] def githubServer(env: Map[String, String]): Either[String, McpServer] = {
    val allow = env
      .get(GithubToolsVar)
      .fold(GithubTools)(_.split(',').toVector.map(_.trim).filter(_.nonEmpty).toSet)
    for {
      _ <- Either.cond(allow.nonEmpty, (), s"$GithubToolsVar names no tool")
      server <- McpServer
        .of(
          "github",
          env.getOrElse(GithubUrlVar, GithubUrl),
          grit.core.edge.Variable(GithubTokenVar),
          McpScope.Open,
          allow
        )
        .left
        .map(why => s"$GithubUrlVar: $why")
    } yield server
  }

  /** The GitHub edge `grit serve` serves when `env` sets [[GithubTokenVar]] ([[McpEdge]], over
    * [[githubServer]]), and the link that makes every Slack conversation work in its service;
    * `None` when the token is unset; why not as [[githubServer]] says.
    */
  private[main] def github(
      env: Map[String, String]
  ): Either[String, Option[(ServedEdge, WorksIn)]] =
    if (!env.contains(GithubTokenVar)) Right(None)
    else
      for {
        service <- Service.of("github")
        server <- githubServer(env)
        edge <- McpEdge.serving(service, Vector(server))
      } yield Some((edge, WorksIn(Place.under(Namespace.Slack, Vector.empty), service)))

  /** The variable saying how many days back `grit backfill` reads. */
  private val DaysVar = "GRIT_BACKFILL_DAYS"

  /** The days read when [[DaysVar]] is unset. */
  private[main] val DefaultDays: Int = 2

  /** The days `grit backfill` reads ([[DaysVar]]); why not, naming the variable, when it is
    * not a whole number above zero, or when [[ListenVar]] names no channel, so there is
    * nothing to backfill.
    */
  private[main] def backfillDays(env: Map[String, String]): Either[String, Int] =
    for {
      listen <- listening(env)
      _ <- Either.cond(
        listen.nonEmpty,
        (),
        s"$ListenVar names no channel: there is nothing to backfill"
      )
      days <- env.get(DaysVar) match {
        case None => Right(DefaultDays)
        case Some(raw) =>
          raw.trim.toIntOption
            .filter(_ > 0)
            .toRight(s"$DaysVar is a whole number of days above zero, not '$raw'")
      }
    } yield days

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
  private[main] def toolRounds(env: Map[String, String]): Either[String, TurnLoop.Budget] =
    env.get(ToolRoundsVar) match {
      case None => TurnLoop.Budget.of(DefaultToolRounds)
      case Some(raw) =>
        raw.trim.toIntOption
          .toRight(s"$ToolRoundsVar is not a whole number")
          .flatMap(TurnLoop.Budget.of(_).left.map(why => s"$ToolRoundsVar: $why"))
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
