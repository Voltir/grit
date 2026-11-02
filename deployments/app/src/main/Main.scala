package grit.app.main

import scala.concurrent.duration.DurationLong

import grit.app.config.PrefsEnv
import grit.assembly.estimate.CharEstimate
import grit.chat.host.{ChatHost, Replies}
import grit.chat.run.Chat
import grit.chat.settings.Prefs
import grit.core.admin.PlaceNames
import grit.core.clock.{Clock, Fresh}
import grit.core.edge.{Console, ServedEdge, Variable}
import grit.core.id.{SourceId, TurnRef}
import grit.core.identity.Account
import grit.core.message.Message
import grit.core.model.ModelId
import grit.core.place.Directory
import grit.core.store.Origin
import grit.dbos.engine.{Engine, Link, NotTaken, Unopened}
import grit.host.checkout.{LocalInstructions, Redaction}
import grit.host.machine.LocalMachine
import grit.kit.deployment.{Assembly, Deployment}
import grit.kit.environment.{DotEnv, Exit, Secrets}
import grit.kit.run.{Kit, Launch, LocalEdge, LocalTools}
import grit.mcp.client.KeyFiles
import grit.models.{OpenRouterConfig, StubProvider}
import grit.slack.edge.{Backfill, SlackEdge}

import RunChoices.*
import Clearances.*
import ServedEdges.*
import Backfills.*

// doc-length: the program's manual, naming every variable and mode it reads
/** grit, against the Postgres named by `GRIT_DATABASE_*` (see [[DbConfig]]). The model is
  * called when `OPENROUTER_API_KEY` or `ANTHROPIC_API_KEY` is set, through OpenRouter, or
  * through Anthropic's own API for a pair at `api.anthropic.com` ([[grit.models.Keys]]; a pair whose
  * gateway's key is unset refuses the start): each role's model, budget and upstream as
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
  * `LocalShell` passes. Each turn's model is offered `file_read`, more of a file an edge kept
  * in the file area (ADR 0045), whose bytes are kept in `GRIT_FILES_DIR` (default a directory
  * of the machine's temporary directory named for the database), each for `GRIT_FILES_EXPIRY`
  * (default 24 hours, at least a minute), read as [[Defaults]] reads PDFs, Word and Excel
  * files. The engine sweeps every `GRIT_SWEEP` (default
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
  * and closes, what its balance holds, and how a window weights its own conversation against
  * what it draws on elsewhere, are data in the database: on every start the engine writes them
  * from `GRIT_IDLE`, `GRIT_SETTLE`, `GRIT_RESOLVE_AT`, `GRIT_ASKS`, `GRIT_RETENTION`,
  * `GRIT_LEDGER`, `GRIT_BALANCE` and `GRIT_WEIGHT` ([[LifecycleEnv.of]]), and logs them; `/set`
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
  *   - **`backfill topics --days N [--per-run n] [--yes]`: `grit backfill topics`**, the
  *     topics plugin's job run for each of the N days before today (1 to
  *     [[TopicDocuments.BackfillMost]]), oldest first, at
  *     each of its labels, one run at a time, on serve's database ([[Kit.runOnce]],
  *     [[TopicDocuments.backfill]]), at most `n` topics a run ([[TopicDocuments.PerRun]] when
  *     absent): how a topic data version's reset completes. It prints the database and asks
  *     before running, with the most it may cost ([[TopicDocuments.backfillQuestion]]; `--yes` does not ask),
  *     then each run's line and the spend (`nothing ran` when declined). A day run again
  *     writes only what changed. Refused while serve runs, and on a database serve never ran
  *     this deployment on. A nightly run that comes due meanwhile runs too; a topic both write
  *     is left to the next run.
  *   - **`cache-report [conversation-id]`: `grit cache-report`**, how the conversation's turns
  *     used the provider's prompt cache ([[Kit.cacheReport]]), printed: each turn's carry or
  *     why it drew afresh, its share of input read from the cache and the gap since the turn
  *     before, and each call's tokens and cost. With no id, the conversation the chat holds in
  *     the working directory under `GRIT_SESSION` ([[Kit.find]]); refused when no chat was held
  *     there. It only reads, beside a running grit or none, never starts the engine, and never
  *     creates the conversation.
  *   - **Other arguments: each is a message**, answered by one turn and printed. Repeat a
  *     message to watch a redelivery come back as the same turn; run again to watch
  *     finished turns replay without calling the model. Nothing here answers a gated call,
  *     so `GRIT_TOOLS=all` is refused.
  */
object Main {

  /** The argument runs share one conversation, apart from any TUI session. */
  private[main] val RunOrigin: Origin = Origin.Task("m0", "main")

  def main(args: Array[String]): Unit = {
    // A refusal before anything runs ends with status 2; a failure once it runs, with 1.
    val exitOnLeft = Exit.process("[main]", 2)
    val failed = Exit.process("[main]", 1)
    val tui = args.isEmpty
    // `grit serve` alone: the engine and the Slack edge (a lone message "serve" is this).
    val serving = args.sameElements(Array("serve"))
    // `grit backfill`, and `--yes` to hear without asking.
    val backfilling =
      args.sameElements(Array("backfill")) || args.sameElements(Array("backfill", "--yes"))
    val slack = serving || backfilling
    // `grit backfill topics --days N [--per-run n] [--yes]`: the topics job for past days.
    val topicsBackfill: Option[BackfillTopics] =
      if (args.take(2).sameElements(Array("backfill", "topics")))
        Some(exitOnLeft(backfillTopics(args.drop(2).toVector)))
      else None
    val reported = cacheReported(args.toVector)
    val env = exitOnLeft(DotEnv.loadDefault(sys.env))
    val log = env.getOrElse(
      "GRIT_LOG",
      Launch.temp.resolve("grit-tui.log").toString
    )
    // Before anything loads slf4j (DBOS does): a log line on stderr would paint over the
    // screen, or interleave with the cache report's lines.
    if (tui || reported.nonEmpty) {
      val _ = System.setProperty("org.slf4j.simpleLogger.logFile", log)
    }
    // The Slack SDK logs request bodies at debug: never below info.
    if (slack) { val _ = System.setProperty("org.slf4j.simpleLogger.log.com.slack.api", "info") }
    // The deployment's one redaction, over every secret `.env` and the environment hold.
    val redaction = Redaction.of(env)
    org.slf4j.LoggerFactory.getLogger("grit.main").info(redaction.described)

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
             exitOnLeft(Backfill.read(env)),
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
        Deploying.deployment(
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
    val prefsFile = PrefsEnv.path(env)
    val startTheme = exitOnLeft(theme(env, prefsFile.fold(Prefs.empty)(Prefs.load)))
    // The stub answers the turn after GRIT_STUB_DELAY_MS, a slow model to watch for free.
    val stubDelay = exitOnLeft(millis(env, StubDelayVar))
    val modelName = secrets.keys.fold(StubProvider.Model)(_ =>
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
        if (env.contains(Variable.value(OpenRouterConfig.KeyVar)))
          uncleared(deployment.policy).foreach(line => System.err.println(s"[main] $line"))
        Kit.serve(deployment, env).left.map(_.message).swap.toOption
      } else if (topicsBackfill.nonEmpty)
        topicsBackfill.flatMap(b =>
          topicsBackfilled(
            deployment,
            env,
            b,
            java.time.LocalDate.ofInstant(clock.now(), java.time.ZoneId.systemDefault()),
            clock.now()
          ).swap.toOption
        )
      else if (reported.nonEmpty)
        reported.flatMap { named =>
          // With no id, the conversation the chat holds here, never created by asking.
          named
            .fold(Kit.find(deployment, secrets, Origin.Tui(directory, session), clock))(id =>
              Right(Some(id))
            )
            .left
            .map(_.message)
            .flatMap(
              _.toRight(
                s"no chat has been held in ${Directory.value(directory)} yet (session $session)"
              )
            )
            .flatMap(id =>
              Kit
                .cacheReport(deployment, secrets, id, clock)
                .map(_.foreach(println))
                .left
                .map(_.message)
            )
            .swap
            .toOption
        }
      else if (backfilling)
        Backfill
          .days(env)
          .flatMap { days =>
            Kit
              .catchUp(
                deployment,
                SlackEdge.backfill(days),
                env,
                agreed(args.contains("--yes")),
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
        val here: Console.Edge^ = exitOnLeft(
          LocalEdge
            .over(
              directory,
              new LocalTools(offered, redaction),
              new LocalInstructions(),
              org.slf4j.LoggerFactory.getLogger("grit.edge")
            )
            .left
            .map(_.refusing("the coding tools"))
        )
        val opener = Kit.console(deployment, secrets, stubDelay.millis, clock, here) match {
          case Right(found) => found
          case Left(refused) => failed.refuse(refused.message)
        }
        Chat.run(
          opener,
          origin,
          model = modelName,
          session = s"$session · ${written(root)}",
          startTheme,
          budget,
          CharEstimate,
          prefs = prefsFile,
          log = Some(java.nio.file.Path.of(log)),
          clock,
          fresh
        )
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
    failure.foreach(failed.refuse)
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

  /** Each message answered by one turn, printed. */
  private[main] def say(engine: Link^, messages: List[String]): Option[String] = {
    val started = messages.map { text =>
      for {
        // The text is its own source id, so repeating a message redelivers it.
        turn <- engine.inbox.ingest(
          RunOrigin,
          SourceId(text),
          Message.User(text),
          Account.Local,
          None
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

  /** `dir` as the header shows it: under the home directory as `~/…`. */
  private[main] def written(dir: java.nio.file.Path): String = {
    val home = sys.props.get("user.home").map(java.nio.file.Path.of(_))
    home
      .filter(h => dir.startsWith(h) && dir != h)
      .fold(dir.toString)(h => s"~/${h.relativize(dir)}")
  }

  private[main] def describe(turn: TurnRef): String =
    grit.core.id.WorkflowId.value(turn.workflowId)
}
