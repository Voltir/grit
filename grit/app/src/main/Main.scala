package grit.app.main

import grit.app.chat.{ChatHost, ChatScreen, Replies}
import grit.app.config.{DotEnv, Prefs}
import grit.app.look.Theme
import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.assembly.retrieval.RetrievalAssembler
import grit.core.classify.Classifier
import grit.core.clock.{Clock, Fresh}
import grit.core.context.ContextAssembler
import grit.core.id.{SourceId, TurnRef}
import grit.core.message.{Message, Tokens}
import grit.core.model.{StrictSchemas, TurnProfile}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.store.Origin
import grit.core.tool.{DuplicateName, ToolName}
import grit.dbos.engine.Engine
import grit.dbos.sql.DbConfig
import grit.host.{LocalEdits, LocalShell, LocalWorkspace}
import grit.models.{
  JevClassifier,
  JevConfig,
  OpenRouterConfig,
  OpenRouterProvider,
  Seed,
  StubClassifier,
  StubProvider
}
import grit.tools.Coding
import grit.tui.runtime.app.{Host, Mailbox}
import grit.tui.runtime.loop.Runtime
import grit.turn.{Turn, TurnEnv, TurnLoop, TurnRecords, TurnTooling}

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
  * model is offered the tools `GRIT_TOOLS` names ([[ToolChoice]]) over the checkout grit
  * runs in, in at most `GRIT_TOOL_ROUNDS` model calls (default [[DefaultToolRounds]], at
  * least 2), the last with tools off. A command it runs sees only the environment
  * `LocalShell` passes.
  *
  *   - **No arguments: the chat TUI**, over the conversation `GRIT_SESSION` names
  *     (default `default`). Logs go to `GRIT_LOG` (default `grit-tui.log` in the temp
  *     directory), never to the screen.
  *   - **Arguments: each is a message**, answered by one turn and printed. Repeat a
  *     message to watch a redelivery come back as the same turn; run again to watch
  *     finished turns replay without calling the model. Nothing here answers a gated call,
  *     so `GRIT_TOOLS=all` is refused.
  */
object Main {

  /** The argument runs share one conversation, apart from any TUI session. */
  private val RunOrigin: Origin = Origin.Task("m0", "main")

  /** The system prompt, for a checkout whose root is `root`. What each tool does rides with
    * the tool, never here: a model told of a tool it was not offered writes the call out.
    */
  private def systemPrompt(root: java.nio.file.Path): String =
    s"You are grit. You work in the repository at $root, through the tools you are offered."

  def main(args: Array[String]): Unit = {
    val tui = args.isEmpty
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

    val config = exitOnLeft(DbConfig.fromEnv(env).left.map(_.message))
    val budget = exitOnLeft(tokens(env, BudgetVar, LinearAssembler.DefaultBudget))
    val tail = exitOnLeft(tokens(env, TailVar, RetrievalAssembler.DefaultTail))
    val retrieving = exitOnLeft(assemblerChoice(env))
    val rounds = exitOnLeft(toolRounds(env))
    val offered = exitOnLeft(toolChoice(env, chat = tui))
    // The checkout the turn's tools read is the one grit runs in.
    val root = java.nio.file.Path.of("").toAbsolutePath
    val system = systemPrompt(root)
    val prefsFile = Prefs.path(env)
    val startTheme = exitOnLeft(theme(env, prefsFile.fold(Prefs.empty)(Prefs.load)))
    // OpenRouter when a key is set, otherwise the stub: no key, no spend. Each role's model
    // is the seed catalog's policy, with the environment laid over it for this run.
    val pinned: Option[(String, TurnProfile)] =
      if (!env.contains(OpenRouterConfig.KeyVar)) None
      else {
        val key = exitOnLeft(OpenRouterConfig.key(env).left.map(_.message))
        val seed = exitOnLeft(Seed.catalog)
        val policy = exitOnLeft(OpenRouterConfig.policy(env, seed.policy).left.map(_.message))
        Some((key, seed.withPolicy(policy).pin))
      }
    val openRouter = pinned.map((key, p) => OpenRouterConfig.of(key, p.turn))
    val modelName = openRouter.fold(StubProvider.Model)(_.model)
    val summaryConfig = pinned.map((key, p) => OpenRouterConfig.of(key, p.summary))
    val queryConfig = pinned.map((key, p) => OpenRouterConfig.of(key, p.query))
    // The stub answers the turn after GRIT_STUB_DELAY_MS, a slow model to watch for free.
    val stubDelay = exitOnLeft(millis(env, StubDelayVar))
    val topics = exitOnLeft(classifierChoice(env))
    val provider = announced(tui, "turn", openRouter, stubDelay)
    val summarizer = announced(tui, "summary", summaryConfig)
    val writer = announced(tui, "query", queryConfig)

    /** `engine` with the turn launched on it: the assembler reads its stores. Throws when the
      * coding tools repeat a name, a fault in `grit.tools` that no setting can cause.
      */
    def launched(engine: Engine^): Engine^{engine} = {
      val assembler: ContextAssembler^ =
        if (retrieving)
          new RetrievalAssembler(engine.entries, engine.search, writer, CharEstimate, budget, tail)
        else new LinearAssembler(engine.entries, CharEstimate, budget)
      val checkout = new LocalWorkspace(root)
      val strict = pinned.exists(_._2.turn.settings.strict == StrictSchemas.Enforced)
      def launch(tooling: TurnTooling^): Unit =
        engine.launch(
          Turn.body(
            TurnEnv(
              system,
              TurnRecords(engine.entries, engine.ledger, CharEstimate),
              assembler,
              classifier(topics),
              provider,
              summarizer,
              engine.db,
              Clock.system(),
              Fresh.random()
            ),
            tooling
          )
        )
      val launching = offered match {
        case ToolChoice.Read =>
          Coding
            .readOnly(checkout)
            .map(tools => launch(TurnTooling.ReadOnly(checkout, tools, engine.jot, rounds, strict)))
        case ToolChoice.All =>
          val edits = new LocalEdits(root)
          // The process's own environment, not .env's: a command never needs grit's settings.
          val shell = new LocalShell(root, sys.env)
          Coding
            .all(checkout, edits, shell)
            .map(tools =>
              launch(TurnTooling.Full(checkout, edits, shell, tools, engine.jot, rounds, strict))
            )
      }
      // Its caller closes the engine and reports the throw: in the chat, as the engine that
      // could not open.
      launching.left.foreach { case DuplicateName(name) =>
        throw new IllegalStateException(s"the coding tools offer ${ToolName.value(name)} twice")
      }
      engine
    }

    val failure: Option[String] =
      if (tui) {
        // The screen paints first; the engine opens behind it, on the host's thread.
        val opener = new ChatHost.Opener {
          def open(): Engine^ = {
            val engine = Engine.open(config, Turn.Epoch)
            try launched(engine)
            catch {
              case e: Throwable =>
                // DBOS's threads are non-daemon: an engine that is not handed on is closed.
                engine.close()
                throw e
            }
          }
        }
        val session = env.getOrElse("GRIT_SESSION", "default")
        val host = new ChatHost(
          Origin.Tui(session),
          opener,
          system,
          CharEstimate,
          Some(java.nio.file.Path.of(log))
        )
        // Closing the host stops following and closes the engine, however far it got.
        try
          Runtime.run(
            new ChatScreen.App(modelName, startTheme, budget, session),
            keeping(host, prefsFile)
          )
        finally host.close()
        None
      } else {
        val engine = Engine.open(config, Turn.Epoch)
        try say(launched(engine), args.toList)
        finally {
          // DBOS's threads are non-daemon: a throw that skips this leaves the JVM, and mill,
          // waiting forever.
          engine.close()
        }
      }

    if (tui) println(s"[main] log: $log")
    failure.foreach { message =>
      System.err.println(s"[main] $message")
      sys.exit(1)
    }
  }

  /** Each message answered by one turn, printed. */
  private def say(engine: Engine^, messages: List[String]): Option[String] = {
    val started = messages.map { text =>
      for {
        // The text is its own source id, so repeating a message redelivers it.
        turn <- engine.inbox.ingest(RunOrigin, SourceId(text), Message.User(text))
        _ <- engine.inbox.startTurn(turn)
      } yield turn
    }
    started.collectFirst { case Left(error) => error } match {
      case Some(error) => Some(s"inbox: $error")
      case None =>
        started.collect { case Right(turn) => turn }.distinct.foreach { turn =>
          val outcome = engine.awaitTurn(turn)
          val reply = Replies.of(engine, turn).fold(identity, _.getOrElse("(no reply)"))
          println(s"[main] ${describe(turn)} -> $outcome: $reply")
        }
        None
    }
  }

  /** OpenRouter under `config`, or the stub without one. In the argument run each call is
    * printed with its `role`, so a replayed turn is visibly one that did not call.
    */
  private def announced(
      tui: Boolean,
      role: String,
      config: Option[OpenRouterConfig],
      stubDelayMs: Long = 0
  ): Provider = {
    val model: Provider = config match {
      case Some(c) => new OpenRouterProvider(c)
      case None => new StubProvider(stubDelayMs)
    }
    val name = config.fold(StubProvider.Model)(_.model)
    if (tui) model
    else
      new Provider {
        def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
          println(s"[provider] $role: $name called with ${request.messages.size} message(s)")
          model.complete(request)
        }
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

  private val StubTopicsVar = "GRIT_STUB_TOPICS"

  /** Which classifier places each message among its conversation's topics. */
  private[main] enum ClassifierChoice {
    case Jev(config: JevConfig)
    case Stub

    /** None, for `reason`: every message after a conversation's first stays where it is. */
    case Off(reason: String)
  }

  /** Jev when `JEV_API_KEY` is set (an empty one is an error, named, never shown); else the
    * stub when `GRIT_STUB_TOPICS` is `1`; else none.
    */
  private[main] def classifierChoice(env: Map[String, String]): Either[String, ClassifierChoice] =
    if (env.contains(JevConfig.KeyVar))
      JevConfig.fromEnv(env).map(ClassifierChoice.Jev(_)).left.map(_.message)
    else if (env.get(StubTopicsVar).map(_.trim).contains("1")) Right(ClassifierChoice.Stub)
    else Right(ClassifierChoice.Off(s"${JevConfig.KeyVar} is not set"))

  private def classifier(choice: ClassifierChoice): Classifier^ = choice match {
    case ClassifierChoice.Jev(config) => new JevClassifier(config)
    case ClassifierChoice.Stub => new StubClassifier
    case ClassifierChoice.Off(reason) => Classifier.none(reason)
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
  private[main] def toolRounds(env: Map[String, String]): Either[String, TurnLoop.Budget] =
    env.get(ToolRoundsVar) match {
      case None => TurnLoop.Budget.of(DefaultToolRounds)
      case Some(raw) =>
        raw.trim.toIntOption
          .toRight(s"$ToolRoundsVar is not a whole number")
          .flatMap(TurnLoop.Budget.of(_).left.map(why => s"$ToolRoundsVar: $why"))
    }

  private val ToolsVar = "GRIT_TOOLS"

  /** Which tools a turn's model is offered. */
  private[main] enum ToolChoice {

    /** `read`, `list` and `search` (`Coding.readOnly`): nothing asks first. */
    case Read

    /** Those and `write`, `edit` and `run` (`Coding.all`), each of which asks first. */
    case All
  }

  /** The tools `GRIT_TOOLS` names, `read` or `all`, for the chat when `chat`, else for a run
    * with arguments. Unset is [[ToolChoice.All]] in the chat, where a person approves each
    * call that changes something, and [[ToolChoice.Read]] in a run, where nobody can; `all`
    * in a run is refused.
    */
  private[main] def toolChoice(
      env: Map[String, String],
      chat: Boolean
  ): Either[String, ToolChoice] =
    env.get(ToolsVar).map(_.trim) match {
      case None => Right(if (chat) ToolChoice.All else ToolChoice.Read)
      case Some("read") => Right(ToolChoice.Read)
      case Some("all") if chat => Right(ToolChoice.All)
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

  private def describe(turn: TurnRef): String =
    grit.core.id.WorkflowId.value(turn.workflowId)

  private def exitOnLeft[A](result: Either[String, A]): A = result match {
    case Right(a) => a
    case Left(message) =>
      System.err.println(s"[main] $message")
      sys.exit(2)
  }
}
