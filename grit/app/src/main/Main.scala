package grit.app.main

import grit.app.chat.{ChatHost, ChatScreen, Replies}
import grit.app.config.DotEnv
import grit.app.look.Theme
import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.assembly.retrieval.RetrievalAssembler
import grit.core.context.ContextAssembler
import grit.core.id.{SourceId, TurnRef}
import grit.core.message.{Message, Tokens}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.store.Origin
import grit.dbos.engine.Engine
import grit.dbos.sql.DbConfig
import grit.models.{ModelRole, OpenRouterConfig, OpenRouterProvider, StubProvider}
import grit.tui.runtime.loop.Runtime
import grit.turn.Turn

/** grit, against the Postgres named by `GRIT_DATABASE_*` (see [[DbConfig]]). The model is
  * OpenRouter's when `OPENROUTER_API_KEY` is set (per [[ModelRole]], see
  * [[OpenRouterConfig]]), the stub otherwise, answering turns after `GRIT_STUB_DELAY_MS`
  * (default 0). Each turn's window fits in `GRIT_WINDOW_TOKENS`
  * estimated tokens (default [[LinearAssembler.DefaultBudget]]) and is chosen by
  * `GRIT_ASSEMBLER`: `retrieval` (the default), the recent turns that fit in
  * `GRIT_TAIL_TOKENS` (default [[RetrievalAssembler.DefaultTail]]) plus the earlier turns a
  * written query finds ([[RetrievalAssembler]]); or `linear`, the recent turns that fit. Every variable
  * may come from a `.env` file instead ([[DotEnv]]).
  *
  *   - **No arguments: the chat TUI**, over the conversation `GRIT_SESSION` names
  *     (default `default`). Logs go to `GRIT_LOG` (default `grit-tui.log` in the temp
  *     directory), never to the screen.
  *   - **Arguments: each is a message**, answered by one turn and printed. Repeat a
  *     message to watch a redelivery come back as the same turn; run again to watch
  *     finished turns replay without calling the model.
  */
object Main {

  /** The argument runs share one conversation, apart from any TUI session. */
  private val RunOrigin: Origin = Origin.Task("m0", "main")

  private val SystemPrompt = "You are grit."

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
    val startTheme = exitOnLeft(theme(env))
    // OpenRouter when a key is set, otherwise the stub: no key, no spend.
    val openRouter: Option[OpenRouterConfig] =
      if (!env.contains(OpenRouterConfig.KeyVar)) None
      else Some(exitOnLeft(OpenRouterConfig.fromEnv(env, ModelRole.Turn).left.map(_.message)))
    val modelName = openRouter.fold(StubProvider.Model)(_.model)
    val summaryConfig: Option[OpenRouterConfig] =
      openRouter.map(_ =>
        exitOnLeft(OpenRouterConfig.fromEnv(env, ModelRole.Summary).left.map(_.message))
      )
    val queryConfig: Option[OpenRouterConfig] =
      openRouter.map(_ =>
        exitOnLeft(OpenRouterConfig.fromEnv(env, ModelRole.Query).left.map(_.message))
      )
    // The stub answers the turn after GRIT_STUB_DELAY_MS, a slow model to watch for free.
    val stubDelay = exitOnLeft(millis(env, StubDelayVar))
    val provider = announced(tui, "turn", openRouter, stubDelay)
    val summarizer = announced(tui, "summary", summaryConfig)
    val writer = announced(tui, "query", queryConfig)

    /** `engine` with the turn launched on it: the assembler reads its stores. */
    def launched(engine: Engine^): Engine^{engine} = {
      val assembler: ContextAssembler^ =
        if (retrieving)
          new RetrievalAssembler(engine.entries, engine.search, writer, CharEstimate, budget, tail)
        else new LinearAssembler(engine.entries, CharEstimate, budget)
      engine.launch(
        Turn.body(
          SystemPrompt,
          engine.entries,
          engine.ledger,
          assembler,
          CharEstimate,
          provider,
          summarizer,
          engine.db
        )
      )
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
        val host = new ChatHost(Origin.Tui(session), opener, SystemPrompt, CharEstimate)
        // Closing the host stops following and closes the engine, however far it got.
        try Runtime.run(new ChatScreen.App(modelName, startTheme, budget), host)
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

  private val ThemeVar = "GRIT_THEME"

  /** The theme `GRIT_THEME` names; unset is [[Theme.Default]]. */
  private def theme(env: Map[String, String]): Either[String, Theme] =
    env.get(ThemeVar) match {
      case None => Right(Theme.Default)
      case Some(name) =>
        Theme.named(name).toRight(s"$ThemeVar is none of ${Theme.all.map(_.key).mkString(", ")}")
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
