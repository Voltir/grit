package grit.app

import grit.assembly.{CharEstimate, LinearAssembler}
import grit.core.id.{SourceId, TurnRef}
import grit.core.message.{Message, Tokens}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.store.Origin
import grit.dbos.engine.Engine
import grit.dbos.sql.DbConfig
import grit.models.{OpenRouterConfig, OpenRouterProvider, StubProvider}
import grit.tui.runtime.loop.Runtime
import grit.turn.Turn

/** grit, against the Postgres named by `GRIT_DATABASE_*` (see [[DbConfig]]). The model is
  * OpenRouter's when `OPENROUTER_API_KEY` is set ([[OpenRouterConfig]]), the stub
  * otherwise. Each turn's window holds the recent turns that fit in `GRIT_WINDOW_TOKENS`
  * estimated tokens (default [[LinearAssembler.DefaultBudget]]). Every variable may come
  * from a `.env` file instead ([[DotEnv]]).
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
    val budget = exitOnLeft(windowBudget(env))
    // OpenRouter when a key is set, otherwise the stub: no key, no spend.
    val openRouter: Option[OpenRouterConfig] =
      if (!env.contains(OpenRouterConfig.KeyVar)) None
      else Some(exitOnLeft(OpenRouterConfig.fromEnv(env).left.map(_.message)))
    val modelName = openRouter.fold(StubProvider.Model)(_.model)
    val model: Provider = openRouter match {
      case Some(c) => new OpenRouterProvider(c)
      case None => new StubProvider()
    }
    // Prints each call in the argument run, so a replayed turn is visibly one that did
    // not call.
    val provider: Provider =
      if (tui) model
      else
        new Provider {
          def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
            println(s"[provider] $modelName called with ${request.messages.size} message(s)")
            model.complete(request)
          }
        }

    val engine = Engine.open(config, Turn.Epoch)
    val failure: Option[String] =
      try {
        engine.launch(
          Turn.body(
            SystemPrompt,
            engine.entries,
            engine.ledger,
            new LinearAssembler(engine.entries, CharEstimate, budget),
            CharEstimate,
            provider,
            engine.db
          )
        )
        if (tui) {
          val session = env.getOrElse("GRIT_SESSION", "default")
          val host = new ChatHost(engine, Origin.Tui(session))
          // Following stops before the engine it reads from closes.
          try Runtime.run(new ChatScreen.App(modelName), host)
          finally host.close()
          None
        } else say(engine, args.toList)
      } finally {
        // DBOS's threads are non-daemon: a throw that skips this leaves the JVM, and mill,
        // waiting forever.
        engine.close()
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

  private val BudgetVar = "GRIT_WINDOW_TOKENS"

  /** The window's budget from `GRIT_WINDOW_TOKENS`, or the default when it is unset. */
  private def windowBudget(env: Map[String, String]): Either[String, Tokens] =
    env.get(BudgetVar) match {
      case None => Right(LinearAssembler.DefaultBudget)
      case Some(raw) =>
        raw.trim.toLongOption
          .filter(_ >= 0)
          .map(Tokens(_))
          .toRight(s"$BudgetVar is not a non-negative whole number")
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
