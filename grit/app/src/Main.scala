package grit.app

import grit.assembly.LinearAssembler
import grit.core.{
  AssistantBlock,
  Message,
  ModelRequest,
  Origin,
  Payload,
  Provider,
  ProviderError,
  SourceId,
  TurnRef
}
import grit.dbos.{DbConfig, Engine}
import grit.models.StubProvider
import grit.turn.Turn

/** The M0 run: each argument is a source message id, ingested and answered by the real
  * turn with the stub provider, against the Postgres named by `GRIT_DATABASE_*` (see
  * [[DbConfig]]). Repeat an id to watch a redelivery come back as the same turn; run again
  * with the same ids to watch finished turns replay without calling the provider.
  */
object Main {

  /** Every run shares one conversation. */
  private val RunOrigin: Origin = Origin.Task("m0", "main")

  private val SystemPrompt = "You are grit."

  def main(args: Array[String]): Unit = {
    val config = DbConfig.fromEnv(sys.env) match {
      case Right(c) => c
      case Left(invalid) =>
        System.err.println(s"[main] ${invalid.message}")
        sys.exit(2)
    }
    val sources =
      if (args.isEmpty) List("m0-" + System.currentTimeMillis()) else args.toList

    val engine = Engine.open(config, Turn.Epoch)
    val failure: Option[String] =
      try {
        // Prints each call, so a replayed turn is visibly one that did not call.
        val provider = new Provider {
          private val stub = new StubProvider()
          def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
            println(s"[provider] called with ${request.messages.size} message(s)")
            stub.complete(request)
          }
        }
        engine.launch(
          Turn.body(
            SystemPrompt,
            engine.entries,
            new LinearAssembler(engine.entries),
            provider,
            engine.db
          )
        )
        val started = sources.map { source =>
          for {
            turn <- engine.inbox.ingest(
              RunOrigin,
              SourceId(source),
              Message.User(s"message $source")
            )
            _ <- engine.inbox.startTurn(turn)
          } yield turn
        }
        started.collectFirst { case Left(error) => error } match {
          case Some(error) => Some(s"inbox: $error")
          case None =>
            started.collect { case Right(turn) => turn }.distinct.foreach { turn =>
              println(
                s"[main] ${describe(turn)} -> ${engine.awaitTurn(turn)}: ${reply(engine, turn)}"
              )
            }
            None
        }
      } finally {
        // DBOS's threads are non-daemon: a throw that skips this leaves the JVM, and mill,
        // waiting forever.
        engine.close()
      }

    failure.foreach { message =>
      System.err.println(s"[main] $message")
      sys.exit(1)
    }
  }

  private def describe(turn: TurnRef): String =
    grit.core.WorkflowId.value(turn.workflowId)

  /** The text of `turn`'s reply, as recorded. */
  private def reply(engine: Engine^, turn: TurnRef): String =
    engine.db.read(engine.entries.get(Turn.replyId(turn))) match {
      case Right(Some(entry)) =>
        entry.payload match {
          case Payload.Message(Message.Assistant(blocks, _, _, _)) =>
            blocks.collect { case AssistantBlock.Text(t) => t }.mkString
          case Payload.Message(other) => s"unexpected reply: $other"
        }
      case Right(None) => "(no reply recorded)"
      case Left(error) => s"(unreadable: $error)"
    }
}
