package grit.app.main

import java.nio.file.{Files, Path}

import grit.core.id.{ClaimHolder, ConversationId, Harness, SessionId, ToolCallId}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.TestPostgres
import grit.turn.Turn

import utest.*

/** A turn whose model calls `read`, against Postgres and DBOS with the real coding tools over a
  * temporary checkout and the stub provider (no model is paid for): the call is logged on its
  * conversation's session, which works as the holder the turn's sessions name.
  */
object TurnSessionLiveTests extends TestSuite {

  import LiveTurn.*

  /** Calls `read` on `notes.txt` while the last message is the user's; after its result,
    * answers.
    */
  private final class Reading extends Provider {
    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      val usage = Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0)))
      Right(request.messages.lastOption match {
        case Some(Message.ToolResult(_, _, _)) =>
          Message.Assistant(Vector(AssistantBlock.Text("read")), StopReason.EndTurn, usage, "m")
        case _ =>
          Message.Assistant(
            Vector(
              AssistantBlock.ToolCall(ToolCallId("r1"), "read", ujson.Obj("path" -> "notes.txt"))
            ),
            StopReason.ToolUse,
            usage,
            "m"
          )
      })
    }
  }

  /** A checkout holding `notes.txt`, handed to `body`, then deleted. */
  private def checkout[A](body: Path => A): A = {
    val root = Files.createTempDirectory("grit-session")
    try {
      val _ = Files.writeString(root.resolve("notes.txt"), "a draft\n")
      body(root)
    } finally
      Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.delete(p))
  }

  private val holder = ClaimHolder.of("coder/abc").fold(sys.error, identity)

  val tests = Tests {
    test("a turn's read is logged on its conversation's session, working as its holder") {
      checkout { root =>
        val engine = LiveEngine.open(TestPostgres.freshDatabase("turn_session"), Turn.Epoch)
        try {
          launchIn(engine, engine.entries, new Reading, root, holder = _ => Some(holder))
          val turn = say(engine, "read it")
          val _ = engine.awaitTurn(turn)
          val id = SessionId.of(Harness.Grit, ConversationId.value(turn.conversationId))
          val lately = id.left
            .map(grit.core.store.StoreError.Invalid(_))
            .flatMap(engine.sessions.lately(origin(root).room, _))
          lately.map(
            _.map(l =>
              (l.session.holder, l.session.counters.toolCalls, l.read.map(_.name), l.session.by.id)
            )
          ) ==> Right(Some((Some(holder), 1L, Vector("notes.txt"), grit.core.id.PrincipalId.Local)))
        } finally engine.close()
      }
    }
  }
}
