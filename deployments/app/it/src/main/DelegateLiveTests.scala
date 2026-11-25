package grit.app.main

import grit.core.agent.{Subagent, SubagentFixtures, Subagents}
import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{Delta, ModelRequest, Provider, ProviderError}
import grit.core.store.Payload
import grit.core.visibility.{Integrity, Subject}
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.TestPostgres
import grit.models.StubModels
import grit.turn.{Spawnable, Turn}

import utest.*

/** A turn that hands a task to a run with `delegate`, against Postgres and DBOS: its child is a
  * turn workflow of its own, and the parent keeps its reply as the call's result.
  */
object DelegateLiveTests extends TestSuite {
  import LiveTurn.*

  private def right[A](e: Either[Any, A]): A = e.fold(x => sys.error(x.toString), identity)

  private val brief = "Find the paper."

  /** A scholar whose calls go to the turn's provider, believing a brief from anyone. */
  private val scholars: Spawnable = Spawnable.of(
    right(
      Subagent
        .of(
          SubagentFixtures.scholar,
          SubagentFixtures.persona,
          StubModels.Catalog.pin.turn,
          SubagentFixtures.services,
          SubagentFixtures.spendCap,
          Integrity.Untrusted
        )
        .flatMap(s => Subagents.of(Vector(s)))
    ),
    _ => Vector.empty
  )

  /** The parent's model calls `delegate` first and answers once it has a result; the child's,
    * shown its brief, replies at once.
    */
  private final class Delegating extends Provider {
    private def said(blocks: Vector[AssistantBlock], stop: StopReason): Message.Assistant =
      Message.Assistant(blocks, stop, Usage(Tokens(10), Tokens(2), Tokens.Zero, None), "m")

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      val texts = request.messages.collect { case Message.User(text) => text }
      val results = request.messages.collect { case r: Message.ToolResult => r }
      Right(
        if (texts.exists(_.contains(brief)))
          said(Vector(AssistantBlock.Text("Found it.")), StopReason.EndTurn)
        else if (results.isEmpty)
          said(
            Vector(
              AssistantBlock.ToolCall(
                ToolCallId("d1"),
                "delegate",
                ujson.Obj("agent" -> "scholar", "task" -> brief)
              )
            ),
            StopReason.ToolUse
          )
        else said(Vector(AssistantBlock.Text("done")), StopReason.EndTurn)
      )
    }

    override def stream(
        request: ModelRequest,
        onDelta: Delta => Unit
    ): Either[ProviderError, Message.Assistant] = complete(request)
  }

  val tests = Tests {
    test(
      "a turn's delegate runs its child as a workflow and keeps its reply as the call's result"
    ) {
      val engine = LiveEngine.open(TestPostgres.freshDatabase("delegate"), Turn.Epoch)
      try {
        launchIn(
          engine,
          engine.entries,
          new Delegating,
          java.nio.file.Path.of("").toAbsolutePath,
          subagents = scholars
        )
        val turn = say(engine, "delegating")
        val _ = engine.awaitTurn(turn)
        val results = right(engine.db.read(Subject.Turn(turn))(engine.entries.ofTurn(turn)))
          .map(_.payload)
          .collect { case Payload.Result(r, _) => r.content }
        // The stub prices nothing, so the reply says no spend.
        results ==> Vector("The scholar run replied:\n<reply>\nFound it.\n</reply>")
      } finally engine.close()
    }
  }
}
