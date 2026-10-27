package grit.app.main

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import grit.core.approval.Approval
import grit.core.id.{ToolCallId, TurnRef}
import grit.core.identity.Account
import grit.core.inbox.InboxError
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.store.{Entry, Payload}
import grit.core.visibility.Subject
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.TestPostgres
import grit.turn.Turn

import utest.*

/** A turn whose model calls `edit`, against Postgres and DBOS with the real coding tools
  * over a temporary checkout: the call asks, waits for its answer through the inbox, and
  * runs only when approved; the wait survives a restart.
  */
object ApprovalLiveTests extends TestSuite {
  import LiveTurn.*

  private val call = ToolCallId("edit-1")

  /** Calls `edit` on `notes.txt` while the last message is the user's; after a result,
    * answers with it, and after a person's message that follows one, with that.
    */
  private final class Editing extends Provider {
    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      val usage = Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0)))
      val args = ujson.Obj(
        "path" -> "notes.txt",
        "edits" -> ujson.Arr(ujson.Obj("oldText" -> "draft", "newText" -> "final"))
      )
      def answer(text: String): Message.Assistant =
        Message.Assistant(Vector(AssistantBlock.Text(text)), StopReason.EndTurn, usage, "m")
      val called = request.messages.exists(_.isInstanceOf[Message.ToolResult])
      Right(request.messages.lastOption match {
        case Some(Message.ToolResult(_, content, _)) => answer(s"did: $content")
        // The person's words after its result: a declined call's reason.
        case Some(Message.User(said)) if called => answer(s"heard: $said")
        case _ =>
          Message.Assistant(
            Vector(AssistantBlock.ToolCall(call, "edit", args)),
            StopReason.ToolUse,
            usage,
            "m"
          )
      })
    }
  }

  /** A checkout holding `notes.txt`, handed to `body`, then deleted. */
  private def checkout[A](body: Path => A): A = {
    val root = Files.createTempDirectory("grit-approval")
    try {
      val _ = Files.writeString(root.resolve("notes.txt"), "a draft\n")
      body(root)
    } finally
      Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.delete(p))
  }

  private def own(engine: Engine^, turn: TurnRef): Vector[Entry] =
    engine.db
      .read(Subject.Public)(engine.entries.list(turn.conversationId))
      .getOrElse(Vector.empty)
      .filter(_.turnSeq == turn.turnSeq)

  /** The Ask entries of `turn`, once one is there. */
  private def asked(engine: Engine^, turn: TurnRef): Vector[Payload.Ask] = {
    val until = System.nanoTime() + 60.seconds.toNanos
    def asks = own(engine, turn).collect { case Entry(_, _, _, _, _, a: Payload.Ask, _, _) => a }
    while (asks.isEmpty && System.nanoTime() < until) Thread.sleep(50)
    asks
  }

  /** What `turn`'s call came to, as the model read it. */
  private def results(engine: Engine^, turn: TurnRef): Vector[(String, Boolean)] =
    own(engine, turn).collect {
      case Entry(_, _, _, _, _, Payload.Result(Message.ToolResult(_, content, failed), _), _, _) =>
        (content, failed)
    }

  val tests = Tests {
    test("an edit asks, runs once approved, and a second answer is ignored") {
      checkout { root =>
        val engine = LiveEngine.open(TestPostgres.freshDatabase("approve_edit"), Turn.Epoch)
        try {
          launchIn(engine, engine.entries, new Editing, root, all = true)
          val turn = say(engine, "edit it")
          val asks = asked(engine, turn)
          asks.map(_.call) ==> Vector(call)
          assert(asks.forall(_.shown.startsWith("Edit notes.txt")))
          // Nothing has run while it waits.
          Files.readString(root.resolve("notes.txt")) ==> "a draft\n"
          engine.inbox.answer(turn.workflowId, call, Approval.Approved, Account.Local) ==> Right(())
          // Sent before the turn reads the first it is ignored; after the call settles, Closed.
          val second =
            engine.inbox.answer(turn.workflowId, call, Approval.Declined(None), Account.Local)
          assert(second == Right(()) || second == Left(InboxError.Closed(turn.workflowId, call)))
          val _ = engine.awaitTurn(turn)
          Files.readString(root.resolve("notes.txt")) ==> "a final\n"
          results(engine, turn).map(_._2) ==> Vector(false)
          // The result as the model reads it, fenced (Shown.turn).
          reply(engine, turn) ==> Some(
            "did: <tool_output source=\"edit notes.txt\">\n" +
              "Edited notes.txt: 1 replacement, starting on line 1.\n</tool_output>"
          )
        } finally engine.close()
      }
    }

    test("a declined edit does not run, and the model reads the reason as the person's message") {
      checkout { root =>
        val engine = LiveEngine.open(TestPostgres.freshDatabase("decline_edit"), Turn.Epoch)
        try {
          launchIn(engine, engine.entries, new Editing, root, all = true)
          val turn = say(engine, "edit it")
          val _ = asked(engine, turn)
          engine.inbox
            .answer(turn.workflowId, call, Approval.Declined(Some("not yet")), Account.Local) ==>
            Right(())
          val _ = engine.awaitTurn(turn)
          Files.readString(root.resolve("notes.txt")) ==> "a draft\n"
          results(engine, turn) ==> Vector(
            (
              "Declined: the person you are answering declined this call; it did not run.",
              true
            )
          )
          // The person's reason as Payload.declined renders it.
          reply(engine, turn) ==> Some(
            "heard: The person you are answering declined the call edit notes.txt, and wrote:\n" +
              "not yet"
          )
        } finally engine.close()
      }
    }

    test("an edit nobody answers in time is unanswered, not declined, and does not run") {
      checkout { root =>
        val engine = LiveEngine.open(TestPostgres.freshDatabase("timeout_edit"), Turn.Epoch)
        try {
          launchIn(engine, engine.entries, new Editing, root, all = true, answerWithin = 1.second)
          val turn = say(engine, "edit it")
          val _ = engine.awaitTurn(turn)
          Files.readString(root.resolve("notes.txt")) ==> "a draft\n"
          results(engine, turn).map(_._1) ==> Vector(
            "Unanswered: nobody answered in time; the call did not run."
          )
        } finally engine.close()
      }
    }

    test("a restart while an edit waits: it still waits, then runs once approved") {
      checkout { root =>
        val config = TestPostgres.freshDatabase("restart_edit")
        val before = LiveEngine.open(config, Turn.Epoch)
        val turn =
          try {
            launchIn(before, before.entries, new Editing, root, all = true)
            val t = say(before, "edit it")
            val _ = asked(before, t)
            t
          } finally before.close()
        Files.readString(root.resolve("notes.txt")) ==> "a draft\n"
        val after = LiveEngine.open(config, Turn.Epoch)
        try {
          launchIn(after, after.entries, new Editing, root, all = true)
          after.inbox.answer(turn.workflowId, call, Approval.Approved, Account.Local) ==> Right(())
          val _ = after.awaitTurn(turn)
          Files.readString(root.resolve("notes.txt")) ==> "a final\n"
          // Asked once: the recovered turn replayed its ask, and did not ask again.
          asked(after, turn).size ==> 1
          results(after, turn).map(_._2) ==> Vector(false)
        } finally after.close()
      }
    }
  }
}
