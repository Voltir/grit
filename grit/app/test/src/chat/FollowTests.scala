package grit.app.chat

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.store.{Entry, Payload}
import grit.dbos.engine.{RecordedStep, TurnStatus}

import utest.*

/** Following a conversation, with no database: what the screen is told as entries appear
  * and turns run, finish or fail.
  */
object FollowTests extends TestSuite {

  private val c = ConversationId("c")

  private def entry(seq: Long, turn: Long, message: Message): Entry =
    Entry(EntryId(s"e$seq"), c, TurnSeq(turn), None, seq, Payload.Message(message), Instant.EPOCH)

  private def user(seq: Long, turn: Long, text: String) = entry(seq, turn, Message.User(text))

  private def reply(seq: Long, turn: Long, text: String) = entry(
    seq,
    turn,
    Message.Assistant(
      Vector(AssistantBlock.Text(text)),
      StopReason.EndTurn,
      Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
      "m"
    )
  )

  private def running(recorded: String*): TurnStatus =
    TurnStatus.Running(recorded.toVector.map(RecordedStep(_, None, None)))

  private def all(status: TurnStatus): TurnRef => TurnStatus = _ => status

  val tests = Tests {
    test("the conversation so far arrives once, in order") {
      val entries = Vector(user(0, 0, "hi"), reply(1, 0, "hello"))
      val (next, msgs) = Follow.step(Follow.start, entries, all(TurnStatus.Unknown))
      msgs ==> Vector(
        ChatScreen.Msg
          .Arrived(
            Vector(
              ChatScreen.Said(ChatScreen.Voice.User, "hi"),
              ChatScreen.Said(ChatScreen.Voice.Reply, "hello")
            ),
            None
          )
      )
      Follow.step(next, entries, all(TurnStatus.Unknown))._2 ==> Vector.empty
    }

    test("a turn's tool loop arrives as one line per exchange, in order") {
      def exchange(seq: Long, m: Message) =
        Entry(EntryId(s"x$seq"), c, TurnSeq(0), None, seq, Payload.Exchange(m), Instant.EPOCH)
      val called = Message.Assistant(
        Vector(
          AssistantBlock.Text("let me look"),
          AssistantBlock
            .ToolCall(grit.core.id.ToolCallId("a"), "read", ujson.Obj("path" -> "x.txt")),
          AssistantBlock.ToolCall(grit.core.id.ToolCallId("b"), "list", ujson.Obj("depth" -> 2))
        ),
        StopReason.ToolUse,
        Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
        "m"
      )
      val entries = Vector(
        user(0, 0, "hi"),
        exchange(1, called),
        exchange(2, Message.ToolResult(grit.core.id.ToolCallId("a"), "one\ntwo\nthree", false)),
        exchange(3, Message.ToolResult(grit.core.id.ToolCallId("b"), "No such dir.\nat .", true)),
        reply(4, 0, "done")
      )
      val (_, msgs) = Follow.step(Follow.start, entries, all(TurnStatus.Unknown))
      msgs ==> Vector(
        ChatScreen.Msg.Arrived(
          Vector(
            ChatScreen.Said(ChatScreen.Voice.User, "hi"),
            ChatScreen.Said(ChatScreen.Voice.Tool, "read x.txt · list"),
            ChatScreen.Said(ChatScreen.Voice.Tool, "← 3 lines"),
            ChatScreen.Said(ChatScreen.Voice.Tool, "← No such dir."),
            ChatScreen.Said(ChatScreen.Voice.Reply, "done")
          ),
          None
        )
      )
    }

    test("the first look says what it saw, even an empty conversation, and only the first") {
      val (looked, msgs) = Follow.step(Follow.start, Vector.empty, all(TurnStatus.Unknown))
      msgs ==> Vector(ChatScreen.Msg.Arrived(Vector(), None))
      Follow.step(looked, Vector.empty, all(TurnStatus.Unknown))._2 ==> Vector.empty
    }

    test("a message without a reply, whose turn runs, is thinking until the reply lands") {
      val asked = Vector(user(0, 0, "hi"))
      val (thinking, first) = Follow.step(Follow.start, asked, all(running()))
      first ==> Vector(
        ChatScreen.Msg
          .Arrived(Vector(ChatScreen.Said(ChatScreen.Voice.User, "hi")), Some("classify"))
      )
      Follow.step(thinking, asked, all(running()))._2 ==> Vector.empty
      Follow
        .step(thinking, asked :+ reply(1, 0, "hello"), all(TurnStatus.Finished("replied")))
        ._2 ==>
        Vector(
          ChatScreen.Msg.Arrived(Vector(ChatScreen.Said(ChatScreen.Voice.Reply, "hello")), None)
        )
    }

    test("the screen is told each step the turn moves to, once") {
      val asked = Vector(user(0, 0, "hi"))
      val (assembling, _) = Follow.step(Follow.start, asked, all(running()))
      val (answering, moved) = Follow.step(assembling, asked, all(running("assemble")))
      moved ==> Vector(ChatScreen.Msg.Arrived(Vector(), Some("record-window")))
      Follow.step(answering, asked, all(running("assemble")))._2 ==> Vector.empty
    }

    test("a turn that finished with no reply is reported once, as failed") {
      val asked = Vector(user(0, 0, "hi"))
      val (after, msgs) =
        Follow.step(Follow.start, asked, all(TurnStatus.Finished("failed: Model(down)")))
      msgs ==> Vector(
        ChatScreen.Msg.Arrived(Vector(ChatScreen.Said(ChatScreen.Voice.User, "hi")), None),
        ChatScreen.Msg.Failed("failed: Model(down)")
      )
      Follow.step(after, asked, all(TurnStatus.Finished("failed: Model(down)")))._2 ==> Vector.empty
    }

    test("a turn's summary is told once, when it is written") {
      val answered = Vector(user(0, 0, "hi"), reply(1, 0, "hello"))
      val (seen, _) = Follow.step(Follow.start, answered, all(TurnStatus.Finished("replied")))
      val summary = Entry(
        EntryId("e2"),
        c,
        TurnSeq(0),
        None,
        2,
        Payload.Summary("greeted"),
        Instant.EPOCH
      )
      val (after, msgs) =
        Follow.step(seen, answered :+ summary, all(TurnStatus.Finished("replied")))
      msgs ==> Vector(
        ChatScreen.Msg.Arrived(Vector(), None, Vector(ChatScreen.Summarised(TurnSeq(0), "greeted")))
      )
      Follow.step(after, answered :+ summary, all(TurnStatus.Finished("replied")))._2 ==>
        Vector.empty
    }

    test("only the latest message's turn decides thinking") {
      val entries = Vector(user(0, 0, "old"), reply(1, 0, "answered"), user(2, 1, "new"))
      val asked: TurnRef => TurnStatus =
        t => if (t.turnSeq == TurnSeq(1)) running() else TurnStatus.Finished("x")
      Follow.step(Follow.start, entries, asked)._1.thinking ==> true
    }
  }
}
