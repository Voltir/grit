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

    test("a turn's tool loop arrives as one line per call, with what it came to, in order") {
      def at(seq: Long, p: Payload) =
        Entry(EntryId(s"x$seq"), c, TurnSeq(0), None, seq, p, Instant.EPOCH)
      def result(seq: Long, id: String, content: String, failed: Boolean, shown: String) =
        at(
          seq,
          Payload.Result(Message.ToolResult(grit.core.id.ToolCallId(id), content, failed), shown)
        )
      val called: Message.Assistant = Message.Assistant(
        Vector(
          AssistantBlock.Text("let me look"),
          AssistantBlock
            .ToolCall(grit.core.id.ToolCallId("a"), "read", ujson.Obj("path" -> "x.txt")),
          AssistantBlock.ToolCall(grit.core.id.ToolCallId("b"), "list", ujson.Obj("depth" -> 2)),
          AssistantBlock.ToolCall(
            grit.core.id.ToolCallId("s"),
            "search",
            ujson.Obj("path" -> "grit/turn", "pattern" -> "Round\\b")
          )
        ),
        StopReason.ToolUse,
        Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
        "m"
      )
      val entries = Vector(
        user(0, 0, "hi"),
        at(1, Payload.Exchange(called)),
        result(2, "a", "one\ntwo\nthree", false, "read x.txt"),
        result(3, "b", "No such dir.\nat .", true, "list"),
        result(4, "s", "a:1: x", false, "search \"Round\\b\" grit/turn"),
        // A call that did not read is shown by the name it sent.
        result(5, "u", "There is no tool named `sing`.", true, "sing"),
        reply(7, 0, "done")
      )
      val (_, msgs) = Follow.step(Follow.start, entries, all(TurnStatus.Unknown))
      msgs ==> Vector(
        ChatScreen.Msg.Arrived(
          Vector(
            ChatScreen.Said(ChatScreen.Voice.User, "hi"),
            ChatScreen.Said(ChatScreen.Voice.Tool, "read x.txt ← 3 lines"),
            ChatScreen.Said(ChatScreen.Voice.Tool, "list ← No such dir."),
            ChatScreen.Said(ChatScreen.Voice.Tool, "search \"Round\\b\" grit/turn ← 1 line"),
            ChatScreen.Said(ChatScreen.Voice.Tool, "sing ← There is no tool named `sing`."),
            ChatScreen.Said(ChatScreen.Voice.Reply, "done")
          ),
          None
        )
      )
    }

    test("a running turn's call that asks is told once, and put away once it moves on") {
      val call = grit.core.id.ToolCallId("t1")
      def at(seq: Long, p: Payload) =
        Entry(EntryId(s"x$seq"), c, TurnSeq(0), None, seq, p, Instant.EPOCH)
      val asking = Vector(user(0, 0, "edit it"), at(1, Payload.Ask(call, "Edit a.txt")))
      val turn = TurnRef(c, TurnSeq(0))
      val (waiting, first) = Follow.step(Follow.start, asking, all(running("ask:0:0")))
      first.lastOption ==>
        Some(ChatScreen.Msg.Asking(Some(ChatScreen.Asked(turn.workflowId, call, "Edit a.txt"))))
      Follow.step(waiting, asking, all(running("ask:0:0")))._2 ==> Vector.empty
      val begun = asking :+ at(2, Payload.Attempt(call))
      Follow.step(waiting, begun, all(running("ask:0:0")))._2.lastOption ==>
        Some(ChatScreen.Msg.Asking(None))
      val done = asking :+ at(
        2,
        Payload.Result(Message.ToolResult(call, "denied", true), "edit a.txt")
      )
      Follow.step(waiting, done, all(running("tool:0:0")))._2.lastOption ==>
        Some(ChatScreen.Msg.Asking(None))
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
          .Arrived(Vector(ChatScreen.Said(ChatScreen.Voice.User, "hi")), Some("pin-models"))
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
