package grit.app

import grit.core.*
import grit.dbos.TurnStatus
import java.time.Instant
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

  private def all(status: TurnStatus): TurnRef => TurnStatus = _ => status

  val tests = Tests {
    test("the conversation so far arrives once, in order") {
      val entries = Vector(user(0, 0, "hi"), reply(1, 0, "hello"))
      val (next, msgs) = Follow.step(Follow.start, entries, all(TurnStatus.Unknown))
      msgs ==> Vector(
        ChatScreen.Msg
          .Arrived(Vector(ChatScreen.Said(true, "hi"), ChatScreen.Said(false, "hello")), false)
      )
      Follow.step(next, entries, all(TurnStatus.Unknown))._2 ==> Vector.empty
    }

    test("a message without a reply, whose turn runs, is thinking until the reply lands") {
      val asked = Vector(user(0, 0, "hi"))
      val (thinking, first) = Follow.step(Follow.start, asked, all(TurnStatus.Running))
      first ==> Vector(ChatScreen.Msg.Arrived(Vector(ChatScreen.Said(true, "hi")), true))
      Follow.step(thinking, asked, all(TurnStatus.Running))._2 ==> Vector.empty
      Follow
        .step(thinking, asked :+ reply(1, 0, "hello"), all(TurnStatus.Finished("replied")))
        ._2 ==>
        Vector(ChatScreen.Msg.Arrived(Vector(ChatScreen.Said(false, "hello")), false))
    }

    test("a turn that finished with no reply is reported once, as failed") {
      val asked = Vector(user(0, 0, "hi"))
      val (after, msgs) =
        Follow.step(Follow.start, asked, all(TurnStatus.Finished("failed: Model(down)")))
      msgs ==> Vector(
        ChatScreen.Msg.Arrived(Vector(ChatScreen.Said(true, "hi")), false),
        ChatScreen.Msg.Failed("failed: Model(down)")
      )
      Follow.step(after, asked, all(TurnStatus.Finished("failed: Model(down)")))._2 ==> Vector.empty
    }

    test("only the latest message's turn decides thinking") {
      val entries = Vector(user(0, 0, "old"), reply(1, 0, "answered"), user(2, 1, "new"))
      val asked: TurnRef => TurnStatus =
        t => if (t.turnSeq == TurnSeq(1)) TurnStatus.Running else TurnStatus.Finished("x")
      Follow.step(Follow.start, entries, asked)._1.thinking ==> true
    }
  }
}
