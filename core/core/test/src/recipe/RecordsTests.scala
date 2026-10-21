package grit.core.recipe

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, EntrySeq, ToolCallId, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Usage}
import grit.core.store.{Entry, Payload}
import grit.core.visibility.Integrity

import utest.*

/** [[Records.query]]: what the records near a heard message are searched by. */
object RecordsTests extends TestSuite {

  private val t = Instant.parse("2026-10-20T12:00:00Z")

  private def entry(n: Long, payload: Payload) =
    Entry(
      EntryId(s"e$n"),
      ConversationId("c"),
      TurnSeq(n),
      None,
      EntrySeq(n),
      payload,
      t.plusSeconds(n),
      Integrity.Member
    )

  private def reply(text: String) =
    Payload.Message(
      Message.Assistant(Vector(AssistantBlock.Text(text)), StopReason.EndTurn, Usage.Zero, "m")
    )

  val tests = Tests {
    test("the query is the message, then the last 400 characters of the thread before it") {
      val heard = entry(4, Payload.Heard("where is the runbook?"))
      val before = Vector(
        entry(1, Payload.Message(Message.User("x" * 300))),
        entry(2, Payload.Message(Message.ToolResult(ToolCallId("t1"), "left out", false))),
        entry(3, reply("y" * 200))
      )
      (Records.query(heard, before), Records.query(heard, Vector.empty)) ==> (
        "where is the runbook?\n\n" + "x" * 198 + "\n\n" + "y" * 200,
        "where is the runbook?"
      )
    }
  }
}
