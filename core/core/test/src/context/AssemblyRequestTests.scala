package grit.core.context

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnRef, TurnSeq}
import grit.core.message.Message
import grit.core.store.{Entry, Payload, WentOn}
import grit.core.visibility.Integrity

import utest.*

/** What a boundary may draw of a conversation's earlier turns ([[AssemblyRequest.drawable]]). */
object AssemblyRequestTests extends TestSuite {

  private val c = ConversationId("c")

  private def entry(turn: Long, seq: Long, payload: Payload): Entry =
    Entry(
      EntryId(s"t$turn:$seq"),
      c,
      TurnSeq(turn),
      None,
      EntrySeq(seq),
      payload,
      Instant.EPOCH,
      Integrity.Member
    )

  private val continued = Payload.Continuation(TurnSeq.First, 1, WentOn.AtLimit)

  private def continuing(task: Option[EntrySeq]): AssemblyRequest =
    AssemblyRequest(TurnRef(c, TurnSeq(2)), Width.Deployed, CarriedWindow.Empty, task = task)

  val tests = Tests {
    test("a continuation whose newest earlier turn held only its note may draw no turn") {
      val turns = Vector(
        Vector(entry(0, 0, Payload.Message(Message.User("the task")))),
        Vector(entry(1, 1, continued))
      )
      (continuing(Some(EntrySeq(0))).drawable(turns), continuing(None).drawable(turns)) ==>
        (Vector.empty, turns)
    }
  }
}
