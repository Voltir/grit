package grit.turn

import java.time.Instant

import grit.core.classify.StateJson
import grit.core.context.Window
import grit.core.id.{ConversationId, EntryId, PeriodSeq, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Usage}
import grit.core.period.{CloseReason, TestClosings}
import grit.core.store.{Entry, Payload, Speakers}

import utest.*

object TurnJudgeTests extends TestSuite {

  private def entry(n: Long, payload: Payload): Entry =
    Entry(EntryId(s"e$n"), ConversationId("c"), TurnSeq(n), None, n, payload, Instant.EPOCH)

  private val record =
    entry(
      0,
      Payload.Closed(PeriodSeq.First, CloseReason.Lapsed, TestClosings.prose("The freeze moved."))
    )
  private val asked = entry(1, Payload.Message(Message.User("when is the freeze?")))
  private val answered = entry(
    2,
    Payload.Message(
      Message.Assistant(
        Vector(AssistantBlock.Text("Thursday.")),
        StopReason.EndTurn,
        Usage.Zero,
        "m"
      )
    )
  )
  private val root = entry(3, Payload.Heard("is it still on?"))
  private val later = entry(4, Payload.Heard("said after"))
  private val all: Vector[Entry] = Vector(record, asked, answered, root, later)
  private val names = Speakers(Map(EntryId("e1") -> "Ana", EntryId("e3") -> "Ben"))

  val tests = Tests {
    test("the judge sees the thread to its root under names, and only the records it recalled") {
      val state = TurnJudge.state(all, root, Window(all.map(_.id)), Vector.empty, names, "draft")
      state.thread ==> "Ana: when is the freeze?\nAssistant: Thursday.\nBen: is it still on?"
      assert(state.recalled.contains("The freeze moved."), !state.recalled.contains("when is"))
    }

    test("a window with no record recalls nothing") {
      TurnJudge.state(all, root, Window(Vector(asked.id)), Vector.empty, names, "d").recalled ==> ""
    }

    test("the judge is shown the thread's end and the recalled's start, each cut to its limit") {
      val json = StateJson[TurnJudge.State].json(
        TurnJudge.State(
          "x" * 10 + "y" * TurnJudge.ThreadChars,
          "d",
          "a" * TurnJudge.RecalledChars + "b"
        )
      )
      (json("thread").str, json("recalled").str) ==> (
        "y" * TurnJudge.ThreadChars,
        "a" * TurnJudge.RecalledChars
      )
    }
  }
}
