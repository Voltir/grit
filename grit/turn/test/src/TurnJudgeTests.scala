package grit.turn

import java.time.Instant

import grit.core.classify.StateJson
import grit.core.context.Window
import grit.core.id.{ConversationId, EntryId, PeriodSeq, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Usage}
import grit.core.period.{CloseReason, TestClosings}
import grit.core.place.Place
import grit.core.store.{Entry, Nearby, Payload, Speakers}

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
    test(
      "the judge sees every message so far under names, replies after the root included, and only the records it recalled"
    ) {
      val state = TurnJudge.state(all, Window(all.map(_.id)), Vector.empty, names, "draft")
      state.thread ==>
        "Ana: when is the freeze?\nAssistant: Thursday.\nBen: is it still on?\nSomeone: said after"
      assert(state.recalled.contains("The freeze moved."), !state.recalled.contains("when is"))
    }

    test(
      "a heard question's thread is its own messages; what its window recalled, own or nearby, is only in recalled"
    ) {
      val place = Place.read("slack:T1/C1/1.0").fold(e => sys.error(e), identity)
      def other(n: Long, conversation: String, payload: Payload): Entry =
        Entry(
          EntryId(s"o$n"),
          ConversationId(conversation),
          TurnSeq(0),
          None,
          0,
          payload,
          Instant.EPOCH
        )
      val decided = other(
        1,
        "d",
        Payload.Closed(PeriodSeq.First, CloseReason.Lapsed, TestClosings.prose("A 12-month term."))
      )
      val markup = other(2, "m", Payload.Heard("I marked up the contract."))
      val question = entry(1, Payload.Heard("where did we land on the term?"))
      val state = TurnJudge.state(
        Vector(record, question),
        Window(
          Vector(record.id, question.id),
          Vector.empty,
          Vector(
            Nearby.Closed(ConversationId("d"), place, decided.id),
            Nearby.Open(ConversationId("m"), place, Vector(markup.id))
          )
        ),
        Vector(decided, markup),
        Speakers(Map(question.id -> "Nick")),
        "draft"
      )
      state.thread ==> "Nick: where did we land on the term?"
      assert(
        state.recalled.contains("The freeze moved."),
        state.recalled.contains("A 12-month term."),
        state.recalled.contains("I marked up the contract."),
        !state.recalled.contains("where did we land")
      )
    }

    test("a window with no record recalls nothing") {
      TurnJudge.state(all, Window(Vector(asked.id)), Vector.empty, names, "d").recalled ==> ""
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
