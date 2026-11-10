package grit.core.message

import utest.*

/** What a message says of itself ([[Message]]). */
object MessageTests extends TestSuite {

  private def holding(blocks: AssistantBlock*): Message.Assistant =
    Message.Assistant(blocks.toVector, StopReason.EndTurn, Usage.Zero, "m")

  val tests = Tests {
    test("an assistant message withholds only reasoning it returned nothing to replay") {
      (
        holding(AssistantBlock.Text("done"), AssistantBlock.Reasoning("thought", None)).withholds,
        holding(AssistantBlock.Reasoning("thought", Some(ujson.Obj("sig" -> "s")))).withholds,
        holding(AssistantBlock.Text("done")).withholds
      ) ==> (true, false, false)
    }

    test(
      "a reply's text is its text blocks run together in order, its reasoning and calls left out"
    ) {
      val blocks = Vector(
        AssistantBlock.Reasoning("thinking", None),
        AssistantBlock.Text("Moved a.txt"),
        AssistantBlock.ToolCall(grit.core.id.ToolCallId("t1"), "peek", ujson.Obj()),
        AssistantBlock.Text(" and b.txt.")
      )
      (AssistantBlock.text(blocks), AssistantBlock.text(blocks.take(1))) ==>
        ("Moved a.txt and b.txt.", "")
    }
  }
}
