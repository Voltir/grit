package grit.assembly

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}

import utest.*

object TokenEstimateTests extends TestSuite {

  private def assistant(blocks: AssistantBlock*): Message =
    Message.Assistant(
      blocks.toVector,
      StopReason.EndTurn,
      Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
      "test"
    )

  private def estimate(m: Message): Long = Tokens.value(TokenEstimate.of(m))

  val tests = Tests {
    test("a message costs its framing plus a token per four characters, rounded up") {
      estimate(Message.User("")) ==> 4
      estimate(Message.User("abcd")) ==> 5
      estimate(Message.User("abcde")) ==> 6
      estimate(assistant(AssistantBlock.Text("abcd"), AssistantBlock.Text("efgh"))) ==> 6
    }

    test("reasoning costs its replay form, not its readable text") {
      estimate(assistant(AssistantBlock.Reasoning("x" * 400, None))) ==> 4
      // {"a":"bcdefgh"} is 15 characters.
      estimate(assistant(AssistantBlock.Reasoning("", Some(ujson.Obj("a" -> "bcdefgh"))))) ==> 8
    }

    test("a tool call costs its id, name and arguments; a tool result its id and content") {
      // "id" + "look" + {"q":1} (7) = 13 characters.
      estimate(
        assistant(AssistantBlock.ToolCall(ToolCallId("id"), "look", ujson.Obj("q" -> 1)))
      ) ==> 8
      estimate(Message.ToolResult(ToolCallId("id"), "abcdef", isError = false)) ==> 6
    }
  }
}
