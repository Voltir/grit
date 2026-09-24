package grit.assembly

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.ModelRequest

import utest.*

object CharEstimateTests extends TestSuite {

  private def assistant(blocks: AssistantBlock*): Message =
    Message.Assistant(
      blocks.toVector,
      StopReason.EndTurn,
      Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
      "test"
    )

  private def estimate(m: Message): Long = Tokens.value(CharEstimate.message(m))

  val tests = Tests {
    test("a request costs its system prompt, framed like a message, and every message") {
      Tokens.value(CharEstimate.system("abcde")) ==> 6
      Tokens.value(
        CharEstimate.request(ModelRequest("abcde", Vector(Message.User("abcd"), Message.User(""))))
      ) ==> 6 + 5 + 4
    }

    test("a message costs its framing plus a token per four characters, rounded up") {
      estimate(Message.User("")) ==> 4
      estimate(Message.User("abcd")) ==> 5
      estimate(Message.User("abcde")) ==> 6
      estimate(assistant(AssistantBlock.Text("abcd"), AssistantBlock.Text("efgh"))) ==> 6
    }

    test("an earlier reply's reasoning costs nothing: the model drops it") {
      estimate(assistant(AssistantBlock.Reasoning("x" * 400, None))) ==> 4
      estimate(assistant(AssistantBlock.Reasoning("", Some(ujson.Obj("a" -> "b" * 400))))) ==> 4
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
