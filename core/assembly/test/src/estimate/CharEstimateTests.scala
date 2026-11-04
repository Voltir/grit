package grit.assembly.estimate

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{ModelRequest, ToolSchema}

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

    test("text costs a token per four characters, rounded up, with no framing") {
      (
        Tokens.value(CharEstimate.text("")),
        Tokens.value(CharEstimate.text("abcd")),
        Tokens.value(CharEstimate.text("abcde"))
      ) ==> (0, 1, 2)
    }

    test("a message costs its framing plus a token per four characters, rounded up") {
      estimate(Message.User("")) ==> 4
      estimate(Message.User("abcd")) ==> 5
      estimate(Message.User("abcde")) ==> 6
      estimate(assistant(AssistantBlock.Text("abcd"), AssistantBlock.Text("efgh"))) ==> 6
    }

    test(
      "a reasoning block the provider replays costs its text; one it cannot replay costs nothing"
    ) {
      (
        estimate(assistant(AssistantBlock.Reasoning("x" * 400, Some(ujson.Obj("a" -> "b"))))),
        estimate(assistant(AssistantBlock.Reasoning("x" * 400, None)))
      ) ==> (4 + 100, 4)
    }

    test(
      "a message that replays reasoning costs at least the output its provider counted; one replaying none costs its characters"
    ) {
      val replayed = AssistantBlock.Reasoning("x" * 40, Some(ujson.Obj("a" -> "b")))
      val unreplayed = AssistantBlock.Reasoning("x" * 40, None)
      val text = AssistantBlock.Text("y" * 40)
      def said(output: Long, blocks: AssistantBlock*): Long =
        estimate(
          Message.Assistant(
            blocks.toVector,
            StopReason.EndTurn,
            Usage(Tokens(5), Tokens(output), Tokens.Zero, None),
            "test"
          )
        )
      val chars = estimate(assistant(replayed, text))
      (
        said(900, replayed, text),
        said(1, replayed, text),
        said(900, unreplayed, text),
        said(900, text)
      ) ==> (
        Tokens.value(CharEstimate.PerMessage) + 900,
        chars,
        estimate(assistant(text)),
        estimate(assistant(text))
      )
    }

    test(
      "offered tools cost their definitions at the schema ratio, rounded up, and the tool preamble once; none cost nothing"
    ) {
      val peek = ToolSchema("peek", "Reads.", ujson.Obj("type" -> "object"))
      val look = ToolSchema("look", "Looks.", ujson.Obj())
      // "peek\nReads.\n{"type":"object"}" (29) + "\n" + "look\nLooks.\n{}" (14) = 44.
      val chars = 44
      (
        Tokens.value(CharEstimate.tools(Vector(peek, look))),
        Tokens.value(CharEstimate.tools(Vector.empty)),
        Tokens.value(CharEstimate.request(ModelRequest("", Vector.empty, Vector(peek, look))))
      ) ==> (
        math.ceil(chars / CharEstimate.SchemaCharsPerToken).toLong +
          Tokens.value(CharEstimate.ToolPreamble),
        0L,
        Tokens.value(CharEstimate.system("")) + Tokens.value(CharEstimate.tools(Vector(peek, look)))
      )
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
