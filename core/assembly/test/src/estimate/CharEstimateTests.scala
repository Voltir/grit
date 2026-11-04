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

  /** The recorded coding call in `estimate-coding-call.json`, and the input its provider
    * counted.
    */
  private def recorded: (ModelRequest, Long) = {
    val stream = getClass.getResourceAsStream("/estimate-coding-call.json")
    val json =
      try ujson.read(scala.io.Source.fromInputStream(stream, "UTF-8").mkString)
      finally stream.close()
    val request = ModelRequest(
      json("system").str,
      json("messages").arr.map(m => Message.User(m.str)).toVector,
      json("tools").arr
        .map(t => ToolSchema(t("name").str, t("description").str, t("parameters")))
        .toVector
    )
    (request, json("input").num.toLong)
  }

  val tests = Tests {
    test(
      "a recorded coding call, its system prompt, tools and brief, costs 1.00 to 1.15 of the input its provider counted"
    ) {
      // The fixture is scrubbed (its note says how), which moves the estimate by under 1%:
      // the band absorbs it. The exact check is a recorded turn rebuilt by `scripts/eval
      // context`, which gives every call's share.
      val (request, input) = recorded
      val share = Tokens.value(CharEstimate.request(request)).toDouble / input
      assert(share >= 1.00, share <= 1.15)
    }

    test("a request costs its system prompt, framed like a message, and every message") {
      Tokens.value(CharEstimate.system("x" * 11)) ==> 4 + 5
      Tokens.value(
        CharEstimate.request(
          ModelRequest("x" * 11, Vector(Message.User("x" * 22), Message.User("")))
        )
      ) ==> (4 + 5) + (4 + 10) + 4
    }

    test("text costs a token per 2.2 characters, rounded up, with no framing") {
      (
        Tokens.value(CharEstimate.text("")),
        Tokens.value(CharEstimate.text("x" * 11)),
        Tokens.value(CharEstimate.text("x" * 12))
      ) ==> (0, 5, 6)
    }

    test("a message costs its framing plus a token per 2.2 characters, rounded up") {
      estimate(Message.User("")) ==> 4
      estimate(Message.User("x" * 11)) ==> 9
      estimate(Message.User("x" * 12)) ==> 10
      estimate(assistant(AssistantBlock.Text("x" * 11), AssistantBlock.Text("y" * 11))) ==> 14
    }

    test(
      "a reasoning block the provider replays costs its text; one it cannot replay costs nothing"
    ) {
      (
        estimate(assistant(AssistantBlock.Reasoning("x" * 440, Some(ujson.Obj("a" -> "b"))))),
        estimate(assistant(AssistantBlock.Reasoning("x" * 440, None)))
      ) ==> (4 + 200, 4)
    }

    test(
      "a message costs at least the output its provider counted, unless it holds reasoning not sent back; then its characters"
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
        Tokens.value(CharEstimate.PerMessage) + 900
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
        math.ceil(chars / CharEstimate.schemaCharsPerToken).toLong +
          Tokens.value(CharEstimate.toolPreamble),
        0L,
        Tokens.value(CharEstimate.system("")) + Tokens.value(CharEstimate.tools(Vector(peek, look)))
      )
    }

    test("a tool call costs its id, name and arguments; a tool result its id and content") {
      // "id" + "look" + {"q":1} (7) = 13 characters, 6 tokens; "id" + "abcdef" = 8, 4.
      estimate(
        assistant(AssistantBlock.ToolCall(ToolCallId("id"), "look", ujson.Obj("q" -> 1)))
      ) ==> 4 + 6
      estimate(Message.ToolResult(ToolCallId("id"), "abcdef", isError = false)) ==> 4 + 4
    }
  }
}
