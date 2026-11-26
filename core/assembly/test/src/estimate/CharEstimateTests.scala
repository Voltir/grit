package grit.assembly.estimate

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Density, Settings}
import grit.core.provider.{ModelRequest, TokenEstimator, ToolSchema}

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

  /** The density fitted for `z-ai/glm-5.3-flash` from its ledger rows, as its catalog profile
    * gives it.
    */
  private val Glm: Density = Density.of(3.08) match {
    case Right(d) => d
    case Left(why) => throw new java.lang.AssertionError(why)
  }

  /** The recorded calls in `estimate-glm-calls.json`, each request and the input its provider
    * counted.
    */
  private def recordedOnGlm: Vector[(ModelRequest, Long)] = {
    val stream = getClass.getResourceAsStream("/estimate-glm-calls.json")
    val json =
      try ujson.read(scala.io.Source.fromInputStream(stream, "UTF-8").mkString)
      finally stream.close()
    def message(m: ujson.Value): Message = m("role").str match {
      case "user" => Message.User(m("text").str)
      case "tool" => Message.ToolResult(ToolCallId(m("id").str), m("content").str, m("error").bool)
      case _ =>
        Message.Assistant(
          m("blocks").arr.toVector.map { b =>
            val o = b.obj
            if (o.contains("text")) AssistantBlock.Text(b("text").str)
            else if (o.contains("reasoning"))
              AssistantBlock.Reasoning(
                b("reasoning").str,
                Option.when(b("replayed").bool)(ujson.Arr())
              )
            else AssistantBlock.ToolCall(ToolCallId(b("call").str), b("name").str, b("arguments"))
          },
          StopReason.ToolUse,
          Usage(Tokens.Zero, Tokens(m("output").num.toLong), Tokens.Zero, None),
          "z-ai/glm-5.3-flash"
        )
    }
    json("turns").arr.toVector.flatMap { turn =>
      val messages = turn("messages").arr.toVector.map(message)
      val tools = turn("tools").arr.toVector
        .map(t =>
          ToolSchema(t("name").str, t("description").str, t("parameters"), t("strict").bool)
        )
      turn("calls").arr.toVector.map(c =>
        (
          ModelRequest(turn("system").str, messages.take(c("messages").num.toInt), tools),
          c("input").num.toLong
        )
      )
    }
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

    test(
      "at a pair's fitted density, two dozen recorded coding calls on it cost no less than the input their provider counted, and at most 1.20 of it at the median"
    ) {
      // The fixture's note says where the calls came from; at 2.2 they cost 1.53 to 2.04 of it.
      val shares = recordedOnGlm.map((request, input) =>
        Tokens.value(CharEstimate.withDensity(Glm).request(request)).toDouble / input
      )
      val sorted = shares.sorted
      val median = (sorted((sorted.size - 1) / 2) + sorted(sorted.size / 2)) / 2
      (shares.size, shares.filter(_ < 1.0)) ==> (24, Vector.empty)
      assert(median <= 1.20)
    }

    test("at a density, text costs a token per that many characters, rounded up") {
      val at = CharEstimate.withDensity(Glm)
      (
        Tokens.value(at.text("x" * 308)),
        Tokens.value(at.text("x" * 309)),
        Tokens.value(at.message(Message.User("x" * 308)))
      ) ==> (100, 101, 104)
    }

    test("at a density, tool definitions keep their ratio to text, 3.0 to 2.2") {
      val tool = ToolSchema("t", "d" * 412, ujson.Obj())
      val text = TokenEstimator.definitions(Vector(tool)).length
      // At 3.08 characters a token of text, tool definitions run at 4.2.
      Tokens.value(CharEstimate.withDensity(Glm).tools(Vector(tool))) ==>
        300 + math.ceil(text / 4.2).toLong
    }

    test("under settings with no density a request costs what it costs; with one, at it") {
      val (request, _) = recorded
      val none = Settings.of(None)
      CharEstimate.withSettings(none).request(request) ==> CharEstimate.request(request)
      Tokens.value(
        CharEstimate.withSettings(none.copy(density = Some(Glm))).system("x" * 308)
      ) ==> 104
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
