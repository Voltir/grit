package grit.turn

import java.time.{Instant, ZoneId, ZoneOffset}

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{AfterToolResult, ToolGuidance}
import grit.core.provider.{ModelRequest, ToolUse}

import utest.*

/** What [[TurnShown]] tells a turn's model besides its window: the line dating its message, and
  * how each call of its loop is shaped from the plain request.
  */
object TurnShownTests extends TestSuite {

  private val At = Instant.parse("2026-10-21T15:03:00Z")

  private val (pacific, eastern) = (ZoneId.of("America/Los_Angeles"), ZoneId.of("America/New_York"))

  /** The tail every zoned line carries, its times read as `short`. */
  private def tail(short: String): String =
    s"People here give times in their own zone, not UTC: read the times the person gives as $short " +
      s"unless they say otherwise, and answer with times in $short. Times grit shows you in your " +
      "view are UTC."

  val tests = Tests {
    test(
      "the answering line dates the message in the person's zone, says whose, then UTC, and how to read times"
    ) {
      val personal = "The message you are answering was said Wed 2026-10-21 08:03 PDT, the " +
        s"person's time (America/Los_Angeles); 15:03 UTC. ${tail("PDT")}"
      (
        TurnShown.answering(At, Some(TurnOffer.Zoned(Some(pacific), eastern)), direct = true),
        TurnShown.answering(At, Some(TurnOffer.Zoned(None, pacific)), direct = true),
        TurnShown.answering(At, None, direct = true),
        TurnShown.answering(At, Some(TurnOffer.Zoned(Some(pacific), eastern)), direct = false)
      ) ==> (
        personal,
        "The message you are answering was said Wed 2026-10-21 08:03 PDT, this deployment's " +
          "time (America/Los_Angeles); the person's own zone is not known; 15:03 UTC. " +
          tail("PDT"),
        "The message you are answering was said Wed 2026-10-21 15:03 UTC.",
        s"$personal Read times other people here give as EDT, this deployment's time, unless " +
          "they say otherwise."
      )
    }

    test("the UTC time carries its date when it falls on another day than the dated one") {
      TurnShown.answering(
        Instant.parse("2026-10-22T06:30:00Z"),
        Some(TurnOffer.Zoned(Some(pacific), eastern)),
        direct = true
      ) ==> "The message you are answering was said Wed 2026-10-21 23:30 PDT, the person's time " +
        s"(America/Los_Angeles); 06:30 UTC Thu 2026-10-22. ${tail("PDT")}"
    }

    test("a deployment in UTC, under any of its names, is said as UTC") {
      Vector(ZoneOffset.UTC, ZoneId.of("UTC"), ZoneId.of("Etc/UTC")).map(z =>
        TurnShown.answering(At, Some(TurnOffer.Zoned(None, z)), direct = true)
      ) ==> Vector.fill(3)(
        "The message you are answering was said Wed 2026-10-21 15:03 UTC, this deployment's " +
          "time (UTC); the person's own zone is not known; 15:03 UTC. " + tail("UTC")
      )
    }

    test(
      "a turn asked about its item tags the same message on every call, so each call repeats the one before it"
    ) {
      def reply(text: String, calls: String*): Message.Assistant = Message.Assistant(
        AssistantBlock.Text(text) +: calls.toVector.map(id =>
          AssistantBlock.ToolCall(ToolCallId(id), "peek", ujson.Obj())
        ),
        if (calls.isEmpty) StopReason.EndTurn else StopReason.ToolUse,
        Usage(Tokens(1), Tokens(1), Tokens.Zero, None),
        "m"
      )
      val c = TurnAgenda.Classification(Vector.empty, None, Vector.empty, None, None)
      // The person's message, then the loop's exchange: a reply calling a tool, its result, a
      // declined call's reason as a user message after it, then a second round's.
      val asked = Vector(Message.User("earlier"), reply("before"), Message.User("read it"))
      val exchange = Vector(
        reply("", "t1"),
        Message.ToolResult(ToolCallId("t1"), "alpha", isError = false),
        Message.User("No, not that one.")
      )
      val second = Vector(reply("", "t2"), Message.ToolResult(ToolCallId("t2"), "beta", false))
      def shaped(use: ToolUse, messages: Vector[Message]): Vector[Message] =
        TurnShown
          .loopShape(
            Some(c),
            use,
            Vector.empty,
            AfterToolResult.UserMessage,
            ToolGuidance.SchemaOnly,
            Vector.empty
          )(ModelRequest("sys", messages))
          .messages
      val first = shaped(ToolUse.Auto, asked)
      first.lastOption ==> Some(Message.User(s"read it\n\n${TurnVerdict.tag(c)}"))
      (
        shaped(ToolUse.Auto, asked ++ exchange),
        shaped(ToolUse.Off, asked ++ exchange ++ second)
      ) ==> (
        first ++ exchange,
        first ++ exchange ++ second :+ Message.User(TurnLoop.LastCall)
      )
    }

    test("the loop's request marks stable every message but the last call's note") {
      val asked = Vector(Message.User("earlier"), Message.User("read it"))
      val call = Message.Assistant(
        Vector(AssistantBlock.ToolCall(ToolCallId("t1"), "peek", ujson.Obj())),
        StopReason.ToolUse,
        Usage(Tokens(1), Tokens(1), Tokens.Zero, None),
        "m"
      )
      val result = Message.ToolResult(ToolCallId("t1"), "alpha", isError = false)
      def marked(use: ToolUse, after: AfterToolResult): (Int, Int) = {
        val r = TurnShown.loopShape(
          None,
          use,
          Vector.empty,
          after,
          ToolGuidance.SchemaOnly,
          Vector.empty
        )(ModelRequest("sys", asked ++ Vector(call, result)))
        (r.stable, r.messages.size)
      }
      // A later call repeats every message a call with tools is sent; the last call's note is
      // its own, after the messages or, where a pair refuses a user there, ending the last
      // result, which no later call repeats either.
      Vector(
        marked(ToolUse.Auto, AfterToolResult.UserMessage),
        marked(ToolUse.Off, AfterToolResult.UserMessage),
        marked(ToolUse.Auto, AfterToolResult.InLastResult),
        marked(ToolUse.Off, AfterToolResult.InLastResult)
      ) ==> Vector((4, 4), (4, 5), (4, 4), (3, 4))
    }
  }
}
