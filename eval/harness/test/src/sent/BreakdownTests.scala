package grit.eval.harness.sent

import grit.assembly.estimate.CharEstimate
import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{ModelRequest, ToolSchema}
import grit.turn.TurnRecord.WindowMessage

import utest.*

/** A request's input by part ([[Breakdown]]), costed by a fixed estimator that recalibrating
  * grit's own does not move.
  */
object BreakdownTests extends TestSuite {

  private val estimator = new CharEstimate(4.0, 4.0, Tokens.Zero)

  private def t(n: Long) = Tokens(n)

  private def said(text: String): Message.Assistant =
    Message.Assistant(Vector(AssistantBlock.Text(text)), StopReason.EndTurn, Usage.Zero, "m")

  private val brief = Message.User("<brief>keep it short</brief>")
  private val recalledAsk = Message.User("what was it?")
  private val recalledReply = said("it was the blue one")
  private val dated = Message.User("The message you are answering was said today.")
  private val asker = Message.User("fix the parser")
  private val calling = Message.Assistant(
    Vector(AssistantBlock.ToolCall(ToolCallId("t1"), "read", ujson.Obj("path" -> "a.scala"))),
    StopReason.ToolUse,
    Usage.Zero,
    "m"
  )
  private val result = Message.ToolResult(ToolCallId("t1"), "object A { def b = 1 }", false)

  private val request = ModelRequest(
    "You are grit.",
    Vector(brief, recalledAsk, recalledReply, dated, asker, calling, result),
    Vector(ToolSchema("read", "Read a file.", ujson.Obj("type" -> "object")))
  )

  private val window: Vector[WindowMessage] =
    Vector(WindowMessage.Brief, WindowMessage.Recalled, WindowMessage.Recalled)

  private def cost(ms: Message*): Tokens = ms.map(estimator.message).foldLeft(t(0))(_ + _)

  val tests = Tests {
    test(
      "a request's parts are its estimator's costs: the window's messages as it names them, the rest by kind"
    ) {
      val parts = Breakdown.of(request, window, estimator)
      parts ==> Breakdown(
        estimator.system(request.system),
        estimator.tools(request.tools),
        cost(brief, dated, asker),
        cost(calling),
        cost(result),
        cost(recalledAsk, recalledReply)
      )
      parts.total ==> estimator.request(request)
    }

    test("the messages from an index on are costed alone, without the system prompt or tools") {
      Breakdown.messages(request, 4, window, estimator) ==>
        Breakdown(t(0), t(0), cost(asker), cost(calling), cost(result), t(0))
    }

    test("scaled parts sum to the count, each its exact share rounded down or up by one") {
      Breakdown(t(10), t(20), t(30), t(0), t(0), t(40)).scaled(t(7)) ==>
        Some(Breakdown(t(1), t(1), t(2), t(0), t(0), t(3)))
    }

    test("a remainder shared by equal fractions goes to the earlier parts") {
      Breakdown(t(1), t(1), t(1), t(0), t(0), t(0)).scaled(t(10)) ==>
        Some(Breakdown(t(4), t(3), t(3), t(0), t(0), t(0)))
    }

    test("nothing to scale, or a count below 0, scales to nothing") {
      (Breakdown.Zero.scaled(t(5)), Breakdown(t(1), t(0), t(0), t(0), t(0), t(0)).scaled(t(-1))) ==>
        (None, None)
    }
  }
}
