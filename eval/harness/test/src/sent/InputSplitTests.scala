package grit.eval.harness.sent

import grit.assembly.estimate.CharEstimate
import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.Rates
import grit.core.provider.{ModelRequest, ToolSchema}

import utest.*

/** A call's input by part, split by how its provider counted it ([[InputSplit]]). */
object InputSplitTests extends TestSuite {

  private val estimator = new CharEstimate(4.0, 4.0, Tokens.Zero)

  private val asked = Message.User("x" * 36)
  private val reply = Message.Assistant(
    Vector(AssistantBlock.ToolCall(ToolCallId("c1"), "read", ujson.Obj("path" -> "a/b.scala"))),
    StopReason.ToolUse,
    Usage.Zero,
    "m"
  )
  private val result = Message.ToolResult(ToolCallId("c1"), "z" * 36, false)
  private val request = ModelRequest("ssss", Vector(asked, reply, result))

  private def est(m: Message): Long = Tokens.value(estimator.message(m))
  private val system = Tokens.value(estimator.system("ssss"))
  private val whole = system + est(asked) + est(reply) + est(result)

  private def usage(input: Long, read: Long, written: Long): Usage =
    Usage(Tokens(input), Tokens.Zero, Tokens(read), None, Tokens(written))

  private def split(u: Usage): Option[InputSplit] =
    InputSplit.of(request, Vector.empty, u, estimator)

  val tests = Tests {
    test("the input's first cacheRead tokens are read, the next cacheWrite written, the rest not") {
      // Read: the system prompt and the ask; written: the reply; uncached: its result.
      split(usage(whole, system + est(asked), est(reply))).map(s =>
        List(s.read, s.written, s.uncached).map(b => b.parts.map(t => Tokens.value(t)))
      ) ==> Some(
        List(
          Vector(system, 0L, est(asked), 0L, 0L, 0L),
          Vector(0L, 0L, 0L, est(reply), 0L, 0L),
          Vector(0L, 0L, 0L, 0L, est(result), 0L)
        )
      )
    }
    test("each count is scaled from the estimate to the provider's, summing to it exactly") {
      split(usage(whole * 2, whole, 0)).map(s =>
        (Tokens.value(s.read.total), Tokens.value(s.written.total), Tokens.value(s.uncached.total))
      ) ==> Some((whole, 0L, whole))
    }
    test(
      "each part costs its read tokens at the read rate, written at the write, the rest uncached"
    ) {
      val rates = Rates.of(BigDecimal(1), BigDecimal(2), BigDecimal("0.5"), BigDecimal(5)).toOption
      val cost = for {
        s <- split(usage(whole, system + est(asked), est(reply)))
        r <- rates
      } yield s.cost(r)
      cost ==> Some(
        Vector(
          BigDecimal(system) * BigDecimal("0.5"),
          BigDecimal(0),
          BigDecimal(est(asked)) * BigDecimal("0.5"),
          BigDecimal(est(reply)) * 2,
          BigDecimal(est(result)),
          BigDecimal(0)
        ).map(_ / 1000000)
      )
    }
    test("the cache's prefix is the tools, then the system prompt, then the messages") {
      val offered = Vector(ToolSchema("read", "Reads a file.", ujson.Obj("type" -> "object")))
      val tooled = request.copy(tools = offered)
      val tools = Tokens.value(estimator.tools(offered))
      // A read shorter than the system prompt and tools together: the tools alone.
      InputSplit
        .of(tooled, Vector.empty, usage(whole + tools, tools, 0), estimator)
        .map(s => s.read.parts.map(t => Tokens.value(t))) ==> Some(
        Vector(0L, tools, 0L, 0L, 0L, 0L)
      )
    }
    test("a request with no input has no split") {
      split(usage(0, 0, 0)) ==> None
    }
  }
}
