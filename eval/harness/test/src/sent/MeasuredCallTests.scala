package grit.eval.harness.sent

import grit.assembly.estimate.CharEstimate
import grit.core.id.{EntryId, ToolCallId}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{ModelRequest, ToolSchema}
import grit.eval.harness.capture.Spent

import utest.*

/** A recorded turn's calls measured against their ledger rows ([[MeasuredCall]]): the cache's
  * misses priced as reads, the prefix re-sent, fresh tokens, and the input by part, costed by a
  * fixed estimator.
  */
object MeasuredCallTests extends TestSuite {

  private val estimator = new CharEstimate(4.0, 4.0, Tokens.Zero)

  private def t(n: Long) = Tokens(n)

  private def usage(input: Long, output: Long, read: Long = 0, write: Long = 0) =
    Usage(t(input), t(output), t(read), None, t(write))

  private def spent(u: Usage) = Spent(None, "m", u, t(0))

  private val asker = Message.User("fix the parser in a.scala")

  private def replying(blocks: AssistantBlock*): Message.Assistant =
    Message.Assistant(blocks.toVector, StopReason.ToolUse, usage(0, 10), "m")

  private val reading = replying(
    AssistantBlock.ToolCall(ToolCallId("t1"), "read", ujson.Obj("path" -> "a.scala"))
  )

  private val result = Message.ToolResult(ToolCallId("t1"), "object A { def b = 1 }", false)

  private val first =
    ModelRequest("sys", Vector(asker), Vector(ToolSchema("read", "Read.", ujson.Obj())))

  private def after(reply: Message.Assistant) =
    first.copy(messages = first.messages ++ Vector(reply, result))

  private val (one, two) = (EntryId("call:0"), EntryId("call:1"))

  private def rows(kept: (EntryId, Usage)*): EntryId -> Option[Spent] = {
    val byEntry = kept.toMap
    e => byEntry.get(e).map(spent)
  }

  val tests = Tests {
    test("a call's misses cost what its uncached and written input costs over cache reads") {
      // Claude Haiku 5.5's short card: input 0.10, writes 0.125, reads 0.01 USD a million.
      val cold = usage(22497, 249, write = 22493)
      (
        MeasuredCall.missed("claude-haiku-5-5", cold),
        MeasuredCall.missed("claude-haiku-5-5", usage(100, 5, read = 100)),
        MeasuredCall.missed("a-model-with-no-rates", cold)
      ) ==> (Some(BigDecimal("0.002587055")), Some(BigDecimal(0)), None)
    }

    test("the prefix re-sent is what the call before left in the cache and this one did not read") {
      (
        MeasuredCall.resent(usage(22497, 249, write = 22493), usage(23317, 238, read = 22493)),
        MeasuredCall.resent(usage(1600, 9, read = 1000, write = 500), usage(1700, 9, read = 200))
      ) ==> (t(0), t(1300))
    }

    test(
      "a call that appends has as fresh tokens its input less the call before's input and output, its parts the call before's with the reply and its fresh tokens added"
    ) {
      val measured = MeasuredCall.of(
        Vector(one -> first, two -> after(reading)),
        rows(one -> usage(100, 10), two -> usage(150, 10)),
        Vector.empty,
        estimator
      )
      val before = Breakdown.of(first, Vector.empty, estimator).scaled(t(100))
      measured.map(_.fresh) ==> Vector(Some(t(100)), Some(t(40)))
      measured.map(_.parts) ==> Vector(
        before,
        before.map(b => b.copy(calls = b.calls + t(10), results = b.results + t(40)))
      )
    }

    test(
      "a reply that withholds its reasoning is re-sent as its estimate scaled to the call's input"
    ) {
      val thinking = replying(
        AssistantBlock.Reasoning("weighing it", None),
        AssistantBlock.ToolCall(ToolCallId("t1"), "read", ujson.Obj("path" -> "a.scala"))
      ).copy(usage = usage(0, 500))
      val second = after(thinking)
      val measured = MeasuredCall.of(
        Vector(one -> first, two -> second),
        rows(one -> usage(100, 500), two -> usage(150, 500)),
        Vector.empty,
        estimator
      )
      // The reply is estimated at 10 of the second request's 40, which the ledger counts as
      // 150: re-sent as 10 * 150 / 40 = 37.5, half up 38, of its 50 more than the first.
      (
        estimator.message(thinking),
        estimator.request(second),
        measured.map(_.fresh)
      ) ==> (t(10), t(40), Vector(Some(t(100)), Some(t(12))))
    }

    test("a call that does not append has no fresh tokens, and its whole request is scaled") {
      val other = after(reading).copy(system = "another prompt")
      val measured = MeasuredCall.of(
        Vector(one -> first, two -> other),
        rows(one -> usage(100, 10), two -> usage(150, 10)),
        Vector.empty,
        estimator
      )
      measured.map(_.fresh) ==> Vector(Some(t(100)), None)
      measured.map(_.parts).lift(1) ==>
        Some(Breakdown.of(other, Vector.empty, estimator).scaled(t(150)))
    }

    test("a call with no ledger row is measured by nothing, and the next does not append to it") {
      val measured = MeasuredCall.of(
        Vector(one -> first, two -> after(reading)),
        rows(two -> usage(150, 10)),
        Vector.empty,
        estimator
      )
      measured.map(m => (m.missed, m.resent, m.fresh, m.parts.isDefined)) ==>
        Vector((None, None, None, false), (None, None, None, true))
    }
  }
}
