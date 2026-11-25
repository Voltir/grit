package grit.eval.harness.sent

import grit.assembly.estimate.CharEstimate
import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{ModelRequest, ToolSchema}

import utest.*

/** Stubbing results over a recorded turn ([[Stubbing]]): which of its own results the rule
  * stubs and why it keeps the others, and which stubbings hindsight regrets.
  */
object StubbingTests extends TestSuite {

  private val estimator = new CharEstimate(4.0, 4.0, Tokens.Zero)

  private val offered: Vector[ToolSchema] = Vector("read", "search", "edit").map(n =>
    ToolSchema(n, s"The $n tool.", ujson.Obj("type" -> "object"))
  )

  private def calling(id: String, tool: String, args: (String, ujson.Value)*): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.ToolCall(ToolCallId(id), tool, ujson.Obj.from(args))),
      StopReason.ToolUse,
      Usage.Zero,
      "m"
    )

  private def said(text: String): Message.Assistant =
    Message.Assistant(Vector(AssistantBlock.Text(text)), StopReason.EndTurn, Usage.Zero, "m")

  private def result(id: String, content: String): Message.ToolResult =
    Message.ToolResult(ToolCallId(id), content, false)

  /** A long result of plain words, which no later output can point at. */
  private val plain: String = "plain words of a long file that nobody names again\n" * 8

  private val rule: Stubbing.Rule = Stubbing.Rule.of(1, Tokens(10)).getOrElse(Stubbing.Rule.Default)

  /** The turn's first five rounds, its window one message: a read stubbed (round 0), an edit
    * (1), a small read (2), a search the next call points at (3), and that read (4).
    */
  private val five: Vector[Message] = Vector(
    Message.User("fix the bug"),
    calling("c0", "read", "path" -> "a"),
    result("c0", plain),
    calling("c1", "edit", "path" -> "b"),
    result("c1", plain),
    calling("c2", "read", "path" -> "c"),
    result("c2", "ok"),
    calling("c3", "search", "pattern" -> "bug"),
    result("c3", "core/src/Found.scala holds the bug\n" + plain),
    calling("c4", "read", "path" -> "core/src/Found.scala"),
    result("c4", "done")
  )

  private def request(messages: Vector[Message]): ModelRequest =
    ModelRequest("s", messages, offered)

  /** [[five]], then a sixth round reading again with `shown`, and `answer`. */
  private def turn(shown: String, answer: Message.Assistant): Stubbing.Recorded = {
    val six = five ++ Vector(calling("c5", "read", "path" -> "a"), result("c5", "again"))
    Stubbing.Recorded(
      Vector(
        Stubbing.Sent(request(five), Tokens(1000), None),
        Stubbing.Sent(request(six), Tokens(1100), None)
      ),
      Some(answer),
      1,
      Set("read", "search"),
      Map(ToolCallId("c0") -> "read a", ToolCallId("c5") -> shown),
      None,
      None
    )
  }

  private def regretted(t: Stubbing.Recorded): Option[(Int, Int)] =
    Stubbing
      .replay(rule, t, estimator)
      .outcomes
      .find(_.trigger == Stubbing.Trigger.Each)
      .map(o => o.stubbed -> o.regretted)

  val tests = Tests {
    test(
      "an old, large, unpointed read-only result is stubbed; the rest kept, each for its reason"
    ) {
      Stubbing
        .judge(rule, request(five), 1, Set("read", "search"), estimator)
        .map(j => (ToolCallId.value(j.result.call), j.round, j.kept)) ==> Vector(
        ("c0", 0, None),
        ("c1", 1, Some(Stubbing.Kept.Writes)),
        ("c2", 2, Some(Stubbing.Kept.Small)),
        ("c3", 3, Some(Stubbing.Kept.Pointed)),
        ("c4", 4, Some(Stubbing.Kept.Recent))
      )
    }
    test("a stubbing is regretted when a later call repeats it, the same tool and shown line") {
      regretted(turn("read a", said("fixed"))) ==> Some(1 -> 1)
    }
    test("a stubbing is regretted when later output points at it") {
      regretted(turn("read again", said(plain.linesIterator.toVector.headOption.getOrElse("")))) ==>
        Some(1 -> 1)
    }
    test("a stubbing no later call repeats nor output points at is not regretted") {
      regretted(turn("read again", said("fixed"))) ==> Some(1 -> 0)
    }
  }
}
