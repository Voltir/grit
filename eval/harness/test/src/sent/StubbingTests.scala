package grit.eval.harness.sent

import grit.assembly.estimate.CharEstimate
import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Pricing, Rates}
import grit.core.provider.{ModelRequest, ToolSchema}

import utest.*

/** Stubbing results over a recorded turn ([[Stubbing]]): which of its own results the rule
  * stubs and why it keeps the others, when each trigger stubs, what a stubbing rewrites, and
  * which stubbings hindsight regrets.
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

  /** A line of [[plain]], long enough to point at its results when repeated whole. */
  private val plainLine: String = plain.linesIterator.toVector.headOption.getOrElse("")

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

  /** [[five]] with the read stubbed six times as long, most of each request. */
  private val heavy: Vector[Message] = five.updated(2, result("c0", plain * 6))

  private def request(messages: Vector[Message]): ModelRequest =
    ModelRequest("s", messages, offered)

  private def sent(messages: Vector[Message], input: Long, cacheRead: Long): Stubbing.Sent =
    Stubbing.Sent(request(messages), Tokens(input), Tokens(cacheRead), None)

  private def recorded(
      calls: Vector[Stubbing.Sent],
      answer: Message.Assistant,
      shown: Map[ToolCallId, String] = Map(ToolCallId("c0") -> "read a"),
      limit: Option[Tokens] = None,
      pricing: Option[Pricing] = None
  ): Stubbing.Recorded =
    Stubbing.Recorded(calls, Some(answer), 1, Set("read", "search"), shown, limit, pricing)

  /** `start`, then a sixth round reading again with `shown`, and `answer`; each call read
    * `cached` of its input from the cache.
    */
  private def turn(
      shown: String,
      answer: Message.Assistant,
      start: Vector[Message] = five,
      input: Long = 1000,
      cached: Long = 1000
  ): Stubbing.Recorded = {
    val six = start ++ Vector(calling("c5", "read", "path" -> "a"), result("c5", "again"))
    recorded(
      Vector(sent(start, input, math.min(cached, input)), sent(six, input + 100, cached)),
      answer,
      Map(ToolCallId("c0") -> "read a", ToolCallId("c5") -> shown)
    )
  }

  private def outcome(t: Stubbing.Recorded, trigger: Stubbing.Trigger): Option[Stubbing.Outcome] =
    Stubbing.replay(rule, t, estimator).outcomes.find(_.trigger == trigger)

  /** At every call: results stubbed, of them repeated, and of them pointed at unrepeated. */
  private def regret(t: Stubbing.Recorded): Option[(Int, Int, Int)] =
    outcome(t, Stubbing.Trigger.Each).map(o => (o.stubbed, o.repeated, o.pointed))

  private def rates(input: String, write: String, read: String): Option[Pricing] =
    Rates.of(BigDecimal(input), BigDecimal(write), BigDecimal(read), 5).toOption.map(Pricing.flat)

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
    test("a stubbing a later call repeats, the same tool and shown line, counts as repeated") {
      regret(turn("read a", said("fixed"))) ==> Some((1, 1, 0))
    }
    test("a stubbing later output points at, no repeat made before, counts as pointed at") {
      regret(turn("read again", said(plainLine))) ==> Some((1, 0, 1))
    }
    test("a stubbing pointed at only after a call repeated it is not pointed at unrepeated") {
      regret(turn("read a", said(plainLine))) ==> Some((1, 1, 0))
    }
    test("a stubbing no later call repeats nor output points at is neither") {
      regret(turn("read again", said("fixed"))) ==> Some((1, 0, 0))
    }
    test("a stubbing rewrites the part of its tail its call read from the cache, no more") {
      // The tail is 699: read whole, the stubbing rewrites it; 900 of 1000 read, the newest
      // 100 were sent uncached anyway; nothing read, nothing more is sent uncached.
      Vector(1000L, 900L, 0L).map(cached =>
        outcome(turn("read again", said("fixed"), cached = cached), Stubbing.Trigger.Each)
          .map(o => Tokens.value(o.rewrote))
      ) ==> Vector(Some(699L), Some(599L), Some(0L))
    }
    test("a second stubbing rewrites from its own first stub on, not the first stubbing's") {
      // Round 5 reads a long file the call after round 6 stubs, c0 stubbed two calls before.
      // The second tail is c5's stub, c6's call and its result, scaled to the call's input;
      // c0's stub lies before it and shortens it by nothing.
      val six = five ++ Vector(calling("c5", "read", "path" -> "d"), result("c5", plain))
      val seven = six ++ Vector(calling("c6", "read", "path" -> "e"), result("c6", "x"))
      val t = recorded(
        Vector(sent(five, 1000, 1000), sent(six, 1100, 1100), sent(seven, 1200, 1200)),
        said("fixed")
      )
      outcome(t, Stubbing.Trigger.Each).map(o => (o.stubbings, Tokens.value(o.rewrote))) ==>
        Some((2, 699L + 87L))
    }
    test("the pressure trigger stubs at a call whose input meets the limit, freeing a quarter") {
      // The first call's input, 1000, is under the limit; the second's meets it.
      val t = turn("read again", said("fixed"), start = heavy).copy(limit = Some(Tokens(1100)))
      outcome(t, Stubbing.Trigger.Pressure).map(o => (o.stubbings, o.stubbed)) ==> Some((1, 1))
    }
    test("the priced trigger stubs where D, 20k or more, over T pays back within four calls") {
      // At 0.15 uncached and 0.03 read, D must reach T.
      val t = turn("read again", said("fixed"), start = heavy, input = 200000, cached = 200000)
        .copy(pricing = rates("0.15", "0.15", "0.03"))
      outcome(t, Stubbing.Trigger.Priced).map(o => (o.stubbings, o.stubbed)) ==> Some((1, 1))
    }
    test("the report says, stubbing at every call, what it dropped and both regrets") {
      // A million tokens: 1 USD uncached, 1.25 written, 0.1 read. Of the tail of 699, the 599
      // read from the cache are rewritten, at 1.15; the 380 stubbed from both calls save 0.1:
      // 0.2¢ becomes 0.265¢.
      val base = turn("read a", said("fixed"), cached = 900)
      val priced = base.copy(
        calls = base.calls.map(_.copy(cost = Some(BigDecimal("0.001")))),
        pricing = rates("1", "1.25", "0.1")
      )
      Stubbing.lines(Stubbing.replay(rule, priced, estimator)).find(_.startsWith("  at every")) ==>
        Some(
          "  at every call: 1 stubbing rewriting 599, 1 result, 80 of 347 result tokens dropped " +
            "(23.1%); 1 repeated (100.0%), 0 pointed at unrepeated (0.0%); input 2100 recorded, " +
            "1720 simulated; cost 0.2¢ recorded, 0.27¢ simulated"
        )
    }
  }
}
