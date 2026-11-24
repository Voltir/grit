package grit.core.provider

import grit.core.message.{Cost, Tokens, Usage}
import grit.core.model.{ModelId, ModelRef, Upstream}

import utest.*

object CallLineTests extends TestSuite {

  private val ref = ModelRef(
    ModelId.of("anthropic/claude-haiku-5-5").getOrElse(sys.error("model")),
    Upstream.of("api.anthropic.com")
  )

  val tests = Tests {
    test(
      "a call's line names its pair, its input with cache reads and writes, its output and its cost in cents"
    ) {
      CallLine.answered(
        ref,
        Usage(Tokens(1200), Tokens(40), Tokens(1000), Some(BigDecimal("0.0002")), Tokens(150))
      ) ==> "[provider] anthropic/claude-haiku-5-5 @ api.anthropic.com answered · in 1200 · " +
        "read 1000 · write 150 · out 40 · 0.02¢"
    }

    test(
      "an answered line reads back as its model, its tokens unpriced and the cost it printed apart; any other line as none"
    ) {
      val usage =
        Usage(Tokens(1200), Tokens(40), Tokens(1000), Some(BigDecimal("0.0002")), Tokens(150))
      (
        CallLine.read(CallLine.answered(ref, usage)),
        CallLine.read(CallLine.answered(ref.copy(upstream = None), usage.copy(costUsd = None))),
        CallLine.read("[provider] anthropic/claude-haiku-5-5 called with 3 message(s)"),
        CallLine.read("[main] w -> done: hi")
      ) ==> (
        Some(
          ("anthropic/claude-haiku-5-5", usage.copy(costUsd = None), Some(BigDecimal("0.0002")))
        ),
        Some(("anthropic/claude-haiku-5-5", usage.copy(costUsd = None), None)),
        None,
        None
      )
    }

    test(
      "the cost a line printed reads back within its rounding, and prints again as it was printed"
    ) {
      val usage = Usage(Tokens(10), Tokens(1), Tokens(0), None, Tokens(0))
      // Cost.cents' own examples, and amounts either side of its switch at 10¢.
      val amounts =
        Vector("0.0029", "0.00003", "0.0124", "0.1256", "1.256", "0", "0.0995", "0.1049")
          .map(BigDecimal(_))
      amounts.map { usd =>
        val back =
          CallLine.read(CallLine.answered(ref, usage.copy(costUsd = Some(usd)))).flatMap(_.rounded)
        (
          usd,
          back.map(b => Cost.cents(b) == Cost.cents(usd)),
          back.map(b => b == 0 && usd == 0 || ((b - usd).abs / usd.max(b)) <= BigDecimal("0.05"))
        )
      } ==> amounts.map(usd => (usd, Some(true), Some(true)))
    }

    test(
      "a call's line names the thinking of its output when reported, and reads it back; a line without it reads as not reported"
    ) {
      val usage = Usage(Tokens(1200), Tokens(40), Tokens(1000), None, Tokens(150), Some(Tokens(31)))
      val line = CallLine.answered(ref, usage)
      (
        line,
        CallLine.read(line),
        CallLine.read(CallLine.answered(ref, usage.copy(thinking = None)))
      ) ==> (
        "[provider] anthropic/claude-haiku-5-5 @ api.anthropic.com answered · in 1200 · " +
          "read 1000 · write 150 · out 40 (thinking 31) · cost unknown",
        Some(("anthropic/claude-haiku-5-5", usage, None)),
        Some(("anthropic/claude-haiku-5-5", usage.copy(thinking = None), None))
      )
    }

    test("a call's sending line names its pair and how many messages it is sent") {
      CallLine.called(ref, 3) ==>
        "[provider] anthropic/claude-haiku-5-5 @ api.anthropic.com called with 3 message(s)"
    }

    test("a call its provider did not price says its cost is unknown") {
      CallLine.spent(Usage(Tokens(10), Tokens(2), Tokens(0), None)) ==>
        "in 10 · read 0 · write 0 · out 2 · cost unknown"
    }
  }
}
