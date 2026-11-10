package grit.models

import grit.core.message.{Cost, Tokens, Usage}

import utest.*

/** The expected figures are worked by hand from Anthropic's pricing page (USD per million
  * tokens), not from the rates table.
  */
object AnthropicRatesTests extends TestSuite {

  private def usage(input: Long, read: Long, write: Long, output: Long): Usage =
    Usage(Tokens(input), Tokens(output), Tokens(read), None, Tokens(write))

  val tests = Tests {
    test(
      "thinking costs its tokens at the output rate of the tier its call takes; none when not reported or unrated"
    ) {
      def thought(input: Long, t: Option[Long]) =
        usage(input, 0, 0, 1000).copy(thinking = t.map(Tokens(_)))
      // 400 thinking at $0.50 on the short tier, at $2.50 on the long one.
      (
        AnthropicRates.pricing("claude-haiku-5-5").flatMap(_.thinking(thought(10000, Some(400)))),
        AnthropicRates.pricing("claude-haiku-5-5").flatMap(_.thinking(thought(120000, Some(400)))),
        AnthropicRates.pricing("claude-haiku-5-5").flatMap(_.thinking(thought(10000, None))),
        AnthropicRates.pricing("gpt-oss").flatMap(_.thinking(thought(10000, Some(400))))
      ) ==> (Some(BigDecimal("0.0002")), Some(BigDecimal("0.001")), None, None)
    }

    test(
      "calls' thinking costs each call at its own model's tier, and at least the priced ones' where a call is unpriced"
    ) {
      def thought(input: Long, t: Option[Long]) =
        usage(input, 0, 0, 1000).copy(thinking = t.map(Tokens(_)))
      // 400 at $0.50 (short) + 400 at $2.50 (long) + 100 at Opus's $20.
      val priced = Vector(
        "claude-haiku-5-5" -> thought(10000, Some(400)),
        "claude-haiku-5-5" -> thought(120000, Some(400)),
        "claude-opus-5-5" -> thought(10000, Some(100))
      )
      (
        AnthropicRates.thinkingCost(priced),
        AnthropicRates.thinkingCost(priced :+ ("gpt-oss" -> thought(10000, Some(400)))),
        AnthropicRates.thinkingCost(Vector.empty)
      ) ==> (
        Cost.Exact(BigDecimal("0.0032")),
        Cost.AtLeast(BigDecimal("0.0032")),
        Cost.Exact(BigDecimal(0))
      )
    }

    test(
      "a total's thinking is exact at the short tier while one tier holds, and at least that past the bound"
    ) {
      def total(input: Long) = usage(input, 0, 0, 1000).copy(thinking = Some(Tokens(400)))
      (
        AnthropicRates.thinkingCostOfTotal("claude-haiku-5-5", total(90000)),
        AnthropicRates.thinkingCostOfTotal("claude-haiku-5-5", total(450000)),
        AnthropicRates.thinkingCostOfTotal("claude-opus-5-5", total(450000)),
        AnthropicRates.thinkingCostOfTotal("gpt-oss", total(90000))
      ) ==> (
        Cost.Exact(BigDecimal("0.0002")),
        Cost.AtLeast(BigDecimal("0.0002")),
        Cost.Exact(BigDecimal("0.008")),
        Cost.AtLeast(BigDecimal(0))
      )
    }

    test(
      "Haiku 5.5: its short tier to 100,000 prompt tokens, its long tier past them, reads and writes counted"
    ) {
      // 2,000 uncached at $0.10, 2,000 written at $0.125, 6,000 read at $0.01, 1,000 out at $0.50.
      AnthropicRates.pricing("claude-haiku-5-5").map(_.cost(usage(10000, 6000, 2000, 1000))) ==>
        Some(BigDecimal("0.00101"))
      AnthropicRates.pricing("claude-haiku-5-5").map(_.cost(usage(100000, 0, 0, 0))) ==> Some(
        BigDecimal("0.01")
      )
      AnthropicRates.pricing("claude-haiku-5-5").map(_.cost(usage(100001, 0, 0, 0))) ==>
        Some(BigDecimal("0.0500005"))
      // 10,000 uncached at $0.50, 10,000 written at $0.625, 100,000 read at $0.05, 1,000 out at
      // $2.50: the prompt is long only because of its cached tokens.
      AnthropicRates.pricing("claude-haiku-5-5").map(_.cost(usage(120000, 100000, 10000, 1000))) ==>
        Some(BigDecimal("0.01875"))
    }

    test(
      "Sonnet 5.5 has one tier, a dated snapshot takes its alias's, and an unknown model has none"
    ) {
      // 2,000 uncached at $2, 2,000 written at $2.50, 6,000 read at $0.10, 1,000 out at $10.
      AnthropicRates.pricing("claude-sonnet-5-5").map(_.cost(usage(10000, 6000, 2000, 1000))) ==>
        Some(BigDecimal("0.0196"))
      AnthropicRates.pricing("claude-sonnet-5-5").map(_.cost(usage(200000, 0, 0, 0))) ==> Some(
        BigDecimal("0.4")
      )
      AnthropicRates.pricing("claude-haiku-5-5-20261001").map(_.cost(usage(100000, 0, 0, 0))) ==>
        Some(BigDecimal("0.01"))
      AnthropicRates.pricing("claude-opus-9").map(_.cost(usage(10, 0, 0, 0))) ==> None
    }

    test("Opus 5.5 has one tier at any prompt length, its reads at 0.05x its input") {
      // 2,000 uncached at $4, 2,000 written at $5, 6,000 read at $0.20, 1,000 out at $20.
      AnthropicRates.pricing("claude-opus-5-5").map(_.cost(usage(10000, 6000, 2000, 1000))) ==>
        Some(BigDecimal("0.0392"))
      AnthropicRates.pricing("claude-opus-5-5").map(_.cost(usage(300000, 0, 0, 0))) ==> Some(
        BigDecimal("1.2")
      )
    }

    test(
      "one tier holds for Haiku 5.5 up to 100,000 prompt tokens in total, for any Opus, and for no unknown model"
    ) {
      AnthropicRates.pricing("claude-haiku-5-5").exists(_.oneTier(Tokens(100000))) ==> true
      AnthropicRates.pricing("claude-haiku-5-5").exists(_.oneTier(Tokens(100001))) ==> false
      AnthropicRates.pricing("claude-opus-5-5").exists(_.oneTier(Tokens(5000000))) ==> true
      AnthropicRates.pricing("claude-unknown-9").exists(_.oneTier(Tokens(1))) ==> false
    }
  }
}
