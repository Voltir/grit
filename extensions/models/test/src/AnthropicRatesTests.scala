package grit.models

import grit.core.message.{Tokens, Usage}

import utest.*

/** The expected figures are worked by hand from Anthropic's pricing page (USD per million
  * tokens), not from the rates table.
  */
object AnthropicRatesTests extends TestSuite {

  private def usage(input: Long, read: Long, write: Long, output: Long): Usage =
    Usage(Tokens(input), Tokens(output), Tokens(read), None, Tokens(write))

  val tests = Tests {
    test(
      "Haiku 5.5: its short card to 100,000 prompt tokens, its long card past them, reads and writes counted"
    ) {
      // 2,000 uncached at $0.10, 2,000 written at $0.125, 6,000 read at $0.01, 1,000 out at $0.50.
      AnthropicRates.cost("claude-haiku-5-5", usage(10000, 6000, 2000, 1000)) ==>
        Some(BigDecimal("0.00101"))
      AnthropicRates.cost("claude-haiku-5-5", usage(100000, 0, 0, 0)) ==> Some(BigDecimal("0.01"))
      AnthropicRates.cost("claude-haiku-5-5", usage(100001, 0, 0, 0)) ==>
        Some(BigDecimal("0.0500005"))
      // 10,000 uncached at $0.50, 10,000 written at $0.625, 100,000 read at $0.05, 1,000 out at
      // $2.50: the prompt is long only because of its cached tokens.
      AnthropicRates.cost("claude-haiku-5-5", usage(120000, 100000, 10000, 1000)) ==>
        Some(BigDecimal("0.01875"))
    }

    test(
      "Sonnet 5.5 has one card, a dated snapshot takes its alias's, and an unknown model has none"
    ) {
      // 2,000 uncached at $2, 2,000 written at $2.50, 6,000 read at $0.10, 1,000 out at $10.
      AnthropicRates.cost("claude-sonnet-5-5", usage(10000, 6000, 2000, 1000)) ==>
        Some(BigDecimal("0.0196"))
      AnthropicRates.cost("claude-sonnet-5-5", usage(200000, 0, 0, 0)) ==> Some(BigDecimal("0.4"))
      AnthropicRates.cost("claude-haiku-5-5-20261001", usage(100000, 0, 0, 0)) ==>
        Some(BigDecimal("0.01"))
      AnthropicRates.cost("claude-opus-9", usage(10, 0, 0, 0)) ==> None
    }

    test("Opus 5.5 has one card at any prompt length, its reads at 0.05x its input") {
      // 2,000 uncached at $4, 2,000 written at $5, 6,000 read at $0.20, 1,000 out at $20.
      AnthropicRates.cost("claude-opus-5-5", usage(10000, 6000, 2000, 1000)) ==>
        Some(BigDecimal("0.0392"))
      AnthropicRates.cost("claude-opus-5-5", usage(300000, 0, 0, 0)) ==> Some(BigDecimal("1.2"))
    }
  }
}
