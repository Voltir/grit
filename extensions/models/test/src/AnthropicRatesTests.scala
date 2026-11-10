package grit.models

import java.time.LocalDate

import grit.core.message.{Cost, Tokens, Usage}
import grit.core.model.{Known, ModelId, ModelRef, Pricing, Profile, Rates, Source, Upstream}

import utest.*

/** Which seed profile prices a model, and what calls' thinking costs together. The expected
  * figures are worked by hand from Anthropic's pricing page (USD per million tokens), not from
  * the seed; how one tier bills a call is `PricingTests`'.
  */
object AnthropicRatesTests extends TestSuite {

  private def usage(input: Long, read: Long, write: Long, output: Long): Usage =
    Usage(Tokens(input), Tokens(output), Tokens(read), None, Tokens(write))

  private val seed = Seed.catalog.getOrElse(sys.error("the seed"))

  /** The seed's profile of `model` at Anthropic's API: its pricing, if it has one. */
  private def direct(model: String): Option[Pricing] =
    ModelId
      .of(model)
      .flatMap(id => seed.profile(ModelRef(id, Some(Upstream.AnthropicApi))))
      .flatMap(_.pricing.toOption)

  val tests = Tests {
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
      "a model is priced as the seed's profile of it at Anthropic's API, a dated snapshot as its alias, and an unknown model not at all"
    ) {
      (
        AnthropicRates.pricing("claude-sonnet-5-5"),
        AnthropicRates.pricing("claude-haiku-5-5-20261001"),
        AnthropicRates.pricing("claude-opus-9")
      ) ==> (direct("anthropic/claude-sonnet-5-5"), direct("anthropic/claude-haiku-5-5"), None)
      assert(direct("anthropic/claude-haiku-5-5").isDefined)
    }

    test(
      "only a profile at Anthropic's API of one of Anthropic's models prices: a routed one, or another vendor's there, does not"
    ) {
      val rates = Rates
        .of(BigDecimal(1), BigDecimal(1), BigDecimal(1), BigDecimal(1))
        .getOrElse(sys.error("rates"))
      val said = Source.Declared("test", LocalDate.of(2026, 11, 10))
      def priced(model: String, upstream: Option[Upstream]) = Profile(
        ModelRef(ModelId.of(model).getOrElse(sys.error(model)), upstream),
        pricing = Known.Of(Pricing.flat(rates), said)
      )
      val catalog = seed.overlaid(
        Vector(
          priced("anthropic/claude-haiku-5.5", None),
          priced("anthropic/claude-fable-1", Upstream.of("anthropic")),
          priced("openai/gpt-9", Some(Upstream.AnthropicApi))
        )
      )
      AnthropicRates.pricedIn(catalog).map(_._1).toSet ==>
        Set("claude-haiku-5-5", "claude-sonnet-5-5", "claude-opus-5-5")
    }
  }
}
