package grit.models

import grit.core.message.{Tokens, Usage}

/** What Anthropic bills for a call, from the rates its pricing page published on 2026-10-29
  * (USD per million tokens). Pure.
  */
object AnthropicRates {

  /** What Anthropic bills for `usage` on `model`, in USD, cache writes at the 5-minute rate;
    * `None` for a model it has no rates for. A dated snapshot (`claude-haiku-5-5-20261001`)
    * takes its alias's rates. A Claude Haiku 5.5 prompt over 100,000 tokens, its cache reads
    * and writes counted, takes the long-context card.
    */
  def cost(model: String, usage: Usage): Option[BigDecimal] =
    card(model, Tokens.value(usage.input)).map { c =>
      val read = Tokens.value(usage.cacheRead)
      val write = Tokens.value(usage.cacheWrite)
      val uncached = Tokens.value(usage.input) - read - write
      (c.input * uncached + c.write * write + c.read * read +
        c.output * Tokens.value(usage.output)) / BigDecimal(1000000)
    }

  /** One price list, USD per million tokens. */
  private final case class Card(
      input: BigDecimal,
      write: BigDecimal,
      read: BigDecimal,
      output: BigDecimal
  )

  private val HaikuShort =
    Card(BigDecimal("0.10"), BigDecimal("0.125"), BigDecimal("0.01"), BigDecimal("0.50"))
  private val HaikuLong =
    Card(BigDecimal("0.50"), BigDecimal("0.625"), BigDecimal("0.05"), BigDecimal("2.50"))
  private val Sonnet =
    Card(BigDecimal("2"), BigDecimal("2.50"), BigDecimal("0.10"), BigDecimal("10"))
  private val Opus =
    Card(BigDecimal("4"), BigDecimal("5"), BigDecimal("0.20"), BigDecimal("20"))

  /** Where Claude Haiku 5.5's long-context card begins: a prompt longer than this. */
  private val HaikuLongPast = 100000L

  private def card(model: String, prompt: Long): Option[Card] = {
    def is(id: String) = AnthropicJson.isOrSnapshotOf(model, id)
    if (is("claude-haiku-5-5")) Some(if (prompt > HaikuLongPast) HaikuLong else HaikuShort)
    else if (is("claude-sonnet-5-5")) Some(Sonnet)
    else if (is("claude-opus-5-5")) Some(Opus)
    else None
  }
}
