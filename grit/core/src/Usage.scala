package grit.core

/** What one model call consumed. `cachedInput` is the part of `input` served
  * from the provider's prompt cache. `costUsd` is the provider's own figure,
  * when it reports one.
  */
final case class Usage(
    input: Tokens,
    output: Tokens,
    cachedInput: Tokens,
    costUsd: Option[BigDecimal]
)
