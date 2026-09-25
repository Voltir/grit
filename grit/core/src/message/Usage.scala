package grit.core.message

/** What one model call consumed. `cachedInput` is the part of `input` served
  * from the provider's prompt cache. `costUsd` is the provider's own figure,
  * when it reports one.
  */
final case class Usage(
    input: Tokens,
    output: Tokens,
    cachedInput: Tokens,
    costUsd: Option[BigDecimal]
) {

  /** What both consumed together. The cost is unknown when either's is. */
  def +(other: Usage): Usage =
    Usage(
      input + other.input,
      output + other.output,
      cachedInput + other.cachedInput,
      costUsd.zip(other.costUsd).map(_ + _)
    )
}

object Usage {

  val Zero: Usage = Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0)))

  def total(usages: Iterable[Usage]): Usage = usages.foldLeft(Zero)(_ + _)
}
