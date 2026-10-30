package grit.core.message

/** What one model call consumed. `input` is every input token, `cacheRead` of them read from
  * the provider's prompt cache and `cacheWrite` written to it (0 where the provider does not
  * say). `costUsd` is the provider's own figure, or grit's from its published rates, when
  * either is known. `cacheWrite` is last, after `costUsd`, only so that the many places that
  * build a `Usage` by position need not name it.
  */
final case class Usage(
    input: Tokens,
    output: Tokens,
    cacheRead: Tokens,
    costUsd: Option[BigDecimal],
    cacheWrite: Tokens = Tokens.Zero
) {

  /** What both consumed together. The cost is unknown when either's is. */
  def +(other: Usage): Usage =
    Usage(
      input + other.input,
      output + other.output,
      cacheRead + other.cacheRead,
      costUsd.zip(other.costUsd).map(_ + _),
      cacheWrite + other.cacheWrite
    )
}

object Usage {

  val Zero: Usage = Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0)))

  def total(usages: Iterable[Usage]): Usage = usages.foldLeft(Zero)(_ + _)
}
