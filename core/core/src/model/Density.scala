package grit.core.model

/** Characters of text per token a pair's tokenizer averages. */
opaque type Density = Double

object Density {

  /** `perToken` characters a token; `Left` when it is not a finite number above 0. */
  def of(perToken: Double): Either[String, Density] =
    if (perToken > 0 && !perToken.isInfinite) Right(perToken)
    else Left(s"a density is a finite number of characters a token above 0, not $perToken")

  extension (d: Density) def perToken: Double = d

  given CanEqual[Density, Density] = CanEqual.derived
}
