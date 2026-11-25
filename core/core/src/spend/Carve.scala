package grit.core.spend

/** The most an acting carved from another's may spend on model calls, in US dollars: a
  * subagent's, set by its type.
  */
final case class Carve private (usd: BigDecimal) extends caps.Pure

object Carve {

  /** `usd` as a carve, or why not: not above zero. */
  def of(usd: BigDecimal): Either[String, Carve] =
    Either.cond(usd > 0, Carve(usd), s"a carve is a number of dollars above zero, not $usd")
}
