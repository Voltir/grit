package grit.core.spend

/** The most grit spends on model calls in a day, in US dollars. */
final case class DailyCap private (usd: BigDecimal)

object DailyCap {

  /** `raw` as a cap in dollars, such as `2` or `0.50`; why not, when it is not a number above
    * zero.
    */
  def of(raw: String): Either[String, DailyCap] =
    scala.util
      .Try(BigDecimal(raw.trim))
      .toOption
      .filter(_ > 0)
      .map(DailyCap(_))
      .toRight(s"a daily cap is a number of dollars above zero, not '$raw'")
}
