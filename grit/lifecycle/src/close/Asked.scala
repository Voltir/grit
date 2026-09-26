package grit.lifecycle.close

/** Which of a closing's sections the summary model is asked to write; the prose is always
  * asked for.
  */
final case class Asked(
    outcome: Boolean,
    decisions: Boolean,
    facts: Boolean,
    open: Boolean,
    sources: Boolean
)

object Asked {

  /** Every section: what a close asks for when it cannot tell which the period needs. */
  val Every: Asked = Asked(true, true, true, true, true)
}
