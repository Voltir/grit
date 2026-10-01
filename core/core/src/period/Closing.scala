package grit.core.period

/** What one period did: `prose` (never blank), its `outcome` when it came to one, and the
  * `changes` its close made to the balance.
  */
final case class Flows private (prose: String, outcome: Option[String], changes: Vector[Change])

object Flows {

  /** Flows with `prose` and `outcome` trimmed and a blank outcome dropped; `None` when `prose`
    * is blank.
    */
  def of(prose: String, outcome: Option[String], changes: Vector[Change]): Option[Flows] =
    Option(prose.trim)
      .filter(_.nonEmpty)
      .map(p => new Flows(p, outcome.map(_.trim).filter(_.nonEmpty), changes))
}

/** What a closed period leaves: its `flows`, and the conversation's `balance` after it. All
  * that a later period or a plugin can know of it once its raw entries are purged.
  */
final case class Closing(flows: Flows, balance: Balance) {

  /** It in one line: its outcome, or else its prose's first sentence. */
  def headline: String =
    flows.outcome.getOrElse(
      Closing.FirstSentence.findFirstIn(flows.prose).getOrElse(flows.prose).trim
    )
}

object Closing {

  /** Up to and including the first `.`, `!` or `?` that ends a sentence: one followed by
    * whitespace or the end.
    */
  private val FirstSentence = """(?s)^.*?[.!?](?=\s|$)""".r
}
