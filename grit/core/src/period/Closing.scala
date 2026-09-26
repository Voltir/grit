package grit.core.period

import java.time.{Instant, ZoneOffset}

/** What a closed period leaves behind once its raw entries are purged: all that a later
  * period or a plugin can know of it.
  */
final case class Closing private (
    prose: String,
    outcome: Option[String],
    decisions: Vector[String],
    facts: Vector[String],
    open: Vector[String],
    sources: Vector[String]
) {

  /** It in one line: its outcome, or else its prose's first sentence. */
  def headline: String =
    outcome.getOrElse(Closing.FirstSentence.findFirstIn(prose).getOrElse(prose).trim)

  /** The one message the model is shown for it: that it comes from earlier in the
    * conversation, closed `at` (its UTC date) for `reason`, then the prose and each section
    * with lines.
    */
  def shown(at: Instant, reason: CloseReason): String = {
    val why = reason match {
      case CloseReason.Resolved => "resolved"
      case CloseReason.Lapsed => "lapsed"
    }
    val day = at.atOffset(ZoneOffset.UTC).toLocalDate
    val sections =
      Vector("Decisions" -> decisions, "Facts" -> facts, "Open" -> open, "Sources" -> sources)
        .filter(_._2.nonEmpty)
        .map((title: String, lines: Vector[String]) =>
          lines.map(l => s"- $l").mkString(s"$title:\n", "\n", "")
        )
    (s"Earlier in this conversation (closed $day, $why): $prose" +:
      (outcome.map(o => s"Outcome: $o").toVector ++ sections)).mkString("\n")
  }
}

object Closing {

  /** Up to and including the first `.`, `!` or `?` that ends a sentence: one followed by
    * whitespace or the end.
    */
  private val FirstSentence = """(?s)^.*?[.!?](?=\s|$)""".r

  /** A closing with every line trimmed, blank lines dropped from its sections and a blank
    * outcome dropped; `None` when `prose` is blank.
    */
  def of(
      prose: String,
      outcome: Option[String],
      decisions: Vector[String],
      facts: Vector[String],
      open: Vector[String],
      sources: Vector[String]
  ): Option[Closing] = {
    def lines(ls: Vector[String]) = ls.map(_.trim).filter(_.nonEmpty)
    Option(prose.trim)
      .filter(_.nonEmpty)
      .map(p =>
        new Closing(
          p,
          outcome.map(_.trim).filter(_.nonEmpty),
          lines(decisions),
          lines(facts),
          lines(open),
          lines(sources)
        )
      )
  }
}
