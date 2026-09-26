package grit.core.period

import java.time.{Instant, ZoneOffset}

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

  /** The one message the model is shown for it: that it comes from earlier in the
    * conversation, closed `at` (its UTC date) for `reason`; its prose and outcome; the lines
    * it resolved, and how; then the balance's open, standing and topic lines. Nothing
    * dropped, evicted, refused or ignored.
    */
  def shown(at: Instant, reason: CloseReason): String = {
    val why = reason match {
      case CloseReason.Resolved(_) => "resolved"
      case CloseReason.Lapsed => "lapsed"
    }
    val day = at.atOffset(ZoneOffset.UTC).toLocalDate
    val settled = flows.changes.collect { case Change.Resolved(l, how) =>
      if (how.trim.isEmpty) l.text else s"${l.text} — ${how.trim}"
    }
    def list(title: String, lines: Vector[String]): Vector[String] =
      Option.when(lines.nonEmpty)(lines.map(l => s"- $l").mkString(s"$title:\n", "\n", "")).toVector
    val topics = balance.in(Section.Topics).map(_.text)
    (s"Earlier in this conversation (closed $day, $why): ${flows.prose}" +:
      (flows.outcome.map(o => s"Outcome: $o").toVector ++
        list("Settled then", settled) ++
        list("Still open", balance.in(Section.Open).map(_.text)) ++
        list("Standing", balance.in(Section.Standing).map(_.text)) ++
        Option.when(topics.nonEmpty)(s"Topics so far: ${topics.mkString("; ")}").toVector))
      .mkString("\n")
  }
}

object Closing {

  /** Up to and including the first `.`, `!` or `?` that ends a sentence: one followed by
    * whitespace or the end.
    */
  private val FirstSentence = """(?s)^.*?[.!?](?=\s|$)""".r
}
