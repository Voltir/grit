package grit.lifecycle.close

import grit.core.message.{AssistantBlock, Message}
import grit.core.period.Closing
import grit.core.provider.ModelRequest

/** What the summary model is asked when a period closes, and how its reply becomes the
  * period's [[Closing]]. It writes from the period's own transcript alone, so the closing
  * reads on its own wherever it is shown.
  */
object ClosingSummary {

  /** The system prompt, before the sections asked for. */
  val System: String =
    "You write the closing record of a stretch of a conversation that has ended. It is all " +
      "that will be kept of it: later conversations will read it instead of the transcript. " +
      "Keep names, numbers, file names and identifiers exactly as written. Reply with " +
      "exactly the labelled parts below and nothing else, in this order."

  /** How much of the transcript, from its end, the model is shown. */
  val TranscriptChars = 40_000

  /** The request that summarises the period whose `transcript` is given, asking for the
    * sections `asked` names beside the prose.
    */
  def request(transcript: String, asked: Asked): ModelRequest = {
    val parts =
      Vector("Summary: two to four sentences: what was asked, and what came of it.") ++
        Option.when(asked.outcome)("Outcome: one line: what the conversation came to.") ++
        list(
          asked.decisions,
          "Decisions",
          "each decision settled: what was chosen, agreed or ruled out"
        ) ++
        list(asked.facts, "Facts", "each fact stated that is worth knowing later") ++
        list(asked.open, "Open", "each question left unanswered or task left unfinished") ++
        list(
          asked.sources,
          "Sources",
          "each file, link or document the facts or decisions came from"
        )
    ModelRequest(
      (System +: parts).mkString("\n") +
        (if (parts.size > 1) "\nWrite none under a list with nothing in it." else ""),
      Vector(Message.User(transcript.takeRight(TranscriptChars)))
    )
  }

  private def list(asked: Boolean, label: String, what: String): Vector[String] =
    if (asked) Vector(s"$label: then one line per item, each starting with \"- \": $what.")
    else Vector.empty

  /** The closing in `reply`, keeping only the sections `asked` names: `Summary:`,
    * `Outcome:`, `Decisions:`, `Facts:`, `Open:` and `Sources:` (any case, markdown emphasis
    * and headings ignored), a label's text running to the next label, a list's items one per
    * line with any bullet or number taken off and "none" dropped. Text before the first
    * label is the prose when `Summary:` is missing; without any label, the whole text is.
    * `None` when it has no prose.
    */
  def read(reply: Message.Assistant, asked: Asked): Option[Closing] = {
    val whole = reply.blocks.collect { case AssistantBlock.Text(t) => t }.mkString.trim
    val labelled = whole.linesIterator.toVector.foldLeft(Vector.empty[(String, String)]) {
      (acc, line) =>
        line match {
          case Label(label, rest) => acc :+ (label.toLowerCase -> rest.trim)
          case other =>
            acc.lastOption match {
              case Some((label, text)) => acc.dropRight(1) :+ (label -> s"$text\n$other")
              case None => acc :+ ("" -> other)
            }
        }
    }
    def part(label: String): Vector[String] =
      labelled
        .collectFirst { case (l, t) if l == label => t.linesIterator.toVector }
        .getOrElse(Vector.empty)
    def text(lines: Vector[String]): String = lines.map(_.trim).filter(_.nonEmpty).mkString(" ")
    def items(label: String, on: Boolean): Vector[String] =
      if (!on) Vector.empty
      else part(label).map(item).filter(i => i.nonEmpty && !isNone(i))
    val prose = Some(text(part("summary"))).filter(_.nonEmpty).getOrElse(text(part("")))
    Closing.of(
      prose,
      Option.when(asked.outcome)(text(part("outcome"))).filterNot(isNone),
      items("decisions", asked.decisions),
      items("facts", asked.facts),
      items("open", asked.open),
      items("sources", asked.sources)
    )
  }

  private val Label =
    """(?i)^[*_#\s]*(summary|outcome|decisions|facts|open|sources)[*_\s]*:[*_\s]*(.*)$""".r

  private val Bullet = """^\s*(?:[-*•]|\d+[.)])\s*(.*)$""".r

  private def item(line: String): String = line match {
    case Bullet(rest) => rest.trim
    case other => other.trim
  }

  private def isNone(s: String): Boolean =
    s.trim.toLowerCase.stripSuffix(".").stripPrefix("(").stripSuffix(")") == "none"
}
