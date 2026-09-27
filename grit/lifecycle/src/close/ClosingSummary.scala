package grit.lifecycle.close

import grit.core.message.{AssistantBlock, Message}
import grit.core.period.{Balance, Edit, Ground, Line, Section}
import grit.core.provider.ModelRequest

/** What the summary model is asked when a period closes, and how its reply becomes the
  * flows of the period's closing and its edits to the balance it opened with. It is shown
  * that balance as already known, then the period's own transcript, and writes only what is
  * new or changed, so a carried line is never rewritten.
  */
object ClosingSummary {

  /** What the writer wrote: `prose`, `outcome`, and its `edits` in the order written. */
  final case class Written(prose: String, outcome: Option[String], edits: Vector[Edit])

  /** The system prompt, before the parts asked for. */
  val System: String =
    "You keep the books of a conversation. A stretch of it has ended, and you record what " +
      "it changed. You are shown what is already known, each line labelled like [o1] or " +
      "[s1], and then the stretch's transcript. Reply with exactly the labelled parts " +
      "below, in this order, and nothing else. Every line you add must read alone: write " +
      "names, numbers, file paths, commands and identifiers out in full, never \"as " +
      "discussed\", \"that file\" or \"the above\". Add only what this stretch newly " +
      "established. Never repeat a line already known, and never record a recap, a lookup, " +
      "or a list of earlier activity that a tool or the assistant reported. Something not " +
      "known, not found or not recorded is an Open item (what to find out), never a " +
      "Standing fact. Lines known elsewhere were shown from the person's other " +
      "conversations: an Open or Standing item that restates one is not new, like a line " +
      "already known."

  /** How much of the transcript, from its end, the model is shown. */
  val TranscriptChars = 40_000

  /** How much of what the period was shown from elsewhere, from its start, the model is
    * shown.
    */
  val ElsewhereChars = 8_000

  /** `known`'s open and standing lines by the labels the writer sees them under: `o1`, `o2`,
    * … for open, `s1`, … for standing, in the balance's order.
    */
  def labels(known: Balance): Vector[(String, Line)] =
    known.in(Section.Open).zipWithIndex.map((l, i) => s"o${i + 1}" -> l) ++
      known.in(Section.Standing).zipWithIndex.map((l, i) => s"s${i + 1}" -> l)

  /** The request for the flows of the period whose `transcript` is given, and its edits to
    * `known` ([[labels]]), asking for the parts `asked` names. The lines its windows showed
    * from other conversations (`elsewhere`,
    * [[grit.lifecycle.transcript.PeriodTranscript.elsewhere]]) are shown as known elsewhere,
    * before the transcript, at most [[ElsewhereChars]] of them.
    */
  def request(
      transcript: String,
      known: Balance,
      elsewhere: Vector[String],
      asked: Asked
  ): ModelRequest = {
    val shown = labels(known)
    val parts =
      Vector("Summary: two to four sentences: what was asked, and what came of it.") ++
        Option.when(asked.outcome)("Outcome: one line: what the conversation came to.") ++
        list(
          asked.open,
          "Open",
          "each question left unanswered, task left unfinished or thing not known, that is " +
            "not already known"
        ) ++
        list(
          asked.standing,
          "Standing",
          "each decision settled or fact established that later work should rely on, that " +
            "is not already known"
        ) ++
        list(
          asked.settled && shown.nonEmpty,
          "Resolved",
          "each known line this stretch answered, finished or overturned, as its label, a " +
            "colon and how (\"- o1: done on Monday\")"
        ) ++
        list(
          asked.settled && shown.nonEmpty,
          "Dropped",
          "each known line that no longer holds and was not resolved, as its label, a colon " +
            "and why"
        ) ++
        list(
          shown.nonEmpty,
          "Touched",
          "each known line this stretch relied on or confirmed, as its label (\"- s3\")"
        )
    val known_ =
      if (shown.isEmpty) "Already known: nothing."
      else {
        def section(title: String, prefix: String) =
          shown.collect {
            case (label, l) if label.startsWith(prefix) => s"[$label] ${l.text}"
          } match {
            case Vector() => Vector.empty
            case lines => (s"$title:" +: lines)
          }
        ("Already known:" +: (section("Open", "o") ++ section("Standing", "s"))).mkString("\n")
      }
    val elsewhere_ =
      if (elsewhere.isEmpty) ""
      else
        "Known elsewhere (shown from other places; never record it here):\n" +
          elsewhere.mkString("\n").take(ElsewhereChars) + "\n\n"
    ModelRequest(
      (System +: parts).mkString("\n") +
        (if (parts.size > 1) "\nWrite none under a part with nothing in it." else ""),
      Vector(
        Message.User(
          s"$known_\n\n${elsewhere_}Transcript:\n${transcript.takeRight(TranscriptChars)}"
        )
      )
    )
  }

  private def list(asked: Boolean, label: String, what: String): Vector[String] =
    if (asked) Vector(s"$label: then one line per item, each starting with \"- \": $what.")
    else Vector.empty

  /** What `reply` wrote, reading only the parts `asked` names (as [[request]] asks for them
    * of `known`): prose, outcome, one `Add` per Open or Standing item, and a `Resolve`,
    * `Drop` or `Touch` per Resolved, Dropped or Touched item whose label is one of `known`'s
    * ([[labels]]). An item with no label, or a label `known` does not show, is `Unread`.
    * Parts are labelled `Summary:`, `Outcome:` and so on (any case, markdown emphasis and
    * headings ignored), a label's text running to the next label, a list's items one per
    * line with any bullet or number taken off and "none" dropped. Text before the first
    * label is the prose when `Summary:` is missing; without any label, the whole text is.
    * `None` when it has no prose.
    */
  def read(reply: Message.Assistant, known: Balance, asked: Asked): Option[Written] = {
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
    val shown = labels(known)
    Option.when(prose.nonEmpty)(
      Written(
        prose,
        Option.when(asked.outcome)(text(part("outcome"))).filter(o => o.nonEmpty && !isNone(o)),
        items("open", asked.open).map(Edit.Add(Section.Open, _)) ++
          items("standing", asked.standing).map(Edit.Stand(_, Ground.Claimed)) ++
          items("resolved", asked.settled && shown.nonEmpty).map(
            named(shown, _)((l, how) => Edit.Resolve(l.id, how))
          ) ++
          items("dropped", asked.settled && shown.nonEmpty).map(
            named(shown, _)((l, why) => Edit.Drop(l.id, why))
          ) ++
          items("touched", shown.nonEmpty).map(named(shown, _)((l, _) => Edit.Touch(l.id)))
      )
    )
  }

  /** The item `written` as the edit `make` makes of the line its label names and the text
    * after the label; `Unread` when it names none of `shown`.
    */
  private def named(shown: Vector[(String, Line)], written: String)(
      make: (Line, String) -> Edit
  ): Edit =
    written match {
      case Named(label, rest) =>
        shown
          .collectFirst { case (l, line) if l == label.toLowerCase => make(line, rest.trim) }
          .getOrElse(Edit.Unread(written, "names no line"))
      case _ => Edit.Unread(written, "names no line")
    }

  /** A label, bracketed or not, then an optional colon or dash, then the rest. */
  private val Named = """(?i)^\[?([os]\d+)\]?\s*(?:[:\-–—]\s*|$)(.*)$""".r

  private val Label =
    """(?i)^[*_#\s]*(summary|outcome|open|standing|resolved|dropped|touched|decisions|facts|sources)[*_\s]*:[*_\s]*(.*)$""".r

  private val Bullet = """^\s*(?:[-*•]|\d+[.)])\s*(.*)$""".r

  private def item(line: String): String = line match {
    case Bullet(rest) => rest.trim
    case other => other.trim
  }

  private def isNone(s: String): Boolean =
    s.trim.toLowerCase.stripSuffix(".").stripPrefix("(").stripSuffix(")") == "none"
}
