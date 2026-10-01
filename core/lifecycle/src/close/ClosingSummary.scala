package grit.lifecycle.close

import grit.core.message.{AssistantBlock, Message}
import grit.core.period.{Balance, Edit, Ground, Line, Section}
import grit.core.provider.ModelRequest
import grit.lifecycle.transcript.Labelled

/** What the summary model is asked when a period closes, and how its reply becomes the
  * flows of the period's closing and its edits to the balance it opened with. It is shown
  * that balance as already known, then the period's own transcript, and writes only what is
  * new or changed, so a carried line is never rewritten.
  */
object ClosingSummary {

  /** What the writer wrote: `prose`, `outcome`, and its `edits` in the order written. */
  final case class Written(prose: String, outcome: Option[String], edits: Vector[Edit])

  /** The system prompt, before the parts asked for; it names the known lines' labels only
    * when `labels`, since a writer told of labels it is never shown takes them for the parts'
    * own.
    */
  def system(labels: Boolean): String =
    "You keep the books of a conversation. A stretch of it has ended, and you record what " +
      s"it changed. You are shown what is already known${labelledLike(labels)}, and then " +
      "the stretch's transcript. Reply with exactly the labelled parts " +
      "below, in this order, and nothing else. Every line you add must read alone: write " +
      "names, numbers, file paths, commands and identifiers out in full, never \"as " +
      "discussed\", \"that file\" or \"the above\". Add only what this stretch newly " +
      "established. Never repeat a line already known, and never record a recap, a lookup, " +
      "or a list of earlier activity that a tool or the assistant reported. Something not " +
      "known, not found or not recorded is an Open item (what to find out), never a " +
      "Standing fact. The transcript's lines are labelled [u1] for the person's words, [a2] " +
      "for the assistant's and [t3] for a tool's result. End each Standing item with the " +
      "labels of the lines that established it, in brackets, as [u1, t3]: cite what " +
      "established it: the person's line where they stated or decided it (never the one " +
      "where they asked), a tool result that showed it; and cite the assistant's own line " +
      "when nothing else did. Then say who established it: by person (the person stated or " +
      "decided it), by tool (a tool's result showed it), or by assistant (only the " +
      "assistant said it), as \"- The api stays on 3000 [u4] by person\". Lines known elsewhere were shown from the person's other " +
      "conversations: an Open or Standing item that restates one is not new, like a line " +
      "already known."

  /** The system prompt for a period grit only heard, before the parts asked for: people spoke
    * to each other, never to grit, so what it records is reported speech. It names the known
    * lines' labels only when `labels`, as [[system]] does.
    */
  def overheard(labels: Boolean): String =
    "You keep the books of a team's conversation that the assistant only overheard: nobody " +
      "in it spoke to the assistant. It has ended, and you record what was said. You are " +
      s"shown what is already known${labelledLike(labels)}, and then the " +
      "transcript, each line labelled like [h1] and starting with the name of the person who " +
      "said it. Reply with exactly the labelled parts below, in this order, and nothing else. " +
      "Write it as reported speech, naming who said what (\"Emily said the freeze moves to " +
      "Thursday\"): what someone said is what they said, never an established fact. Write " +
      "names, numbers, dates, file paths and identifiers out in full, never \"as discussed\", " +
      "\"that\" or \"the above\". Lines known elsewhere were shown from other conversations: " +
      "never repeat one. Report only what was said; add nothing nobody said."

  private def labelledLike(labels: Boolean): String =
    if (labels) ", each line labelled like [o1] or [s1]" else ""

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

  /** The request for the flows of the period whose `transcript` is given, its last whole
    * lines within [[TranscriptChars]] ([[visible]]), and its edits to `known` ([[labels]]),
    * asking for the parts `asked` names. The lines its windows showed from other
    * conversations (`elsewhere`, [[grit.lifecycle.transcript.PeriodTranscript.elsewhere]])
    * are shown as known elsewhere, before the transcript, at most [[ElsewhereChars]] of
    * them, and unlabelled: they cannot be cited. When `overheard` (grit only heard the
    * period), it is asked under [[overheard]] instead of [[system]], its prose and outcome as
    * reported speech.
    */
  def request(
      transcript: Labelled,
      known: Balance,
      elsewhere: Vector[String],
      asked: Asked,
      overheard: Boolean
  ): ModelRequest = {
    val shown = labels(known)
    val parts =
      Vector(
        if (overheard)
          "Summary: two to four sentences of reported speech: who said what, and what came of it."
        else "Summary: two to four sentences: what was asked, and what came of it."
      ) ++
        Option.when(asked.outcome)(
          if (overheard) "Outcome: one line of reported speech: what the conversation came to."
          else "Outcome: one line: what the conversation came to."
        ) ++
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
            "is not already known, ending with the labels that establish it, then who " +
            "established it: by person, by tool, or by assistant"
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
    val system: String =
      if (overheard) ClosingSummary.overheard(shown.nonEmpty)
      else ClosingSummary.system(shown.nonEmpty)
    val elsewhere_ =
      if (elsewhere.isEmpty) ""
      else
        "Known elsewhere (shown from other places; never record it here):\n" +
          elsewhere.mkString("\n").take(ElsewhereChars) + "\n\n"
    ModelRequest(
      (system +: parts).mkString("\n") +
        (if (parts.size > 1) "\nWrite none under a part with nothing in it." else ""),
      Vector(
        Message.User(
          s"$known_\n\n${elsewhere_}Transcript:\n${visible(transcript).text}"
        )
      )
    )
  }

  /** What of `transcript` the writer is shown, and so what its citations can name: its last
    * whole lines within [[TranscriptChars]].
    */
  def visible(transcript: Labelled): Labelled = transcript.within(TranscriptChars)

  private def list(asked: Boolean, label: String, what: String): Vector[String] =
    if (asked) Vector(s"$label: then one line per item, each starting with \"- \": $what.")
    else Vector.empty

  /** What `reply` wrote, reading only the parts `asked` names (as [[request]] asks for them
    * of `known` and `transcript`): prose, outcome, one `Add` per Open item, one `Stand` per
    * Standing item, its trailing citation of labels and the writer's answer (by person, by
    * tool, by assistant) taken off its text: its ground is the weaker ([[Ground.min]]) of
    * that answer and what the cited lines support in what of `transcript` the writer was
    * shown ([[visible]], [[Labelled.ground]]), so the answer can only lower it, and an item
    * resting only on heard lines is not kept; an item with
    * no answer, no citation, or only labels it lacks, is `Claimed`; and a `Resolve`,
    * `Drop` or `Touch` per Resolved, Dropped or Touched item whose label is one of `known`'s
    * ([[labels]]). An item with no label, or a label `known` does not show, is `Unread`.
    * Parts are labelled `Summary:`, `Outcome:` and so on (any case, markdown emphasis and
    * headings ignored), a label's text running to the next label, a list's items one per
    * line with any bullet or number taken off and "none" dropped. Text before the first
    * label is the prose when `Summary:` is missing; without any label, the whole text is.
    * No line label (`[u1]`, `[a2]`, `[t3]`, `[h4]`, `[o1]`, `[s1]`, or several in one
    * citation) reaches the prose or outcome. When an outcome is asked for and
    * no `Outcome:` part was written, the prose's text after its first `[oN]` naming no known
    * line is the outcome. `None` when it has no prose.
    */
  def read(
      reply: Message.Assistant,
      known: Balance,
      asked: Asked,
      transcript: Labelled
  ): Option[Written] = {
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
    val shown = labels(known)
    val whole_ = Some(text(part("summary"))).filter(_.nonEmpty).getOrElse(text(part("")))
    val outcomePart =
      Option.when(asked.outcome)(text(part("outcome"))).filter(o => o.nonEmpty && !isNone(o))
    // A writer that takes [s1] and [o1] for part labels writes its parts inline: what follows
    // the first [oN] naming no known line is then the outcome, when no Outcome part was.
    val inline = Inline
      .findAllMatchIn(whole_)
      .map(m => (m, m.group(1).toLowerCase))
      .collectFirst { case (m, l) if l.startsWith("o") && !shown.exists(_._1 == l) => m }
      .filter(_ => asked.outcome && outcomePart.isEmpty)
    val prose = unlabelled(inline.fold(whole_)(m => whole_.take(m.start)))
    val outcome = outcomePart
      .orElse(inline.map(m => whole_.drop(m.end)))
      .map(unlabelled)
      .filter(o => o.nonEmpty && !isNone(o))
    Option.when(prose.nonEmpty)(
      Written(
        prose,
        outcome,
        items("open", asked.open).map(Edit.Add(Section.Open, _)) ++
          items("standing", asked.standing).flatMap(stand(_, visible(transcript))) ++
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

  /** A Standing item as its edit: the citation and the writer's answer at its end taken off
    * its text; its ground the weaker ([[Ground.min]]) of the answer and what the cited
    * lines of `transcript`, the lines the writer was shown, support. No answer, or one that
    * does not read, is `Claimed`. `None` for an item resting only on heard lines.
    */
  private def stand(item: String, transcript: Labelled): Option[Edit] = {
    // Peel the end off one piece at a time: a citation (`[u1, t3]`, `[u1][t3]`, `(u1 t3)`)
    // or the answer (`by person`, `(by tool)`, `By: assistant`), any emphasis around them.
    def peel(
        text: String,
        cited: Vector[String],
        answer: Option[Ground]
    ): (String, Vector[String], Option[Ground]) =
      text match {
        case Cited(rest, labels) =>
          peel(rest, labels.split("[,\\s*_]+").toVector.filter(_.nonEmpty) ++ cited, answer)
        case Answered(rest, who) if answer.isEmpty =>
          val ground = who.toLowerCase match {
            case "person" => Ground.Person
            case "tool" => Ground.Tool
            case _ => Ground.Claimed
          }
          peel(rest, cited, Some(ground))
        case _ => (text, cited, answer)
      }
    val (text, cited, answer) = peel(item.trim, Vector.empty, None)
    transcript
      .ground(cited)
      .map(supported =>
        Edit.Stand(text.trim, Ground.min(answer.getOrElse(Ground.Claimed), supported))
      )
  }

  /** An item's text, then one citation at its very end: a bracket or parenthesis holding
    * only labels (a letter and a number each), with any emphasis marks around or inside it.
    */
  private val Cited =
    """^(.*?)[\s*_]*[\[(][\s*_]*((?:[a-zA-Z]\d+[\s*_,]*)+)[\])][\s*_]*$""".r

  /** An item's text, then the writer's answer at its very end: "by" and one of person, tool
    * or assistant, any case, with an optional colon, parentheses and emphasis.
    */
  private val Answered =
    """(?i)^(.*?)[\s*_,;:—–-]*\(?[\s*_]*by[\s*_]*:?[\s*_]*(person|tool|assistant)[\s*_]*\)?[\s*_.]*$""".r

  /** A known-line label written inline, `[o1]` or `[s1]`; its label is group 1. */
  private val Inline = """(?i)\[([os]\d+)\]""".r

  /** Any label grit writes on a line, a transcript's (`u`, `a`, `t`, `h`) or a known line's
    * (`o`, `s`), alone or several in one citation (`[u4, t3]`).
    */
  private val AnyLabel = """(?i)\[\s*[uathos]\d+(?:\s*,\s*[uathos]\d+)*\s*\]""".r

  /** `s` with every line label taken out, spaces closed up. */
  private def unlabelled(s: String): String =
    AnyLabel.replaceAllIn(s, "").replaceAll("\\s+", " ").trim

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
