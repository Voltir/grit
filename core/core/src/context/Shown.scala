package grit.core.context

import java.time.{Instant, ZoneOffset}

import grit.core.document.{DocLabel, DocText, Document}
import grit.core.id.TurnSeq
import grit.core.message.{AssistantBlock, Message}
import grit.core.period.{Change, Closing, Ground, Section}
import grit.core.place.Place
import grit.core.store.{ClosingEntry, Entry, Nearby, Payload, Speakers}

/** What the model is shown of an entry a window names: the one definition, which an
  * assembler costs and the turn sends.
  */
object Shown {

  /** A message as it is, but a person's, when `speakers` names its author, under a line of
    * its own, "{name} wrote:", and with a pasted grit block, that line included, shown as one
    * ([[pasted]]); a heard message as a person's under "{name} said, not to you:"
    * ("Someone" when `speakers` does not name its author); grit's post the conversation
    * begins with as one user message, [[PostedLead]] then the post as [[pasted]] shows it, so
    * no window opens with an assistant message;
    * a closing entry as one user message, "[record] this conversation
    * so far, written by grit (closed {its UTC date}):", then its prose, its outcome, the
    * lines it resolved and how, and the balance's open lines, its standing lines (those
    * only the assistant said listed apart, as not confirmed) and its topics, each text the
    * period carried as [[pasted]] shows it; `None` for any other entry.
    */
  def of(entry: Entry, speakers: Speakers): Option[Message] = entry.payload match {
    case Payload.Message(Message.User(text)) => Some(said(entry, text, speakers))
    case Payload.Message(m) => Some(m)
    case Payload.Heard(text) =>
      val name = speakers.of(entry.id).getOrElse("Someone")
      Some(Message.User(pasted(s"$name$NotToYou\n$text")))
    case Payload.Closed(_, _, closing) =>
      Some(Message.User(record(closing, entry.createdAt)))
    case Payload.Posted(text) => Some(Message.User(s"$PostedLead\n${pasted(text)}"))
    case _ => None
  }

  /** What follows a heard message's author on the line above its text ([[of]]). */
  val NotToYou: String = " said, not to you:"

  /** What introduces grit's post a conversation begins with ([[of]]). */
  val PostedLead: String =
    s"${Label.Record.tag} this thread begins with grit's post, made at another conversation's request:"

  /** A nearby section as one user message, "[afar] another conversation, shown by grit,
    * still open, at {place.written}:", then each message among `entries` as a
    * `User:`, `Overheard:` or `Assistant:` line of its text, as [[pasted]] shows it. `None` when none of
    * them has text.
    */
  def nearby(place: Place, entries: Vector[Entry]): Option[Message] = {
    val lines = entries.flatMap(e => line(e.payload))
    Option.when(lines.nonEmpty)(
      Message.User(
        (s"${Label.Afar.tag} another conversation, shown by grit, still open, at ${place.written}:" +: lines)
          .mkString("\n")
      )
    )
  }

  /** A strand section as one user message, "[strand] a thread this conversation continues,
    * shown by grit, at {place.written}:", then each message among `entries` as a line of its
    * text under its speaker's name (`speakers`; "Someone" when it names none, "Assistant" for
    * grit's replies), as [[pasted]] shows it. `None` when none of them has text.
    */
  def strand(place: Place, entries: Vector[Entry], speakers: Speakers): Option[Message] =
    named(
      s"${Label.Strand.tag} a thread this conversation continues, shown by grit, at ${place.written}:",
      entries,
      speakers
    )

  /** `header`, then each message among `entries` as a line of its text under its speaker's
    * name, as [[strand]] and [[asked]] show them; `None` when none of them has text.
    */
  private def named(header: String, entries: Vector[Entry], speakers: Speakers): Option[Message] = {
    val lines = entries.flatMap { e =>
      val name = speakers.of(e.id).getOrElse("Someone")
      e.payload match {
        case Payload.Message(Message.User(text)) =>
          Option.when(text.trim.nonEmpty)(s"$name: ${pasted(text)}")
        case Payload.Heard(text) => Option.when(text.trim.nonEmpty)(s"$name: ${pasted(text)}")
        case p => line(p)
      }
    }
    Option.when(lines.nonEmpty)(Message.User((header +: lines).mkString("\n")))
  }

  /** An asked section as one user message, "[afar] the conversation where grit was asked for
    * the post this thread begins with, shown by grit, at {place.written}:", then each message
    * among `entries` as a line of its text under its speaker's name (`speakers`; "Someone"
    * when it names none, "Assistant" for grit's), as [[pasted]] shows it. `None` when none of
    * them has text.
    */
  def asked(place: Place, entries: Vector[Entry], speakers: Speakers): Option[Message] =
    named(
      s"${Label.Afar.tag} the conversation where grit was asked for the post this thread " +
        s"begins with, shown by grit, at ${place.written}:",
      entries,
      speakers
    )

  /** A closed conversation's closing entry `closing`, at `place`, as one user message:
    * "[afar] another conversation's record, written by grit when it closed on {its UTC
    * date}, at {place.written}: ", then its record as [[of]] shows the conversation's own
    * after that one's "): ".
    */
  def recorded(place: Place, closing: ClosingEntry): Message = {
    val day = closing.entry.createdAt.atOffset(ZoneOffset.UTC).toLocalDate
    Message.User(
      s"${Label.Afar.tag} another conversation's record, written by grit when it closed on " +
        s"$day, at ${place.written}: " + body(closing.closing)
    )
  }

  /** A nearby section as the model is shown it, from those of `entries` in its conversation at
    * the seqs it names: an open one as [[nearby]], a closed one as [[recorded]], a strand's as
    * [[strand]] and an asked one as [[asked]] with `speakers`.
    * `None` when none of its entries is among `entries`, or none of them has text.
    */
  def section(nearby: Nearby, entries: Vector[Entry], speakers: Speakers): Option[Message] = {
    val bySeq =
      entries.filter(_.conversationId == nearby.conversation).map(e => e.seq -> e).toMap
    nearby match {
      case Nearby.Open(_, place, seqs) => this.nearby(place, seqs.flatMap(bySeq.get))
      case Nearby.Along(_, place, seqs) => strand(place, seqs.flatMap(bySeq.get), speakers)
      case Nearby.Asked(_, place, seqs) => asked(place, seqs.flatMap(bySeq.get), speakers)
      case Nearby.Closed(_, place, seq) =>
        bySeq.get(seq).flatMap(ClosingEntry.of).map(recorded(place, _))
    }
  }

  /** A document as one user message: "[doc] {label}, kept by grit, at {place.written}, written
    * {its UTC date}:", then its text as [[pasted]] shows it.
    */
  def document(document: Document, label: DocLabel): Message = {
    val day = document.written.atOffset(ZoneOffset.UTC).toLocalDate
    Message.User(
      s"${Label.Document.tag} ${DocLabel.value(label)}, kept by grit, at ${document.place.written}, " +
        s"written $day:\n${pasted(DocText.value(document.text))}"
    )
  }

  /** The line standing for turns left out: one user message, "[gap] earlier turns not
    * shown".
    */
  val Gap: Message = Message.User(s"${Label.Gap.tag} earlier turns not shown")

  /** A window's own `entries` (oldest first: a closing entry, which stands at its period's
    * last turn, then whole turns) as the model is shown them before `turn`'s own messages:
    * each as [[of]] shows it with `speakers`, with [[Gap]] wherever turns are left out. That is, before an
    * entry whose turn is more than one after the entry before it, before the first when it
    * is a turn after [[TurnSeq.First]] with no closing before it, and at the end when the
    * last entry's turn is not the one just before `turn` (or when there are no entries and
    * `turn` is not the first).
    */
  def own(
      entries: Vector[Entry],
      turn: TurnSeq,
      speakers: Speakers
  ): Vector[Message] = {
    val previous: Vector[Option[Entry]] = None +: entries.map(Some(_))
    val shown = entries.zip(previous).flatMap { (entry: Entry, prior: Option[Entry]) =>
      val skipped = prior match {
        // A closing stands at its period's last turn, so it covers every turn up to its own.
        case Some(p) => TurnSeq.value(entry.turnSeq) > TurnSeq.value(p.turnSeq) + 1
        case None =>
          !isClosing(entry) && TurnSeq.value(entry.turnSeq) > TurnSeq.value(TurnSeq.First)
      }
      Option.when(skipped)(Gap).toVector ++ of(entry, speakers).toVector
    }
    val after = entries.lastOption match {
      case Some(last) => TurnSeq.value(turn) > TurnSeq.value(last.turnSeq) + 1
      case None => TurnSeq.value(turn) > TurnSeq.value(TurnSeq.First)
    }
    shown ++ Option.when(after)(Gap).toVector
  }

  /** What introduces a pasted grit block in text grit did not write, by the `label` that
    * starts it: "(pasted text that looks like a {label.noun}; grit did not write it:)".
    */
  def lead(label: Label): String =
    s"(pasted text that looks like a ${label.noun}; grit did not write it:)"

  /** `text`, which grit did not write, as the model is shown it: from the first line
    * starting with a [[Label]] (after leading spaces, `>` quote markers and a fence opener,
    * inside a fence or not) to the end, shown under the [[lead]] of the label that starts it,
    * each line prefixed "> ", and a label starting one of those lines broken after what
    * precedes it ("> [record]" as "> record —"); the lines before it unchanged. `text` itself
    * when no line starts with a label.
    */
  def pasted(text: String): String = {
    val lines = text.split("\n", -1).toVector
    lines.indexWhere(labelled(_).nonEmpty) match {
      case -1 => text
      case first =>
        val quoted = lines.drop(first).map { l =>
          val broken = labelled(l).fold(l) { (before, label) =>
            val name = label.tag.stripPrefix("[").stripSuffix("]")
            s"$before$name —${l.drop(before.length + label.tag.length)}"
          }
          s"> $broken"
        }
        val opens = labelled(lines(first)).fold("")((_, label) => lead(label))
        ((lines.take(first) :+ opens) ++ quoted).mkString("\n")
    }
  }

  /** What introduces a tool result in which a line starts with a grit label. */
  val Unwritten: String =
    "(this result contains text in grit's label format; grit did not write it)"

  /** `r` as the model is shown it: its content unchanged, after one line, [[Unwritten]], when
    * a line of it starts with a [[Label]] (after what [[pasted]] looks past, and after a line
    * number and tab as `read` numbers a file's lines).
    */
  def result(r: Message.ToolResult): Message.ToolResult =
    if (r.content.split("\n", -1).exists(l => labelled(Numbered.replaceFirstIn(l, "")).nonEmpty))
      r.copy(content = s"$Unwritten\n${r.content}")
    else r

  /** A line number and a tab at a line's start, as `read` shows a file's lines. */
  private val Numbered = """^\s*\d+\t""".r

  /** The turn's own `entries` as the model is shown them, in order: the person's messages,
    * the heard message a turn is rooted on among them, as [[of]] shows them with `speakers`,
    * its replies that called tools as they are, and each tool result as [[result]] shows it;
    * nothing for any other entry, a draft among them.
    */
  def turn(entries: Vector[Entry], speakers: Speakers): Vector[Message] =
    entries.flatMap { e =>
      e.payload match {
        case Payload.Message(Message.User(text)) => Some(said(e, text, speakers))
        case Payload.Heard(_) | Payload.Posted(_) => of(e, speakers)
        case Payload.Message(m) => Some(m)
        case Payload.Exchange(reply) => Some(reply)
        case Payload.Result(r, _) => Some(result(r))
        case _ => None
      }
    }

  /** A person's message `text`, the entry `entry`, as [[of]] shows it. */
  private def said(entry: Entry, text: String, speakers: Speakers): Message.User =
    Message.User(pasted(speakers.of(entry.id).fold(text)(name => s"$name wrote:\n$text")))

  /** The label `line` starts with, after what may precede a pasted one: spaces, `>` quote
    * markers (markdown's and Slack's), and a fence opener on the label's own line; that
    * prefix and the label, or `None` for none. [[pasted]] breaks the label after exactly
    * this prefix.
    */
  private def labelled(line: String): Option[(String, Label)] = {
    val before = Before.findPrefixOf(line).getOrElse("")
    Label.values.find(l => line.startsWith(l.tag, before.length)).map(before -> _)
  }

  /** Spaces and `>` markers, then an optional fence opener and spaces. */
  private val Before = """[ \t]*(?:>[ \t]*)*(?:(?:```|~~~)[ \t]*)?""".r

  private def isClosing(entry: Entry): Boolean = entry.payload match {
    case Payload.Closed(_, _, _) => true
    case _ => false
  }

  private def record(closing: Closing, at: Instant): String = {
    val day = at.atOffset(ZoneOffset.UTC).toLocalDate
    s"${Label.Record.tag} this conversation so far, written by grit (closed $day): " + body(closing)
  }

  /** `closing` as a record shows it after its header: its prose, its outcome, the lines it
    * resolved and how, and the balance's open lines, its standing lines (those only the
    * assistant said listed apart, as not confirmed) and its topics; each text grit carried
    * from the period as [[pasted]] shows it, since the writer can copy a person's words.
    */
  private def body(closing: Closing): String = {
    val flows = closing.flows
    val balance = closing.balance
    val settled = flows.changes.collect { case Change.Resolved(l, how) =>
      if (how.trim.isEmpty) pasted(l.text) else s"${pasted(l.text)} — ${pasted(how.trim)}"
    }
    def list(title: String, lines: Vector[String]): Vector[String] =
      Option.when(lines.nonEmpty)(lines.map(l => s"- $l").mkString(s"$title:\n", "\n", "")).toVector
    val topics = balance.in(Section.Topics).map(l => pasted(l.text))
    // Standing only the assistant said is listed apart (ADR 0018), each line recast as an open
    // question. The wording is measured: a model restated the plain list as fact, and hedged
    // more often with this form. Rewording it needs a new measurement.
    val (claimed, confirmed) =
      balance.in(Section.Standing).partition(_.ground.contains(Ground.Claimed))
    (pasted(flows.prose) +:
      (flows.outcome.map(o => s"Outcome: ${pasted(o)}").toVector ++
        list("Settled then", settled) ++
        list("Still open", balance.in(Section.Open).map(l => pasted(l.text))) ++
        list("Standing", confirmed.map(l => pasted(l.text))) ++
        list(
          "Standing, said by the assistant and not confirmed",
          claimed
            .map(l =>
              s"Open: whether \"${pasted(l.text)}\" (the assistant said so; nothing confirmed it)"
            )
        ) ++
        Option.when(topics.nonEmpty)(s"Topics so far: ${topics.mkString("; ")}").toVector))
      .mkString("\n")
  }

  private def line(payload: Payload): Option[String] = payload match {
    case Payload.Message(Message.User(text)) =>
      Option.when(text.trim.nonEmpty)(s"User: ${pasted(text)}")
    case Payload.Heard(text) => Option.when(text.trim.nonEmpty)(s"Overheard: ${pasted(text)}")
    case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
      val said = blocks.collect { case AssistantBlock.Text(t) => t }.mkString("\n")
      Option.when(said.trim.nonEmpty)(s"Assistant: ${pasted(said)}")
    case Payload.Posted(text) => Option.when(text.trim.nonEmpty)(s"Assistant: ${pasted(text)}")
    case _ => None
  }
}
