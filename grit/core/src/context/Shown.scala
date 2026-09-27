package grit.core.context

import java.time.{Instant, ZoneOffset}

import grit.core.id.TurnSeq
import grit.core.message.{AssistantBlock, Message}
import grit.core.period.{Change, Closing, Section}
import grit.core.place.Place
import grit.core.store.{Entry, Payload}

/** What the model is shown of an entry a window names: the one definition, which an
  * assembler costs and the turn sends.
  */
object Shown {

  /** A message as it is; a closing entry as one user message, "[record] this conversation
    * so far, written by grit (closed {its UTC date}):", then its prose, its outcome, the
    * lines it resolved and how, and the balance's open, standing and topic lines; `None`
    * for any other entry.
    */
  def of(entry: Entry): Option[Message] = entry.payload match {
    case Payload.Message(m) => Some(m)
    case Payload.Closed(_, _, closing) =>
      Some(Message.User(record(closing, entry.createdAt)))
    case _ => None
  }

  /** A nearby section as one user message, "[afar] another conversation of yours, shown by
    * grit, still open, at {place.written}:", then each message among `entries` as a
    * `User:` or `Assistant:` line of its text. `None` when none of them has text.
    */
  def nearby(place: Place, entries: Vector[Entry]): Option[Message] = {
    val lines = entries.flatMap(e => line(e.payload))
    Option.when(lines.nonEmpty)(
      Message.User(
        (s"${Label.Afar.tag} another conversation of yours, shown by grit, still open, at ${place.written}:" +: lines)
          .mkString("\n")
      )
    )
  }

  /** The line standing for turns left out: one user message, "[gap] earlier turns not
    * shown".
    */
  val Gap: Message = Message.User(s"${Label.Gap.tag} earlier turns not shown")

  /** A window's own `entries` (oldest first: a closing entry, which stands at its period's
    * last turn, then whole turns) as the model is shown them before `turn`'s own messages:
    * each as [[of]] shows it, with [[Gap]] wherever turns are left out. That is, before an
    * entry whose turn is more than one after the entry before it, before the first when it
    * is a turn after [[TurnSeq.First]] with no closing before it, and at the end when the
    * last entry's turn is not the one just before `turn` (or when there are no entries and
    * `turn` is not the first).
    */
  def own(entries: Vector[Entry], turn: TurnSeq): Vector[Message] = {
    val previous: Vector[Option[Entry]] = None +: entries.map(Some(_))
    val shown = entries.zip(previous).flatMap { (entry: Entry, prior: Option[Entry]) =>
      val skipped = prior match {
        // A closing stands at its period's last turn, so it covers every turn up to its own.
        case Some(p) => TurnSeq.value(entry.turnSeq) > TurnSeq.value(p.turnSeq) + 1
        case None =>
          !isClosing(entry) && TurnSeq.value(entry.turnSeq) > TurnSeq.value(TurnSeq.First)
      }
      Option.when(skipped)(Gap).toVector ++ of(entry).toVector
    }
    val after = entries.lastOption match {
      case Some(last) => TurnSeq.value(turn) > TurnSeq.value(last.turnSeq) + 1
      case None => TurnSeq.value(turn) > TurnSeq.value(TurnSeq.First)
    }
    shown ++ Option.when(after)(Gap).toVector
  }

  private def isClosing(entry: Entry): Boolean = entry.payload match {
    case Payload.Closed(_, _, _) => true
    case _ => false
  }

  private def record(closing: Closing, at: Instant): String = {
    val flows = closing.flows
    val balance = closing.balance
    val day = at.atOffset(ZoneOffset.UTC).toLocalDate
    val settled = flows.changes.collect { case Change.Resolved(l, how) =>
      if (how.trim.isEmpty) l.text else s"${l.text} — ${how.trim}"
    }
    def list(title: String, lines: Vector[String]): Vector[String] =
      Option.when(lines.nonEmpty)(lines.map(l => s"- $l").mkString(s"$title:\n", "\n", "")).toVector
    val topics = balance.in(Section.Topics).map(_.text)
    (s"${Label.Record.tag} this conversation so far, written by grit (closed $day): ${flows.prose}" +:
      (flows.outcome.map(o => s"Outcome: $o").toVector ++
        list("Settled then", settled) ++
        list("Still open", balance.in(Section.Open).map(_.text)) ++
        list("Standing", balance.in(Section.Standing).map(_.text)) ++
        Option.when(topics.nonEmpty)(s"Topics so far: ${topics.mkString("; ")}").toVector))
      .mkString("\n")
  }

  private def line(payload: Payload): Option[String] = payload match {
    case Payload.Message(Message.User(text)) => Option.when(text.trim.nonEmpty)(s"User: $text")
    case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
      val said = blocks.collect { case AssistantBlock.Text(t) => t }.mkString("\n")
      Option.when(said.trim.nonEmpty)(s"Assistant: $said")
    case _ => None
  }
}
