package grit.lifecycle.transcript

import java.util.regex.Pattern

import grit.core.id.{PeriodRef, ToolCallId, TurnSeq}
import grit.core.message.{AssistantBlock, Cut, Message}
import grit.core.period.Section
import grit.core.persona.Persona
import grit.core.stitch.Stitching
import grit.core.store.{Entry, EntryStore, Nearby, Payload, Principals, Reads, Speakers, StoreError}

/** A period as a classifier or the summary model reads it. */
object PeriodTranscript {

  /** The entries of `period`'s turns `first` to `last`, oldest first, read from `store`
    * through `db`.
    */
  def entries(
      db: Reads^,
      store: EntryStore,
      period: PeriodRef,
      first: TurnSeq,
      last: TurnSeq
  ): Either[StoreError, Vector[Entry]] =
    db.read(store.list(period.conversationId))
      .map(_.filter { e =>
        val t = TurnSeq.value(e.turnSeq)
        t >= TurnSeq.value(first) && t <= TurnSeq.value(last)
      })

  /** The names of whoever wrote `entries` ([[Principals.speakers]]), read through `db`. */
  def speakers(
      db: Reads^,
      principals: Principals,
      entries: Vector[Entry]
  ): Either[StoreError, Speakers] =
    db.read(principals.speakers(entries.map(_.id)))

  /** `entries` as one transcript: each person's message, to grit or heard, under their name
    * in `speakers` (`User: …` for one to grit unnamed, `Someone: …` for one heard unnamed), and
    * each reply's text as `Assistant: …`, in order, a blank line between them; a reply with no
    * text, and every other kind of entry, left out.
    */
  def of(entries: Vector[Entry], speakers: Speakers): String =
    lines(entries, speakers).map(_.shown).mkString("\n\n")

  /** [[of]] by message, each one [[Stitching.Line]]. */
  def lines(entries: Vector[Entry], speakers: Speakers): Vector[Stitching.Line] =
    entries.flatMap(line(_, speakers))

  /** What the recorded windows among `entries` showed from other conversations
    * ([[Payload.Window]]'s nearby sections), read from `store` through `db`: each entry
    * once, in the order first shown; a message as `[{place}] User: …`,
    * `[{place}] Someone: …` (heard) or `[{place}] Assistant: …`, as [[of]] writes it with no
    * names, and a closed conversation's record as
    * `[{place}] Record: {prose}` and one `[{place}] Standing: {line}` for each Standing
    * line. One gone since (purged, or collected) is left out.
    */
  def elsewhere(
      db: Reads^,
      store: EntryStore,
      entries: Vector[Entry]
  ): Either[StoreError, Vector[String]] = {
    val sections = entries.flatMap(_.payload match {
      case Payload.Window(_, _, nearby, _, _) => nearby
      case _ => Vector.empty
    })
    val shown = sections
      .flatMap(n => n.names.map(seq => (n.place.written, n.conversation, seq)))
      .distinctBy((_, c, seq) => (c, seq))
    if (shown.isEmpty) Right(Vector.empty)
    else
      db.read(Nearby.read(sections, store)).map { found =>
        val at = found.map(e => (e.conversationId, e.seq) -> e).toMap
        shown.flatMap((place, c, seq) =>
          at.get((c, seq)).toVector.flatMap(known).map(l => s"[$place] $l")
        )
      }
  }

  /** What `entry`, shown from elsewhere, is known as: a message's line, or a record's prose
    * and Standing lines.
    */
  private def known(entry: Entry): Vector[String] = entry.payload match {
    case Payload.Closed(_, _, closing) =>
      s"Record: ${closing.flows.prose}" +:
        closing.balance.in(Section.Standing).map(l => s"Standing: ${l.text}")
    case _ => line(entry, Speakers.none).map(_.shown).toVector
  }

  /** Whether grit only heard the period whose entries are `entries`: some message was heard
    * ([[Payload.Heard]]), none was said to grit, and grit said nothing in it but the post it
    * may begin with ([[Payload.Posted]]), which it made elsewhere.
    */
  def overheard(entries: Vector[Entry]): Boolean =
    entries.exists(_.payload match {
      case Payload.Heard(_) => true
      case _ => false
    }) && !entries.exists(_.payload match {
      case Payload.Message(_) => true
      case _ => false
    })

  /** The most characters of a tool line's result when the line grounds nothing (an error, or
    * one below Outside), an ending "…" included.
    */
  val ToolChars = 120

  /** The most characters of a tool line's result when the line can ground
    * ([[Labelled.supports]]), an ending "…" included: enough to show what it established.
    */
  val GroundingToolChars = 4000

  /** `entries` as [[Labelled]]: each person's message (`u` to grit, `h` heard) and reply
    * text as [[of]] renders it with `speakers`, but a person's message mentioning the
    * assistant, which presents as `persona`, as their words to it (`[h1] Ana, to the
    * assistant: …`, the mention taken out); a declined call's reason as a `u` line of the
    * person its turn answers (`User` unnamed), "declined the call {shown}, and wrote: …"; and
    * each tool result as its call as shown, " →",
    * " failed:" when it failed, and its content on one line as the line's result, clipped to
    * [[GroundingToolChars]] when it can ground ([[Labelled.supports]]), else to [[ToolChars]];
    * each line knows the entry it was rendered from, and a tool line the name its call among
    * `entries` gave.
    */
  def labelled(entries: Vector[Entry], speakers: Speakers, persona: Persona): Labelled = {
    // Each call's name, by its id, as the replies among the entries sent it.
    val called: Map[ToolCallId, String] = entries
      .flatMap(_.payload match {
        case Payload.Exchange(reply) => reply.blocks
        case Payload.Message(reply: Message.Assistant) => reply.blocks
        case _ => Vector.empty
      })
      .collect { case AssistantBlock.ToolCall(id, name, _) => id -> name }
      .toMap
    // Each call as shown, by its id; and the name of whoever each turn answers, by the turn.
    val shownCalls: Map[ToolCallId, String] = entries
      .map(_.payload)
      .collect { case Payload.Result(r, shown) => r.callId -> shown }
      .toMap
    val askers: Map[TurnSeq, String] = entries.reverse
      .filter(_.payload.said.nonEmpty)
      .flatMap(e => speakers.of(e.id).map(e.turnSeq -> _))
      .toMap
    // Who wrote each line, which entry it is, its text after the label, and a tool's result.
    val written: Vector[(Labelled.Source, Entry, String, Option[String])] = entries.flatMap(e =>
      e.payload match {
        case Payload.Declined(call, reason) =>
          val who = askers.getOrElse(e.turnSeq, "User")
          val named = shownCalls.getOrElse(call, ToolCallId.value(call))
          Some(
            (
              Labelled.Source.Person,
              e,
              s"$who: declined the call $named, and wrote: $reason",
              None
            )
          )
        case Payload.Result(result, shown) =>
          val content = result.content.trim.split("\\s+").mkString(" ")
          val failed = if (result.isError) " failed:" else ""
          val source = Labelled.Source.Tool(!result.isError, called.get(result.callId))
          val chars = if (Labelled.supports(source, e.integrity)) GroundingToolChars else ToolChars
          Some((source, e, s"$shown →$failed", Some(Cut.head(content, chars))))
        case Payload.Message(Message.User(_)) =>
          line(e, speakers).map(l => (Labelled.Source.Person, e, addressed(l, persona).shown, None))
        case Payload.Heard(_) =>
          line(e, speakers).map(l => (Labelled.Source.Heard, e, addressed(l, persona).shown, None))
        case _ => line(e, speakers).map(l => (Labelled.Source.Assistant, e, l.shown, None))
      }
    )
    Labelled.of(written.zipWithIndex.map { case ((source, e, text, result), i) =>
      val letter = source match {
        case Labelled.Source.Person => "u"
        case Labelled.Source.Heard => "h"
        case Labelled.Source.Assistant => "a"
        case Labelled.Source.Tool(_, _) => "t"
      }
      val label = s"$letter${i + 1}"
      Labelled.Line(label, e.id, source, s"[$label] $text", result, e.integrity)
    })
  }

  /** `l`, a person's line, its every mention of `persona` (`@` and its name, in any case, not
    * running on into a longer name) taken out of its text and its speaker shown speaking to the
    * assistant (`Ana, to the assistant: …`), when it holds one; else `l`.
    */
  private def addressed(l: Stitching.Line, persona: Persona): Stitching.Line = {
    val mention = ("(?i)@" + Pattern.quote(persona.name) + """(?![\p{L}\p{N}_])""").r
    if (mention.findFirstIn(l.text).isEmpty) l
    else
      l.copy(
        speaker = s"${l.speaker}, to the assistant",
        text = mention
          .replaceAllIn(l.text, "")
          .replaceAll("""\s+(?=[,.;:!?])""", "")
          .replaceAll("""\s{2,}""", " ")
          .replaceFirst("""^[\s,;:]+""", "")
          .trim
      )
  }

  /** `entry` as one line: a person's words under their name in `speakers` (`User` for one
    * unnamed, speaking to grit; `Someone` for one unnamed and heard), a reply's or a post's
    * text under `Assistant`; `None` for a reply with no text and every other kind of entry.
    */
  private def line(entry: Entry, speakers: Speakers): Option[Stitching.Line] = {
    def said(speaker: String, text: String) = Stitching.Line(speaker, text, entry.createdAt)
    entry.payload match {
      case Payload.Message(Message.User(text)) =>
        Some(said(speakers.of(entry.id).getOrElse("User"), text))
      case Payload.Heard(text) => Some(said(speakers.of(entry.id).getOrElse("Someone"), text))
      case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
        val text = blocks.collect { case AssistantBlock.Text(t) => t }.mkString.trim
        Option.when(text.nonEmpty)(said("Assistant", text))
      case Payload.Posted(text) => Option.when(text.trim.nonEmpty)(said("Assistant", text.trim))
      case _ => None
    }
  }
}
