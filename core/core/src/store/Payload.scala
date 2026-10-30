package grit.core.store

import java.time.Instant

import grit.core.id.{
  DocumentVersionId,
  EntryId,
  EntrySeq,
  PeriodSeq,
  PieceId,
  Sha256,
  ToolCallId,
  TurnSeq
}
import grit.core.message.{Message, Tokens}
import grit.core.period.{CloseReason, Closing}

/** What an [[Entry]] holds. Each case is one kind of entry; context fetched during
  * assembly is a future case.
  */
enum Payload {

  /** A message sent to or received from the model. */
  case Message(message: grit.core.message.Message)

  /** A person's message where grit listens but was not addressed
    * ([[grit.core.inbox.Inbox.hear]]): the first entry of its turn, which runs only when grit
    * drafts a reply to it ([[grit.core.speech.Speech.decide]]). Its author is in
    * [[Speakers]]. Shown to the model as speech not said to it, and searched by its text.
    */
  case Heard(text: String)

  /** grit's post that its conversation begins with, made at another turn's request
    * ([[grit.core.inbox.Inbox.posted]]): the first entry of a turn that never runs. Shown to
    * the model as grit's post, quoted ([[grit.core.context.Shown.of]]), and searched by its
    * text.
    */
  case Posted(text: String)

  /** A short summary of the turn the entry belongs to, written after its reply. Not a
    * message: the model sees it only if an assembler chooses to show it.
    */
  case Summary(text: String)

  /** The search query assembly wrote for the turn the entry belongs to. A record of what
    * was searched for: never shown to the model, never itself searched.
    */
  case Query(text: String)

  /** The window the reply of the turn the entry belongs to was written from
    * ([[grit.core.context.Window]]): the seqs of the entries of its own conversation the
    * model saw before the turn's own, in the order it saw them, which earlier turns among them search
    * `recalled` rather than recency, the `nearby` sections from other conversations, the
    * documents it held, by version, and the pieces of files it held, by id with how many more
    * of its file's pieces its search matched (the count the model was shown). A record of what
    * the model saw: never shown to the model, never searched. `carry`, how a later turn of its
    * window carries it ([[Payload.Carry]]): `None` for a record kept before windows were
    * carried, which no later turn carries. With one, what it names is what its turn's window
    * drew (ADR 0052): the whole window at a boundary, else only what was appended after
    * `carry.after`'s.
    */
  case Window(
      entries: Vector[EntrySeq],
      recalled: Vector[TurnSeq],
      nearby: Vector[Nearby] = Vector.empty,
      documents: Vector[DocumentVersionId] = Vector.empty,
      pieces: Vector[(PieceId, Int)] = Vector.empty,
      carry: Option[Payload.Carry]
  )

  /** What happened to the conversation's agenda during the turn the entry belongs to
    * ([[grit.core.agenda.Agenda.fold]] folds them). A record: never shown to the model, never
    * searched. Stored under the kind `"topic"`, the name from before the rename to agenda.
    */
  case Agenda(events: Vector[grit.core.agenda.Agenda.Event])

  /** A reply inside the tool loop of the turn the entry belongs to that called tools; each
    * call's [[Result]] follows it. The turn's own later model calls and its summary read it;
    * no window includes it, so a call is never shown without its result. Never searched.
    */
  case Exchange(reply: grit.core.message.Message.Assistant)

  /** A tool call's result inside the tool loop of the turn the entry belongs to, read as
    * [[Exchange]] is. `shown` is its call in one line, as a person is shown it: the tool's
    * name and what the call acts on, or only the name as sent when the call did not read.
    */
  case Result(result: grit.core.message.Message.ToolResult, shown: String)

  /** What the person a turn answers said when they declined its call `call`, a gated one: their
    * own words, kept beside the call's [[Result]], which says only that it was declined. Shown
    * to the model as their message once the results of the reply that made the call end
    * ([[grit.core.context.Shown.turn]]); no window includes it, and it is never searched.
    */
  case Declined(call: ToolCallId, reason: String)

  /** A tool call that asks a person first began to run, before its result was recorded: a
    * record that a crash may have cut it short. Never shown to the model, never searched.
    */
  case Attempt(call: ToolCallId)

  /** A tool call that asks a person first, waiting for their answer until `until` (`None` for an
    * ask recorded before asks were dated): `shown` is what they are asked to approve. Answered
    * through [[grit.core.inbox.Inbox.answer]]; the call's result follows once they answer or
    * the wait runs out. Never shown to the model, never searched.
    */
  case Ask(call: ToolCallId, shown: String, until: Option[Instant] = None)

  /** A period's closing entry, written when it closed. Searched by its flows; shown to the
    * model as [[grit.core.context.Shown.of]] shows it when a window includes it.
    */
  case Closed(period: PeriodSeq, reason: CloseReason, closing: Closing)

  /** The prefix of the last call the turn the entry belongs to made, as a later turn of its
    * window must send it again to carry that turn (ADR 0052): the digest of the call's system
    * prompt, its tools and its first `messages` messages, those a later call repeats
    * ([[grit.core.provider.ModelRequest.stable]]). One per answered turn, about 100 bytes; a
    * record: never shown to the model, never searched.
    */
  case Prefix(messages: Int, digest: Sha256)

  /** An unprompted turn's answer before it was judged ([[grit.core.speech.Outcome]]): a
    * record, never shown to the model, never searched, and not a period's activity. A posted
    * one is also the turn's reply.
    */
  case Draft(message: grit.core.message.Message.Assistant)

  /** The words a person said in this entry, to grit or heard; `None` for any other entry. */
  def said: Option[String] = this match {
    case Message(grit.core.message.Message.User(text)) => Some(text)
    case Heard(text) => Some(text)
    case _ => None
  }
}

object Payload {

  /** What a later turn of its window needs of a window record to show it again (ADR 0052):
    * `after`, the window record it was appended after, `None` when it was drawn at a boundary;
    * `cost`, what this record's part drew, as shown, in the estimated tokens of the assembler
    * that drew it: this part alone, not the window it follows; `dated` and `tag`, the line its
    * turn's request sent just before the turn's own messages (`None` for a turn with no own
    * entry) and the note it added to the turn's asking message (`None` when it added none),
    * each as sent, so a later turn repeats their bytes.
    */
  final case class Carry(
      after: Option[EntryId],
      cost: Tokens,
      dated: Option[String],
      tag: Option[String]
  )

  /** The asks among `payloads`, a turn's in order, still waiting for an answer at `now`: each
    * whose call has no [[Result]] among them, and whose `until` has not passed (one recorded
    * with none waits until its turn stops waiting).
    */
  def waiting(payloads: Vector[Payload], now: Instant): Vector[Ask] = {
    val settled = payloads.collect { case Result(result, _) => result.callId }.toSet
    payloads.collect {
      case ask: Ask if !settled.contains(ask.call) && ask.until.forall(now.isBefore) => ask
    }
  }

  /** How a person's words name a file they shared with them: `[file: {name}]`. */
  def attached(name: String): String = s"[file: $name]"

  /** Whether `text`, a person's words, says nothing: blank once each file it names
    * ([[attached]]) is taken out.
    */
  def saysNothing(text: String): Boolean = Attached.replaceAllIn(text, "").isBlank

  /** `declined` as the words of `who`, the person who declined it: "{who} declined the call
    * {call}, and wrote:", a newline, then their reason; the call named as its [[Result]] among
    * `payloads` shows it, else by its id. Every view of a declined call says it this way.
    */
  def declined(who: String, declined: Declined, payloads: Vector[Payload]): String = {
    val call = payloads
      .collectFirst { case Result(r, shown) if r.callId == declined.call => shown }
      .getOrElse(ToolCallId.value(declined.call))
    s"$who declined the call $call, and wrote:\n${declined.reason}"
  }

  private val Attached = """\[file: [^\]]*\]""".r
}
