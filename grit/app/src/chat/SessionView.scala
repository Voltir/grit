package grit.app.chat

import grit.core.id.{EntryId, TurnSeq}
import grit.core.message.{Message, Tokens, Usage}
import grit.core.store.{Entry, Payload, UsageLedger}
import grit.core.topic.{Placement, TopicEvent}

/** The conversation so far, as the panel's session tab shows it: how long it is, what it
  * cost, what search recalled, and which models answered in each role. Pure data, built by
  * [[SessionView.of]] from the conversation's entries and its turns' ledger rows.
  *
  * @param turns the turns asked so far, answered or not
  * @param messages the user's messages and the replies
  * @param input the input tokens every model call was billed for
  * @param output the output tokens every model call was billed for
  * @param spent what every model call cost, when every one of them was priced
  * @param recalls how many turns' windows recalled an earlier turn
  * @param recalled every earlier turn some window recalled, in conversation order
  * @param roles each role that called a model, in the order a turn calls them
  */
final case class SessionView(
    turns: Int,
    messages: Int,
    input: Tokens,
    output: Tokens,
    spent: Option[BigDecimal],
    recalls: Int,
    recalled: Vector[TurnSeq],
    roles: Vector[SessionView.Role]
)

object SessionView {

  /** A conversation with nothing in it. */
  val empty: SessionView =
    SessionView(0, 0, Tokens.Zero, Tokens.Zero, None, 0, Vector.empty, Vector.empty)

  /** What one role's calls came to: the models that answered it, in the order they first
    * did, how many calls it made, and what they cost when every one was priced.
    */
  final case class Role(
      name: String,
      models: Vector[String],
      calls: Int,
      spent: Option[BigDecimal]
  )

  /** The roles, by what each writes: the query assembly searched with, the reply, the
    * turn's summary, the classifier's placement of its message among the topics, and the
    * main model's verdict on it.
    */
  val Query = "query"
  val Turn = "turn"
  val Summary = "summary"
  val Classify = "classify"
  val Verdict = "verdict"

  /** The conversation `entries` describe, with `costs`: the ledger rows of its turns, in
    * any order. A row is put to a role by the entry it holds; a row whose entry is not in
    * `entries` counts toward the totals and no role.
    */
  def of(entries: Vector[Entry], costs: Vector[UsageLedger.Row]): SessionView = {
    val roleOf: Map[EntryId, String] = entries.flatMap { e =>
      (e.payload match {
        case Payload.Query(_) => Some(Query)
        case Payload.Message(Message.Assistant(_, _, _, _)) => Some(Turn)
        case Payload.Summary(_) => Some(Summary)
        case Payload.Topic(events) =>
          val asked = events.exists {
            case TopicEvent.Placed(_, _, Placement.Asked(_, _)) => true
            case _ => false
          }
          Some(if (asked) Verdict else Classify)
        case _ => None
      }).map(e.id -> _)
    }.toMap
    val windows = entries.collect { case Entry(_, _, _, _, _, w: Payload.Window, _) => w }
    val roles = Vector(Turn, Query, Summary, Classify, Verdict).flatMap { name =>
      val rows = costs.filter(r => roleOf.get(r.entry).contains(name))
      Option.when(rows.nonEmpty)(Role(name, rows.map(_.model).distinct, rows.size, spent(rows)))
    }
    SessionView(
      turns = entries.collect { case e if isUser(e) => e.turnSeq }.distinct.size,
      messages = entries.count(e => isUser(e) || isReply(e)),
      input = costs.foldLeft(Tokens.Zero)(_ + _.usage.input),
      output = costs.foldLeft(Tokens.Zero)(_ + _.usage.output),
      spent = spent(costs),
      recalls = windows.count(_.recalled.nonEmpty),
      recalled = windows
        .flatMap(_.recalled)
        .distinct
        .sortBy(TurnSeq.value),
      roles = roles
    )
  }

  /** Which turns of `entries` to read the ledger rows of, when those of the turns
    * `settled` are already read and can change no more; and the turns settled once these
    * are read. A turn settles when it is read with its summary in `entries`: the summary is
    * the last entry a turn writes, in the same transaction as its ledger row.
    */
  def unread(entries: Vector[Entry], settled: Set[TurnSeq]): (Vector[TurnSeq], Set[TurnSeq]) = {
    val summarised = entries.collect { case Entry(_, _, t, _, _, Payload.Summary(_), _) => t }
    (entries.map(_.turnSeq).distinct.filterNot(settled), settled ++ summarised)
  }

  private def spent(rows: Vector[UsageLedger.Row]): Option[BigDecimal] =
    Option.when(rows.nonEmpty)(Usage.total(rows.map(_.usage))).flatMap(_.costUsd)

  private def isUser(e: Entry): Boolean = e.payload match {
    case Payload.Message(Message.User(_)) => true
    case _ => false
  }

  private def isReply(e: Entry): Boolean = e.payload match {
    case Payload.Message(Message.Assistant(_, _, _, _)) => true
    case _ => false
  }
}
