package grit.core.store

import grit.core.id.TurnSeq
import grit.core.period.{Balance, Edit, Section}
import grit.core.topic.{TopicEvent, TopicId, Topics}

/** A conversation's topics, one period at a time: carried by the balance of the newest
  * closing entry before a turn, then placed by that period's own topic events. The same
  * whether or not earlier periods' raw entries are purged.
  */
object EntryTopics {

  /** The topics in `all` as they stood when `turn` began: the topics of the newest closing
    * entry before it ([[Topics.Carried]]), then the topic events of the turns after that
    * closing and before `turn`.
    */
  def before(all: Vector[Entry], turn: TurnSeq): Topics = {
    val earlier = all.filter(e => TurnSeq.value(e.turnSeq) < TurnSeq.value(turn))
    val opened = earlier.flatMap(ClosingEntry.of).lastOption
    val from = opened.fold(Long.MinValue)(o => TurnSeq.value(o.entry.turnSeq))
    Topics.fold(
      opened.fold(Vector.empty[Topics.Carried])(o => carried(o.closing.balance)),
      earlier.filter(e => TurnSeq.value(e.turnSeq) > from).flatMap(e => events(e.payload))
    )
  }

  /** As [[before]], with `turn`'s own events. */
  def through(all: Vector[Entry], turn: TurnSeq): Topics = before(all, turn.next)

  /** The topic events `payload` holds; none unless it is a topic record. */
  def events(payload: Payload): Vector[TopicEvent] = payload match {
    case Payload.Topic(events) => events
    case _ => Vector.empty
  }

  /** The edits a close makes to the topics of `balance`, the balance its period opened with,
    * given the topic `events` of the period's own turns, oldest spoken in first: a carried
    * topic that a message was placed in is touched; a topic opened in the period and named is
    * added under its name; an unnamed one is left out.
    */
  def edits(balance: Balance, events: Vector[TopicEvent]): Vector[Edit] = {
    val topics = Topics.fold(carried(balance), events)
    val lines = balance.in(Section.Topics).map(l => TopicId.carried(l.id) -> l).toMap
    topics.topics.reverse.filter(_.turns.nonEmpty).flatMap { t =>
      lines.get(t.id) match {
        case Some(line) => Some(Edit.Touch(line.id))
        case None => t.name.map(Edit.Add(Section.Topics, _))
      }
    }
  }

  /** `balance`'s topics as the next period carries them, most recently spoken in first. */
  private def carried(balance: Balance): Vector[Topics.Carried] =
    balance.in(Section.Topics).reverse.map(l => Topics.Carried(TopicId.carried(l.id), l.text))
}
