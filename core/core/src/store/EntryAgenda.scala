package grit.core.store

import grit.core.agenda.Agenda
import grit.core.id.TurnSeq
import grit.core.period.{Balance, Edit, Line, Section}

/** A conversation's agenda, one period at a time: carried by the balance of the newest
  * closing entry before a turn, then placed by that period's own agenda events. The same
  * whether or not earlier periods' raw entries are purged.
  */
object EntryAgenda {

  /** The items in `all` as they stood when `turn` began: the items of the newest closing
    * entry before it ([[Agenda.Carried]]), then the agenda events of the turns after that
    * closing and before `turn`.
    */
  def before(all: Vector[Entry], turn: TurnSeq): Agenda = {
    val earlier = all.filter(e => TurnSeq.value(e.turnSeq) < TurnSeq.value(turn))
    val opened = earlier.flatMap(ClosingEntry.of).lastOption
    val from = opened.fold(Long.MinValue)(o => TurnSeq.value(o.entry.turnSeq))
    Agenda.fold(
      opened.fold(Vector.empty[Agenda.Carried])(o => carried(o.closing.balance)),
      earlier.filter(e => TurnSeq.value(e.turnSeq) > from).flatMap(e => events(e.payload))
    )
  }

  /** As [[before]], with `turn`'s own events. */
  def through(all: Vector[Entry], turn: TurnSeq): Agenda = before(all, turn.next)

  /** The agenda events `payload` holds; none unless it is an agenda record. */
  def events(payload: Payload): Vector[Agenda.Event] = payload match {
    case Payload.Agenda(events) => events
    case _ => Vector.empty
  }

  /** The edits a close makes to the items of `balance`, the balance its period opened with,
    * given the item `events` of the period's own turns, oldest spoken in first: a carried
    * item that a message was placed in is touched; an item opened in the period and named is
    * added under its name; an unnamed one is left out. Each item kept whose summary the
    * period described anew is then summarised with it.
    */
  def edits(balance: Balance, events: Vector[Agenda.Event]): Vector[Edit] = {
    val agenda = Agenda.fold(carried(balance), events)
    val lines = balance.in(Section.Agenda).map(l => Agenda.ItemId.carried(l.id) -> l).toMap
    agenda.items.reverse.filter(_.turns.nonEmpty).flatMap { t =>
      lines.get(t.id) match {
        case Some(line) =>
          Edit.Touch(line.id) +:
            t.summary.filterNot(line.summary.contains).map(Edit.Summarise(line.id, _)).toVector
        case None =>
          t.name.toVector.flatMap { name =>
            Edit.Add(Section.Agenda, name) +:
              t.summary.map(Edit.Summarise(Line.idOf(Section.Agenda, name), _)).toVector
          }
      }
    }
  }

  /** `balance`'s items as the next period carries them, most recently spoken in first. */
  private def carried(balance: Balance): Vector[Agenda.Carried] =
    balance
      .in(Section.Agenda)
      .reverse
      .map(l => Agenda.Carried(Agenda.ItemId.carried(l.id), l.text, l.summary))
}
