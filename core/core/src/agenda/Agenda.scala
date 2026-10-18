package grit.core.agenda

import grit.core.id.{LineId, TurnRef, TurnSeq, WorkflowId}

/** A conversation's agenda as it stands: what the conversation is about, item by item. Only
  * [[Agenda.fold]] makes one.
  *
  * @param items
  *   every item, most recently spoken in first. An item is as recent as the latest turn placed
  *   in it; one with no turn in it comes after all that have one, and items equally recent come
  *   newest opened first.
  * @param placements
  *   every placement of each turn's message, in the order they were made.
  */
final case class Agenda private (
    items: Vector[Agenda.Item],
    placements: Map[TurnSeq, Vector[Agenda.Event.Placed]],
    private val carried: Vector[Agenda.ItemId]
) {

  /** The item the latest placed message is in. Before any is placed, the first carried item.
    * `None` when there is neither, or when the latest message was placed in an item neither
    * opened nor carried.
    */
  def current: Option[Agenda.Item] =
    placements.keys.maxByOption(TurnSeq.value) match {
      case Some(latest) => placed(latest).flatMap(get)
      case None => carried.headOption.flatMap(get)
    }

  /** Every item but the current one, most recent first. */
  def earlier: Vector[Agenda.Item] = current.fold(items)(c => items.filterNot(_.id == c.id))

  /** The item `turn`'s latest placement puts its message in; `None` if it was never placed. */
  def placed(turn: TurnSeq): Option[Agenda.ItemId] =
    placements.get(turn).flatMap(_.lastOption).map(_.weights.heaviest)

  /** The item with `id`, if it was opened. */
  def get(id: Agenda.ItemId): Option[Agenda.Item] = items.find(_.id == id)
}

object Agenda {

  /** An item of one conversation's agenda. Named for the turn that opened it, so a turn
    * replayed opens the same item, and no two turns open the same one.
    */
  opaque type ItemId = String

  object ItemId {
    def apply(value: String): ItemId = value
    def value(id: ItemId): String = id

    /** The item `turn` opens, if it opens one. The `topic:` prefix is the stored form from
      * before the rename to agenda.
      */
    def openedBy(turn: TurnRef): ItemId = s"topic:${WorkflowId.value(turn.workflowId)}"

    /** The item the balance of a conversation's latest close carries as the line `line`: the
      * id the turns of the next period place messages under.
      */
    def carried(line: LineId): ItemId = s"line:${LineId.value(line)}"
  }

  /** One item, as the events so far make it: its `name` and `summary` once described, and the
    * `turns` whose messages are placed in it, in order. Only [[Agenda.fold]] makes one.
    */
  final case class Item private[agenda] (
      id: ItemId,
      name: Option[String],
      summary: Option[String],
      turns: Vector[TurnSeq]
  ) {

    /** Its name, or [[Item.Unnamed]] until it has one. */
    def shown: String = name.getOrElse(Item.Unnamed)
  }

  object Item {

    /** How an item not yet named is shown. */
    val Unnamed = "new topic"
  }

  /** Something that happened to a conversation's agenda. Recorded, never rewritten: the agenda
    * as it stands is a fold over every event so far ([[Agenda.fold]]).
    */
  enum Event {

    /** A new item, with no name until it is [[Described]]. */
    case Opened(item: ItemId)

    /** The user's message that started `turn`, placed `by` someone. A turn placed again is
      * where its latest placement puts it; the earlier ones stay on record.
      */
    case Placed(turn: TurnSeq, weights: Weights, by: Placement)

    /** `item`'s name (a few words) and a one-line summary of it, as of now. */
    case Described(item: ItemId, name: String, summary: String)
  }

  /** What the main model said a message is about. */
  enum Verdict {

    /** The current item. */
    case Current

    /** The earlier item it called `name`. */
    case Earlier(name: String)

    /** A new item, which it may have suggested a `name` for. */
    case New(name: Option[String])

    /** Nothing readable: `why` says what it sent instead. */
    case Unreadable(why: String)
  }

  val empty: Agenda = Agenda(Vector.empty, Map.empty, Vector.empty)

  /** An item carried by a closing's balance: its `id` ([[ItemId.carried]]), `name`, and
    * one-line `summary` as it was last described, if it ever was.
    */
  final case class Carried(id: ItemId, name: String, summary: Option[String])

  /** The agenda that `carried` (most recently spoken in first), then `events` (in the order
    * they were recorded), leave. A carried item keeps its name, and its summary until it is
    * described again. It ranks as recent as the latest turn placed in it, and below every item
    * with a turn, in `carried`'s order, until one is.
    */
  def fold(carried: Vector[Carried], events: Vector[Event]): Agenda = {
    val carriedIds = carried.map(_.id).distinct
    val opened =
      events.collect { case Event.Opened(t) => t }.distinct.filterNot(carriedIds.contains)
    val placements = events
      .collect { case p: Event.Placed => p }
      .groupBy(_.turn)
    val latest: Map[TurnSeq, ItemId] =
      placements.flatMap((turn, ps) => ps.lastOption.map(p => turn -> p.weights.heaviest))
    val described = events.collect { case d: Event.Described => d }
    def item(id: ItemId, name: Option[String], summary: Option[String]): Item = {
      val turns =
        latest.collect { case (turn, t) if t == id => turn }.toVector.sortBy(TurnSeq.value)
      val about = described.filter(_.item == id).lastOption
      Item(id, about.map(_.name).orElse(name), about.map(_.summary).orElse(summary), turns)
    }
    // Ties on the latest turn (none: -1) go to the higher rank: opened items newest first,
    // then carried ones in carried order.
    val ranked =
      carried
        .distinctBy(_.id)
        .zipWithIndex
        .map((c, i) => (item(c.id, Some(c.name), c.summary), -1 - i)) ++
        opened.zipWithIndex.map((id, i) => (item(id, None, None), i))
    val ordered = ranked
      .sortBy { (t, rank) =>
        (t.turns.lastOption.fold(-1L)(TurnSeq.value), rank)
      }
      .reverse
      .map(_._1)
    Agenda(ordered, placements, carriedIds)
  }
}
