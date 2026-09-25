package grit.core.topic

import grit.core.id.TurnSeq

/** One topic, as the events so far make it: its `name` and `summary` once described, and
  * the `turns` whose messages are placed in it, in order. Only [[Topics.fold]] makes one.
  */
final case class Topic private[topic] (
    id: TopicId,
    name: Option[String],
    summary: Option[String],
    turns: Vector[TurnSeq]
) {

  /** Its name, or [[Topic.Unnamed]] until it has one. */
  def shown: String = name.getOrElse(Topic.Unnamed)
}

object Topic {

  /** How a topic not yet named is shown. */
  val Unnamed = "new topic"
}

/** A conversation's topics as they stand. Only [[Topics.fold]] makes one.
  *
  * @param topics
  *   every topic, most recently spoken in first. A topic is as recent as the latest turn
  *   placed in it; one with no turn in it comes after all that have one, and topics equally
  *   recent come newest opened first.
  * @param placements
  *   every placement of each turn's message, in the order they were made.
  */
final case class Topics private (
    topics: Vector[Topic],
    placements: Map[TurnSeq, Vector[TopicEvent.Placed]]
) {

  /** The topic the latest placed message is in; `None` before any is placed, or when it is
    * placed in a topic never opened.
    */
  def current: Option[Topic] =
    placements.keys.maxByOption(TurnSeq.value).flatMap(placed).flatMap(get)

  /** Every topic but the current one, most recent first. */
  def earlier: Vector[Topic] = current.fold(topics)(c => topics.filterNot(_.id == c.id))

  /** The topic `turn`'s latest placement puts its message in; `None` if it was never placed. */
  def placed(turn: TurnSeq): Option[TopicId] =
    placements.get(turn).flatMap(_.lastOption).map(_.weights.heaviest)

  /** The topic with `id`, if it was opened. */
  def get(id: TopicId): Option[Topic] = topics.find(_.id == id)
}

object Topics {

  val empty: Topics = Topics(Vector.empty, Map.empty)

  /** The topics `events`, in the order they were recorded, leave. */
  def fold(events: Vector[TopicEvent]): Topics = {
    val opened = events.collect { case TopicEvent.Opened(t) => t }.distinct
    val placements = events
      .collect { case p: TopicEvent.Placed => p }
      .groupBy(_.turn)
    val latest: Map[TurnSeq, TopicId] =
      placements.flatMap((turn, ps) => ps.lastOption.map(p => turn -> p.weights.heaviest))
    val described = events.collect { case d: TopicEvent.Described => d }
    val topics = opened.zipWithIndex.map { (id, i) =>
      val turns =
        latest.collect { case (turn, t) if t == id => turn }.toVector.sortBy(TurnSeq.value)
      val about = described.filter(_.topic == id).lastOption
      (Topic(id, about.map(_.name), about.map(_.summary), turns), i)
    }
    val ordered = topics
      .sortBy { (t, i) =>
        (t.turns.lastOption.fold(-1L)(TurnSeq.value), i)
      }
      .reverse
      .map(_._1)
    Topics(ordered, placements)
  }
}
