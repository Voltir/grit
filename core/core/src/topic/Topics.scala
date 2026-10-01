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
    placements: Map[TurnSeq, Vector[TopicEvent.Placed]],
    private val carried: Vector[TopicId]
) {

  /** The topic the latest placed message is in. Before any is placed, the first carried
    * topic. `None` when there is neither, or when the latest message was placed in a topic
    * neither opened nor carried.
    */
  def current: Option[Topic] =
    placements.keys.maxByOption(TurnSeq.value) match {
      case Some(latest) => placed(latest).flatMap(get)
      case None => carried.headOption.flatMap(get)
    }

  /** Every topic but the current one, most recent first. */
  def earlier: Vector[Topic] = current.fold(topics)(c => topics.filterNot(_.id == c.id))

  /** The topic `turn`'s latest placement puts its message in; `None` if it was never placed. */
  def placed(turn: TurnSeq): Option[TopicId] =
    placements.get(turn).flatMap(_.lastOption).map(_.weights.heaviest)

  /** The topic with `id`, if it was opened. */
  def get(id: TopicId): Option[Topic] = topics.find(_.id == id)
}

object Topics {

  val empty: Topics = Topics(Vector.empty, Map.empty, Vector.empty)

  /** A topic carried by a closing's balance: its `id` ([[TopicId.carried]]), `name`, and
    * one-line `summary` as it was last described, if it ever was.
    */
  final case class Carried(id: TopicId, name: String, summary: Option[String])

  /** The topics that `carried` (most recently spoken in first), then `events` (in the order
    * they were recorded), leave. A carried topic keeps its name, and its summary until it is
    * described again. It ranks as recent as the latest turn placed in it, and below every topic
    * with a turn, in `carried`'s order, until one is.
    */
  def fold(carried: Vector[Carried], events: Vector[TopicEvent]): Topics = {
    val carriedIds = carried.map(_.id).distinct
    val opened =
      events.collect { case TopicEvent.Opened(t) => t }.distinct.filterNot(carriedIds.contains)
    val placements = events
      .collect { case p: TopicEvent.Placed => p }
      .groupBy(_.turn)
    val latest: Map[TurnSeq, TopicId] =
      placements.flatMap((turn, ps) => ps.lastOption.map(p => turn -> p.weights.heaviest))
    val described = events.collect { case d: TopicEvent.Described => d }
    def topic(id: TopicId, name: Option[String], summary: Option[String]): Topic = {
      val turns =
        latest.collect { case (turn, t) if t == id => turn }.toVector.sortBy(TurnSeq.value)
      val about = described.filter(_.topic == id).lastOption
      Topic(id, about.map(_.name).orElse(name), about.map(_.summary).orElse(summary), turns)
    }
    // Ties on the latest turn (none: -1) go to the higher rank: opened topics newest first,
    // then carried ones in carried order.
    val ranked =
      carried
        .distinctBy(_.id)
        .zipWithIndex
        .map((c, i) => (topic(c.id, Some(c.name), c.summary), -1 - i)) ++
        opened.zipWithIndex.map((id, i) => (topic(id, None, None), i))
    val ordered = ranked
      .sortBy { (t, rank) =>
        (t.turns.lastOption.fold(-1L)(TurnSeq.value), rank)
      }
      .reverse
      .map(_._1)
    Topics(ordered, placements, carriedIds)
  }
}
