package grit.core.topic

import grit.core.id.TurnSeq

/** One topic, as the events so far make it: its `name` and `summary` once described, and
  * the `turns` whose messages are placed in it, in order.
  */
final case class Topic(
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

/** A conversation's topics as they stand: every topic, most recently spoken in first, and
  * every placement of each turn's message, in the order they were made.
  */
final case class Topics(
    topics: Vector[Topic],
    placements: Map[TurnSeq, Vector[TopicEvent.Placed]]
) {

  /** The topic the latest placed message is in; `None` before any is placed. */
  def current: Option[Topic] =
    placements.keys.maxByOption(TurnSeq.value).flatMap(placed).flatMap(get)

  /** Every topic but the current one, most recent first. */
  def earlier: Vector[Topic] = current.fold(topics)(c => topics.filterNot(_.id == c.id))

  /** The topic `turn`'s message is in: the one its latest placement weighs highest. */
  def placed(turn: TurnSeq): Option[TopicId] =
    placements.get(turn).flatMap(_.lastOption).flatMap(Topics.top)

  /** The topic with `id`, if it was opened. */
  def get(id: TopicId): Option[Topic] = topics.find(_.id == id)
}

object Topics {

  val empty: Topics = Topics(Vector.empty, Map.empty)

  /** The topics `events` leave, in the order they were recorded. A topic is as recent as
    * the latest turn placed in it; one with no turn in it comes after all that have one,
    * and topics equally recent come newest opened first.
    */
  def fold(events: Vector[TopicEvent]): Topics = {
    val opened = events.collect { case TopicEvent.Opened(t) => t }.distinct
    val placements = events
      .collect { case p: TopicEvent.Placed => p }
      .groupBy(_.turn)
    val latest: Map[TurnSeq, TopicId] =
      placements.flatMap((turn, ps) => ps.lastOption.flatMap(top).map(turn -> _))
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

  /** The topic `placed` weighs highest, the first on a tie; `None` when it weighs none. */
  def top(placed: TopicEvent.Placed): Option[TopicId] =
    placed.weights
      .foldLeft(Option.empty[(TopicId, Double)]) {
        case (None, tw) => Some(tw)
        case (Some(best), tw) => Some(if (tw._2 > best._2) tw else best)
      }
      .map(_._1)

  /** Distinct keys for `topics`, in order: each one's shown name, and a number after it
    * when an earlier topic in `topics` is shown the same.
    */
  def keys(topics: Vector[Topic]): Vector[(Topic, String)] =
    topics.zipWithIndex.map { (t, i) =>
      val before = topics.take(i).count(_.shown == t.shown)
      t -> (if (before == 0) t.shown else s"${t.shown} (${before + 1})")
    }
}

/** The weights of a placement, from what placed it ([[TopicEvent.Placed]]). */
object Weights {

  /** Wholly in `topic`. */
  def whole(topic: TopicId): (Vector[(TopicId, Double)], Double) = (Vector(topic -> 1.0), 0.0)

  /** In `current` by `pSame`; the rest elsewhere. */
  def same(current: TopicId, pSame: Double): (Vector[(TopicId, Double)], Double) = {
    val p = clamp(pSame)
    (Vector(current -> p), 1 - p)
  }

  /** In `current` by `pSame`, and the rest shared as `choice` says among the topics it
    * names; its share for a new topic goes to `opened` when one was, and elsewhere when
    * none was.
    */
  def changed(
      current: TopicId,
      pSame: Double,
      choice: Vector[(Option[TopicId], Double)],
      opened: Option[TopicId]
  ): (Vector[(TopicId, Double)], Double) = {
    val p = clamp(pSame)
    val total = choice.map(c => math.max(0.0, c._2)).sum
    val share =
      choice.map((t, q) => (t, if (total > 0) (1 - p) * math.max(0.0, q) / total else 0.0))
    val named = share.flatMap((t, q) => t.orElse(opened).map(_ -> q))
    val lost = share.collect { case (None, q) if opened.isEmpty => q }.sum
    val merged = (Vector(current -> p) ++ named)
      .groupMapReduce(_._1)(_._2)(_ + _)
    val order = (current +: named.map(_._1)).distinct
    (order.flatMap(t => merged.get(t).map(t -> _)), lost + (if (total > 0) 0.0 else 1 - p))
  }

  private def clamp(p: Double): Double = if (p.isNaN) 0.0 else math.min(1.0, math.max(0.0, p))
}
