package grit.app.chat

import grit.core.id.TurnSeq
import grit.core.store.{Entry, EntryTopics}
import grit.core.topic.{Band, Placement, TopicId, Topics, Verdict}

/** The conversation's topics, as the panel's topics tab shows them: each topic, most
  * recently spoken in first, and how one turn's message was placed. Pure data, built by
  * [[TopicsView.of]] from the conversation's entries.
  *
  * @param topics every topic, most recent first
  * @param placing how the shown turn's message was placed, once it was
  */
final case class TopicsView(topics: Vector[TopicsView.Row], placing: Option[TopicsView.Placing])

object TopicsView {

  /** A topic: its `name` as shown, how many `messages` are in it, and whether it is the
    * `current` one.
    */
  final case class Row(name: String, messages: Int, current: Boolean)

  /** How the message of `turn` was placed.
    *
    * @param first it opened the conversation's first topic, and nothing was asked
    * @param unclassified the classifier did not answer, and why
    * @param pSame the classifier's p(same), and its `band`
    * @param choice the classifier's choice among the earlier topics and a new one, by name,
    *   when it was asked (most probable first)
    * @param verdict the main model's, when it was asked, and what went wrong asking
    * @param weights the heaviest topics of the last placement, by name, most first (at
    *   most three), and the part `elsewhere`
    * @param placed the topic it is in, by name
    * @param disagree the classifier leant one way (p(same) at or above one half) and the
    *   model said the other
    */
  final case class Placing(
      turn: TurnSeq,
      first: Boolean,
      unclassified: Option[String],
      pSame: Option[Double],
      band: Option[Band],
      choice: Vector[(String, Double)],
      verdict: Option[Verdict],
      anomaly: Option[String],
      weights: Vector[(String, Double)],
      elsewhere: Double,
      placed: Option[String],
      disagree: Boolean
  )

  /** How a new topic is named among a choice's options. */
  val NewOption = "something new"

  /** The most weights a placing lists. */
  val Weights = 3

  /** The topics `entries` record as they stood through `turn` (the newest turn, without
    * one): those the closing before its period carried, and those its period spoke in
    * ([[EntryTopics]]); and the placing of `turn`'s message (the latest placed turn's,
    * without one).
    */
  def of(entries: Vector[Entry], turn: Option[TurnSeq]): TopicsView = {
    val through = turn.orElse(entries.map(_.turnSeq).maxByOption(TurnSeq.value))
    val topics = through.fold(Topics.empty)(EntryTopics.through(entries, _))
    val current = topics.current.map(_.id)
    def name(id: TopicId): String = topics.get(id).fold(TopicId.value(id))(_.shown)
    val rows = topics.topics.map(t => Row(t.shown, t.turns.size, current.contains(t.id)))
    val shown = turn
      .filter(topics.placements.contains)
      .orElse(topics.placements.keys.maxByOption(TurnSeq.value))
    TopicsView(rows, shown.flatMap(t => placing(t, topics, name)))
  }

  private def placing(
      turn: TurnSeq,
      topics: Topics,
      name: TopicId => String
  ): Option[Placing] =
    topics.placements.get(turn).flatMap(_.lastOption).map { last =>
      val all = topics.placements.getOrElse(turn, Vector.empty).map(_.by)
      val classified = all.collectFirst { case c: Placement.Classified => c }
      val asked = all.collectFirst { case a: Placement.Asked => a }
      val disagree = classified.zip(asked).exists { (c, a) =>
        a.verdict match {
          case Verdict.Unreadable(_) => false
          case v => (c.pSame >= 0.5) != (v == Verdict.Current)
        }
      }
      Placing(
        turn = turn,
        first = all.contains(Placement.First),
        unclassified = all.collectFirst { case Placement.Unclassified(r) => r },
        pSame = classified.map(_.pSame),
        band = classified.map(_.outcome.band),
        choice = classified.toVector
          .flatMap {
            _.outcome match {
              case Placement.Outcome.Changed(choice) => choice
              case Placement.Outcome.Same | Placement.Outcome.Uncertain => Vector.empty
            }
          }
          .map(c => (c.topic.fold(NewOption)(name), c.probability))
          .sortBy(-_._2),
        verdict = asked.map(_.verdict),
        anomaly = asked.flatMap(_.anomaly),
        weights = last.weights.byTopic
          .sortBy(-_.weight)
          .take(Weights)
          .map(s => (name(s.topic), s.weight)),
        elsewhere = last.weights.elsewhere,
        placed = Some(name(last.weights.heaviest)),
        disagree = disagree
      )
    }
}
