package grit.eval.harness.score

import java.time.LocalDate

import grit.eval.harness.stats.Proportion

/** A topics run's placement of a room's openings set against the room's truth, by message ids.
  * Pairs are of openings with a truth topic (chatter belongs to none and pairs with nothing):
  * `precision`, of the pairs sharing a topic the run wrote, those of one truth topic, clustered
  * by the run's topic; `recall`, of the pairs of one truth topic, those sharing a topic the run
  * wrote, clustered by truth topic. `splits`, the truth topics whose openings lie in two topics
  * or more; `merges`, the run's topics holding openings of two truth topics or more;
  * `mergeable`, the pairs of the run's topics whose most-held truth topic is the same (the
  * greater name of two held as often), which one topic should have held; `crossDay`, the truth
  * topics two of whose placed openings, said on different days, share no topic: a day's pass
  * that opened a topic where it should have grown one; `unplaced`, the openings with a truth
  * topic in no topic of the run's.
  */
final case class TopicScore(
    precision: Proportion,
    recall: Proportion,
    splits: Int,
    merges: Int,
    mergeable: Int,
    crossDay: Int,
    unplaced: Int
)

object TopicScore {

  /** An opening as the truth has it: its `id`, its `topic` (`None` for chatter), and the `day`
    * it was said.
    */
  final case class Truth(id: String, topic: Option[String], day: LocalDate)

  /** `truth`'s openings placed as `placed` says: each opening's id with the run's topics that
    * hold it (an opening it names in none, or does not name, is in none).
    */
  def of(truth: Vector[Truth], placed: Map[String, Set[String]]): TopicScore = {
    val topical = truth.collect { case Truth(id, Some(t), day) => (id, t, day) }
    def in(id: String): Set[String] = placed.getOrElse(id, Set.empty)
    def pairsOf[A](as: Vector[A]): Vector[(A, A)] =
      as.zipWithIndex.flatMap((a, i) => as.drop(i + 1).map(a -> _))
    val pairs = pairsOf(topical)
    val precision = Proportion.of(
      pairs.flatMap((a, b) => (in(a._1) intersect in(b._1)).toVector.map(_ -> (a._2 == b._2)))
    )
    val recall = Proportion.of(
      pairs.collect {
        case (a, b) if a._2 == b._2 => a._2 -> (in(a._1) intersect in(b._1)).nonEmpty
      }
    )
    val byTruth = topical.groupBy(_._2)
    val splits = byTruth.count((_, os) => os.flatMap(o => in(o._1)).distinct.size > 1)
    val topics = topical.flatMap(o => in(o._1).toVector.map(_ -> o._2)).groupBy(_._1)
    val merges = topics.count((_, held) => held.map(_._2).distinct.size > 1)
    val most = topics.toVector.flatMap((k: String, held: Vector[(String, String)]) =>
      held
        .map(_._2)
        .groupMapReduce(identity)(_ => 1)(_ + _)
        .toVector
        .sortBy((t, n) => (n, t))
        .lastOption
        .map(k -> _._1)
    )
    val mergeable = most.groupBy(_._2).values.map(ks => ks.size * (ks.size - 1) / 2).sum
    val crossDay = byTruth.count((_, os) =>
      pairsOf(os.filter(o => in(o._1).nonEmpty)).exists((a, b) =>
        a._3 != b._3 && (in(a._1) intersect in(b._1)).isEmpty
      )
    )
    TopicScore(
      precision,
      recall,
      splits,
      merges,
      mergeable,
      crossDay,
      topical.count(o => in(o._1).isEmpty)
    )
  }
}
