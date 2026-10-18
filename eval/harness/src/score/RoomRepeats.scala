package grit.eval.harness.score

import grit.eval.harness.stats.Proportion

/** A number over repeats: its mean, least and most. */
final case class Ranged(mean: Double, min: Double, max: Double)

object Ranged {

  /** `xs`' mean, least and most; `None` when there are none. */
  def of(xs: Vector[Double]): Option[Ranged] =
    for {
      min <- xs.minOption
      max <- xs.maxOption
    } yield Ranged(xs.sum / xs.size, min, max)
}

/** One measure of a room's placing over repeats: `name`, its `count` over them, and, for a
  * proportion, the count it is out of (`of`).
  */
final case class RoomMeasure(name: String, count: Ranged, of: Option[Ranged])

object RoomRepeats {

  /** Each measure of `scores`, the same room's repeats, in the order a report gives them: right,
    * offer recall, strict recall, Jev given offered, the wrong follows, the topics split, the
    * strands mixed, and the bursts Jev placed right; none when `scores` is empty.
    */
  def of(scores: Vector[RoomScore]): Vector[RoomMeasure] = {
    def ranged(f: RoomScore => Double): Option[Ranged] = Ranged.of(scores.map(f))
    def proportion(name: String, p: RoomScore => Proportion): Option[RoomMeasure] =
      ranged(s => p(s).hits.toDouble).map(c => RoomMeasure(name, c, ranged(s => p(s).n.toDouble)))
    def count(name: String, n: RoomScore => Int): Option[RoomMeasure] =
      ranged(s => n(s).toDouble).map(RoomMeasure(name, _, None))
    Vector(
      proportion("right", _.right),
      proportion("offer recall", _.recall),
      proportion("strict recall", _.strict),
      proportion("Jev given offered", _.chosen),
      count("wrong follows", _.wrongFollows.size),
      count("topics split", _.split.size),
      count("strands mixed", _.mixed.size),
      ranged(_.bursts.count(_.right).toDouble).map(c =>
        RoomMeasure("bursts Jev right", c, ranged(_.bursts.size.toDouble))
      )
    ).flatten
  }
}
