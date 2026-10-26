package grit.core.screen

import grit.core.period.Probability

/** A screen's answers about one text, each its probability of yes, in its shape's order: the
  * floor's, then the signals'.
  */
final case class Screened private[screen] (
    shape: Screen.Shape,
    floor: Vector[Probability],
    signals: Vector[Probability]
) extends caps.Pure {

  /** Whether any floor answer is at or above [[Screen.Infers]]: then [[fired]] needs a
    * baseline.
    */
  def floorFires: Boolean = floor.exists(_ >= Screen.Infers)

  /** Each failing position: a floor answer at or above [[Screen.Infers]] whose `baseline`
    * answer is below it (every such one when `baseline` is `None`), an absent answer at or above
    * Infers, a present one below [[Screen.States]]. The text passes when it is empty.
    */
  def fired(baseline: Option[Screen.Baseline]): Fired = {
    val signal = signals.zip(shape.musts).zipWithIndex
    Fired(
      unexcused(baseline).collect { case (p, at) if p >= Screen.Infers => at },
      signal.collect { case ((p, Must.Absent), at) if p >= Screen.Infers => at },
      signal.collect { case ((p, Must.Present), at) if !(p >= Screen.States) => at }
    )
  }

  /** The highest absent answer or floor answer `baseline` does not excuse; 0 when there is
    * none.
    */
  def judged(baseline: Option[Screen.Baseline]): Probability = {
    val absent = signals.zip(shape.musts).collect { case (p, Must.Absent) => p }
    (unexcused(baseline).map(_._1) ++ absent)
      .maxByOption(Probability.value)
      .getOrElse(Probability.Zero)
  }

  /** The floor answers, with their positions, that `baseline` does not answer at or above
    * Infers.
    */
  private def unexcused(baseline: Option[Screen.Baseline]): Vector[(Probability, Int)] =
    floor.zipWithIndex.filterNot { (_, at) =>
      baseline.exists(_.floor.lift(at).exists(_ >= Screen.Infers))
    }
}

/** A screen's failing positions, by kind, each in order: `floor` indexes its floor's answers,
  * `absent` and `present` its signals' (from 0).
  */
final case class Fired private[screen] (
    floor: Vector[Int],
    absent: Vector[Int],
    present: Vector[Int]
) extends caps.Pure {
  def passes: Boolean = floor.isEmpty && absent.isEmpty && present.isEmpty
}
