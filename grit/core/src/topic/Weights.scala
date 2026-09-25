package grit.core.topic

/** How much a placed message belongs to each topic it could name, and the part `elsewhere`
  * that belongs to none. Every share is non-negative and together they sum to 1; there is
  * always a topic. Only [[Weights]]' constructors make one.
  */
final case class Weights private (
    lead: Weights.Share,
    others: Vector[Weights.Share],
    elsewhere: Double
) {

  /** Each topic once, in the order it was weighed. */
  def byTopic: Vector[Weights.Share] = lead +: others

  /** The topic the message is in: the heaviest, the first on a tie, whatever `elsewhere`
    * holds.
    */
  def heaviest: TopicId = byTopic.maxByOption(_.weight).fold(lead.topic)(_.topic)
}

object Weights {

  /** `topic`'s share of a message. */
  final case class Share(topic: TopicId, weight: Double)

  /** Wholly in `topic`. */
  def whole(topic: TopicId): Weights = Weights(Share(topic, 1.0), Vector.empty, 0.0)

  /** In `current` by `pSame` (clamped to [0, 1], NaN as 0); the rest elsewhere. */
  def same(current: TopicId, pSame: Double): Weights = {
    val p = clamp(pSame)
    Weights(Share(current, p), Vector.empty, 1 - p)
  }

  /** In `current` by `pSame` (clamped as in [[same]]), the rest shared in proportion to
    * `choice` (a negative or non-finite probability counts as 0), a topic named twice adding
    * up. The share of a new topic (`None`) goes to `opened` when there is one, else
    * elsewhere; with no probability in `choice`, all the rest is elsewhere. Order: `current`,
    * then `choice`'s topics as first named.
    */
  def changed(
      current: TopicId,
      pSame: Double,
      choice: Vector[Placement.Chance],
      opened: Option[TopicId]
  ): Weights = {
    val p = clamp(pSame)
    val total = choice.map(c => sane(c.probability)).sum
    val share =
      choice.map(c => (c.topic, if (total > 0) (1 - p) * sane(c.probability) / total else 0.0))
    val named = share.flatMap((t, q) => t.orElse(opened).map(Share(_, q)))
    val lost =
      if (total > 0) share.collect { case (None, q) if opened.isEmpty => q }.sum else 1 - p
    val merged = (Share(current, p) +: named).groupMapReduce(_.topic)(_.weight)(_ + _)
    val order = (current +: named.map(_.topic)).distinct
    Weights(
      Share(current, merged.getOrElse(current, p)),
      order.drop(1).map(t => Share(t, merged.getOrElse(t, 0.0))),
      lost
    )
  }

  /** These shares as weights, or why they are none. */
  def of(byTopic: Vector[Share], elsewhere: Double): Either[WeightsError, Weights] =
    byTopic match {
      case lead +: others =>
        val topics = byTopic.map(_.topic)
        val sum = byTopic.map(_.weight).sum + elsewhere
        topics
          .diff(topics.distinct)
          .headOption
          .map(WeightsError.Repeated(_))
          .orElse(byTopic.collectFirst {
            case s if !s.weight.isFinite => WeightsError.NotFinite(Some(s.topic))
            case s if s.weight < 0 => WeightsError.Negative(Some(s.topic))
          })
          .orElse(Option.when(!elsewhere.isFinite)(WeightsError.NotFinite(None)))
          .orElse(Option.when(elsewhere < 0)(WeightsError.Negative(None)))
          .orElse(Option.when(math.abs(sum - 1) > Tolerance)(WeightsError.SumOff(sum)))
          .toLeft(Weights(lead, others, elsewhere))
      case _ => Left(WeightsError.NoTopic)
    }

  /** How far from 1 the shares of [[of]] may sum. */
  val Tolerance = 1e-9

  private def clamp(p: Double): Double = if (p.isNaN) 0.0 else math.min(1.0, math.max(0.0, p))

  private def sane(q: Double): Double = if (q.isFinite && q > 0) q else 0.0
}

/** Why shares are not [[Weights]]. */
enum WeightsError {

  /** No topic has a share. */
  case NoTopic

  /** `topic` has more than one share. */
  case Repeated(topic: TopicId)

  /** The share of `topic`, or of elsewhere (`None`), is below 0. */
  case Negative(topic: Option[TopicId])

  /** The share of `topic`, or of elsewhere (`None`), is NaN or infinite. */
  case NotFinite(topic: Option[TopicId])

  /** The shares sum to `sum`, further from 1 than [[Weights.Tolerance]]. */
  case SumOff(sum: Double)
}
