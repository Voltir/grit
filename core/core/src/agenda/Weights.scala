package grit.core.agenda

/** How much a placed message belongs to each item it could name, and the part `elsewhere`
  * that belongs to none. Every share is non-negative and together they sum to 1; there is
  * always an item. Only [[Weights]]' constructors make one.
  */
final case class Weights private (
    lead: Weights.Share,
    others: Vector[Weights.Share],
    elsewhere: Double
) {

  /** Each item once, in the order it was weighed. */
  def byItem: Vector[Weights.Share] = lead +: others

  /** The item the message is in: the heaviest, the first on a tie, whatever `elsewhere`
    * holds.
    */
  def heaviest: Agenda.ItemId = byItem.maxByOption(_.weight).fold(lead.item)(_.item)
}

object Weights {

  /** `item`'s share of a message. */
  final case class Share(item: Agenda.ItemId, weight: Double)

  /** Wholly in `item`. */
  def whole(item: Agenda.ItemId): Weights = Weights(Share(item, 1.0), Vector.empty, 0.0)

  /** In `current` by `pSame` (clamped to [0, 1], NaN as 0); the rest elsewhere. */
  def same(current: Agenda.ItemId, pSame: Double): Weights = {
    val p = clamp(pSame)
    Weights(Share(current, p), Vector.empty, 1 - p)
  }

  /** In `current` by `pSame` (clamped as in [[same]]), the rest shared in proportion to
    * `choice` (a negative or non-finite probability counts as 0), an item named twice adding
    * up. The share of a new item (`None`) goes to `opened` when there is one, else
    * elsewhere; with no probability in `choice`, all the rest is elsewhere. Order: `current`,
    * then `choice`'s items as first named.
    */
  def changed(
      current: Agenda.ItemId,
      pSame: Double,
      choice: Vector[Placement.Chance],
      opened: Option[Agenda.ItemId]
  ): Weights = {
    val p = clamp(pSame)
    val total = choice.map(c => sane(c.probability)).sum
    val share =
      choice.map(c => (c.item, if (total > 0) (1 - p) * sane(c.probability) / total else 0.0))
    val named = share.flatMap((t, q) => t.orElse(opened).map(Share(_, q)))
    val lost =
      if (total > 0) share.collect { case (None, q) if opened.isEmpty => q }.sum else 1 - p
    val merged = (Share(current, p) +: named).groupMapReduce(_.item)(_.weight)(_ + _)
    val order = (current +: named.map(_.item)).distinct
    Weights(
      Share(current, merged.getOrElse(current, p)),
      order.drop(1).map(t => Share(t, merged.getOrElse(t, 0.0))),
      lost
    )
  }

  /** These shares as weights, or why they are none. */
  def of(byItem: Vector[Share], elsewhere: Double): Either[WeightsError, Weights] =
    byItem match {
      case lead +: others =>
        val items = byItem.map(_.item)
        val sum = byItem.map(_.weight).sum + elsewhere
        items
          .diff(items.distinct)
          .headOption
          .map(WeightsError.Repeated(_))
          .orElse(byItem.collectFirst {
            case s if !s.weight.isFinite => WeightsError.NotFinite(Some(s.item))
            case s if s.weight < 0 => WeightsError.Negative(Some(s.item))
          })
          .orElse(Option.when(!elsewhere.isFinite)(WeightsError.NotFinite(None)))
          .orElse(Option.when(elsewhere < 0)(WeightsError.Negative(None)))
          .orElse(Option.when(math.abs(sum - 1) > Tolerance)(WeightsError.SumOff(sum)))
          .toLeft(Weights(lead, others, elsewhere))
      case _ => Left(WeightsError.NoItem)
    }

  /** How far from 1 the shares of [[of]] may sum. */
  val Tolerance = 1e-9

  private def clamp(p: Double): Double = if (p.isNaN) 0.0 else math.min(1.0, math.max(0.0, p))

  private def sane(q: Double): Double = if (q.isFinite && q > 0) q else 0.0
}

/** Why shares are not [[Weights]]. */
enum WeightsError {

  /** No item has a share. */
  case NoItem

  /** `item` has more than one share. */
  case Repeated(item: Agenda.ItemId)

  /** The share of `item`, or of elsewhere (`None`), is below 0. */
  case Negative(item: Option[Agenda.ItemId])

  /** The share of `item`, or of elsewhere (`None`), is NaN or infinite. */
  case NotFinite(item: Option[Agenda.ItemId])

  /** The shares sum to `sum`, further from 1 than [[Weights.Tolerance]]. */
  case SumOff(sum: Double)
}
