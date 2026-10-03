package grit.eval.harness.stats

/** How many of `n` items were `hits`, over how many `clusters`, and the proportion's 95%
  * interval clustered by them.
  */
final case class Proportion(hits: Long, n: Long, clusters: Int, interval: Proportion.Interval) {

  /** `hits` over `n`; `None` when `n` is 0. */
  def rate: Option[Double] = Option.when(n > 0)(hits.toDouble / n)
}

object Proportion {

  /** The fewest clusters a proportion is given an interval over: a clustered standard error
    * rests on the clusters alone, and under about ten it says nothing.
    */
  val MinClusters: Int = 10

  /** A proportion's 95% interval, or why it has none. */
  enum Interval {

    /** Its items are in fewer than [[MinClusters]] clusters. */
    case TooFewClusters

    /** Wilson's score interval at 95% over `effective` items: `n` over the design effect, the
      * variance clustered (CR1) over the binomial one, taken as 1 where it is below 1 or where
      * every item hit or none did. Within [0, 1], `low` at most `high`.
      */
    case Wilson(low: Double, high: Double, effective: Double)
  }

  /** The proportion of `items` that hit, each its cluster and whether it hit. */
  def of[K](items: Vector[(K, Boolean)]): Proportion =
    counted(items.map((k, hit) => (k, if (hit) 1L else 0L, 1L)))

  /** [[of]] over items given by count: each entry `(cluster, hits, of)` stands for `of` items
    * of that cluster, `hits` of them hits (read as within 0 and `of`), such as a call's cached
    * input tokens of its input tokens. An entry of no items adds nothing, not even its cluster.
    */
  def counted[K](counts: Vector[(K, Long, Long)]): Proportion = {
    val items = counts.collect {
      case (k, hits, of) if of > 0 => (k, math.min(math.max(hits, 0L), of), of)
    }
    val hits = items.map(_._2).sum
    val n = items.map(_._3).sum
    val clusters =
      items.groupMapReduce(_._1)(i => (i._2, i._3))((x, y) => (x._1 + y._1, x._2 + y._2))
    val interval =
      if (clusters.size < MinClusters) Interval.TooFewClusters
      else {
        val p = hits.toDouble / n
        val binomial = p * (1 - p) / n
        // CR1 over the clusters' sums of residuals, as Estimate.clustered takes it of each item
        // scored 1 or 0: a cluster's sum is its hits less p of its items.
        val g = clusters.size.toDouble
        val sums = clusters.values.map((h, of) => h - p * of)
        val clustered = g / (g - 1) * sums.map(s => s * s).sum / (n.toDouble * n)
        val effect = if (binomial > 0) math.max(1.0, clustered / binomial) else 1.0
        wilson(hits, n, n / effect)
      }
    Proportion(hits, n, clusters.size, interval)
  }

  /** Wilson's 95% score interval for `hits` of `n` taken as `effective` items. */
  private def wilson(hits: Long, n: Long, effective: Double): Interval.Wilson = {
    val z = 1.96
    val p = hits.toDouble / n
    val z2 = z * z
    val scale = 1 + z2 / effective
    val centre = (p + z2 / (2 * effective)) / scale
    val half = z / scale * math.sqrt(p * (1 - p) / effective + z2 / (4 * effective * effective))
    // At a proportion of 0 or 1 the bound on that side is exactly it; computed, it can miss by
    // a rounding error.
    Interval.Wilson(
      if (hits == 0) 0.0 else centre - half,
      if (hits == n) 1.0 else centre + half,
      effective
    )
  }
}
