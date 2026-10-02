package grit.eval.harness.stats

/** A mean over `n` items with its standard error clustered (CR1: the sandwich over cluster sums
  * of residuals, scaled by g/(g−1)), the items in `g` clusters, at least two.
  */
final case class Estimate private (mean: Double, se: Double, n: Int, g: Int) {

  /** 95%, Student's t on g−1 degrees of freedom. */
  def interval: (Double, Double) = {
    val half = Student.t975(g - 1) * se
    (mean - half, mean + half)
  }

  /** The least true difference a paired comparison of this precision detects 80% of the time
    * at two-sided 5%: (t.975 + t.80)·se, on g−1 degrees of freedom.
    */
  def mde: Double = (Student.t975(g - 1) + Student.t80(g - 1)) * se
}

object Estimate {

  /** The mean of `scores`, each item weighed alike, clustered by their key; `None` with fewer
    * than two clusters, where the standard error is undefined.
    */
  def clustered[K](scores: Vector[(K, Double)]): Option[Estimate] = {
    val n = scores.size
    val groups = scores.groupMap(_._1)(_._2).values.toVector
    val g = groups.size
    Option.when(g >= 2) {
      val mean = scores.map(_._2).sum / n
      val sums = groups.map(_.map(_ - mean).sum)
      val se = math.sqrt(g.toDouble / (g - 1) * sums.map(s => s * s).sum) / n
      new Estimate(mean, se, n, g)
    }
  }

  /** The mean of `b`'s value less `a`'s over the items both hold, clustered by `cluster`;
    * `None` with fewer than two clusters among them.
    */
  def paired[I, K](a: Map[I, Double], b: Map[I, Double], cluster: I => K): Option[Estimate] =
    clustered(b.toVector.flatMap((i, y) => a.get(i).map(x => cluster(i) -> (y - x))))
}
