package grit.eval.harness.score

import scala.collection.immutable.VectorMap

import grit.core.message.{Tokens, Usage}
import grit.eval.harness.capture.Spent

/** A model's prices in USD a token, read from its ledger rows: its uncached `input`, its
  * `cached` input, and its `output`; over `rows` priced rows, `cachedRows` of them with cached
  * tokens. `cached` is `None` where no row had a cached token to read it from. `worst`: the
  * largest share by which the rate misprices a row it was read from.
  */
final case class Rate(
    input: Double,
    cached: Option[Double],
    output: Double,
    rows: Int,
    cachedRows: Int,
    worst: Double
)

/** What a model's provider counts for each token grit estimates, over the `calls` it was
  * read from: tokens the provider counted ≈ `ratio` × grit's estimate. A fit, not a count.
  */
final case class Scale(ratio: Double, calls: Int)

/** What a capture's calls are priced with: each model's [[Rate]], and the [[Scale]] that turns
  * grit's token estimates into the provider's tokens; each `Left` with why a model has none.
  */
final case class Prices(
    rates: VectorMap[String, Either[String, Rate]],
    scales: VectorMap[String, Either[String, Scale]]
)

object Prices {

  /** The rates ([[Rates.fit]]) and scales ([[Rates.scales]]) of `spend`'s models. */
  def of(spend: Vector[Spent]): Prices = Prices(Rates.fit(spend), Rates.scales(spend))
}

/** Each model's [[Rate]] and [[Scale]], derived from the ledger, since grit records no price
  * and estimates its tokens.
  */
object Rates {

  /** The fewest calls a model's [[Scale]] is read from. */
  val MinCalls: Int = 5

  /** Each model of `spend`'s, in the order first met, read by least squares through 0 over
    * its calls that grit estimated and the provider counted input for (both above 0): input
    * tokens = ratio × estimated tokens. `Left` with why for a model with fewer than
    * [[MinCalls]] such calls.
    */
  def scales(spend: Vector[Spent]): VectorMap[String, Either[String, Scale]] =
    VectorMap.from(spend.map(_.model).distinct.map { m =>
      val pairs = spend
        .filter(s => s.model == m)
        .map(s => (Tokens.value(s.estimated).toDouble, Tokens.value(s.usage.input).toDouble))
        .filter((e, a) => e > 0 && a > 0)
      m -> Either.cond(
        pairs.size >= MinCalls,
        Scale(pairs.map(_ * _).sum / pairs.map((e, _) => e * e).sum, pairs.size),
        s"${pairs.size} call${if (pairs.size == 1) "" else "s"} estimated and counted, fewer " +
          s"than the $MinCalls a scale is read from"
      )
    })

  /** Each model of `spend`'s, in the order first met, read by least squares from its priced
    * rows as cost = input·(input tokens − cached) + cached·(cached tokens) + output·(output
    * tokens). `Left` with why for a model whose rows cannot give one: fewer priced rows than
    * prices to read, rows that do not tell the prices apart, or a price read below 0.
    */
  def fit(spend: Vector[Spent]): VectorMap[String, Either[String, Rate]] =
    VectorMap.from(spend.map(_.model).distinct.map(m => m -> of(spend.filter(_.model == m))))

  private def of(rows: Vector[Spent]): Either[String, Rate] = {
    val priced = rows.flatMap(s => s.usage.costUsd.map(c => s.usage -> c.toDouble))
    def n(t: Tokens) = Tokens.value(t).toDouble
    val cachedRows = priced.count((u, _) => Tokens.value(u.cachedInput) > 0)
    // With no row cached, the cached price has nothing to be read from and is left out.
    def x(u: Usage): Vector[Double] =
      if (cachedRows > 0) Vector(n(u.input) - n(u.cachedInput), n(u.cachedInput), n(u.output))
      else Vector(n(u.input), n(u.output))
    val k = if (cachedRows > 0) 3 else 2
    for {
      _ <- Either.cond(
        priced.size >= k,
        (),
        s"${priced.size} priced row${if (priced.size == 1) "" else "s"}, fewer than the $k " +
          "prices read from them"
      )
      b <- solve(priced.map((u, _) => x(u)), priced.map(_._2))
        .toRight("its rows do not tell the prices apart")
      _ <- Either.cond(b.forall(_ >= 0), (), "a price read below 0")
    } yield {
      val worst = priced
        .collect { case (u, c) if c > 0 => math.abs(x(u).zip(b).map(_ * _).sum - c) / c }
        .maxOption
        .getOrElse(0.0)
      if (cachedRows > 0) Rate(b(0), Some(b(1)), b(2), priced.size, cachedRows, worst)
      else Rate(b(0), None, b(1), priced.size, 0, worst)
    }
  }

  /** The least-squares solution of `xs`·b = `ys`, by the normal equations solved by
    * Gauss–Jordan elimination with partial pivoting; `None` when they are singular (a pivot
    * under 1e-12 of the largest diagonal) or there is nothing to solve.
    */
  private def solve(xs: Vector[Vector[Double]], ys: Vector[Double]): Option[Vector[Double]] = {
    val k = xs.headOption.fold(0)(_.size)
    val normal: Vector[Vector[Double]] = Vector.tabulate(k, k + 1) { (i, j) =>
      if (j < k) xs.map(x => x(i) * x(j)).sum else xs.indices.map(r => xs(r)(i) * ys(r)).sum
    }
    val scale = normal.indices.map(i => math.abs(normal(i)(i))).maxOption.getOrElse(0.0)
    val start: Option[Vector[Vector[Double]]] = Option.when(k > 0)(normal)
    val reduced = (0 until k).foldLeft(start) { (done: Option[Vector[Vector[Double]]], c: Int) =>
      done.flatMap { (a: Vector[Vector[Double]]) =>
        (c until k).maxByOption(r => math.abs(a(r)(c))).flatMap { p =>
          val swapped: Vector[Vector[Double]] = a.updated(p, a(c)).updated(c, a(p))
          val pivot = swapped(c)(c)
          Option.when(math.abs(pivot) > scale * 1e-12)(swapped.indices.toVector.map { r =>
            val f = swapped(r)(c) / pivot
            if (r == c) swapped(r) else swapped(r).zip(swapped(c)).map((x, y) => x - f * y)
          })
        }
      }
    }
    reduced.map((a: Vector[Vector[Double]]) => a.indices.toVector.map(i => a(i)(k) / a(i)(i)))
  }
}
