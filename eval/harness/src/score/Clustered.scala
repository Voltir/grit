package grit.eval.harness.score

import grit.eval.harness.corpus.{Case, CaseId, Digest}
import grit.eval.harness.label.{Context, Labels}
import grit.eval.harness.stats.{Estimate, Proportion}

/** A mean over `n` cases clustered twice: by the exchange each belongs to
  * ([[grit.eval.harness.corpus.Clusters.exchange]]), and by its author, a case with no enrolled
  * author alone in its cluster; each `None` under two clusters.
  */
final case class Clustered(n: Int, exchange: Option[Estimate], author: Option[Estimate])

object Clustered {

  /** The mean of each case's value. */
  def of(values: Vector[(Case, Double)]): Clustered =
    Clustered(
      values.size,
      Estimate.clustered(values.map((c, v) => c.clusters.exchange -> v)),
      Estimate.clustered(values.map((c, v) => author(c) -> v))
    )

  /** What a case's author clusters by: its author, or itself when none was enrolled. */
  private[score] def author(c: Case): Either[CaseId, Digest] = c.author.toRight(c.id)
}

/** A proportion of cases clustered twice, as [[Clustered]] is: by exchange and by author. */
final case class Proportions(exchange: Proportion, author: Proportion)

object Proportions {

  /** The proportion of cases that hit, each a case and whether it hit. */
  def of(hits: Vector[(Case, Boolean)]): Proportions =
    Proportions(
      Proportion.of(hits.map((c, hit) => c.clusters.exchange -> hit)),
      Proportion.of(hits.map((c, hit) => Clustered.author(c) -> hit))
    )
}

/** A score over every case scored (`all`), and over those labelled `context: ok` alone and
  * `short` alone: an error on the first is the model's, on the second the input builder's.
  */
final case class Split[A](all: A, ok: A, short: A)

object Split {

  /** `values` clustered, all and by context as `labels` labels each case. */
  def of(values: Vector[(Case, Double)], labels: Labels): Split[Clustered] = {
    def only(c: Context) = values.filter((k, _) => labels.of(k.id).context.contains(c))
    Split(Clustered.of(values), Clustered.of(only(Context.Ok)), Clustered.of(only(Context.Short)))
  }
}
