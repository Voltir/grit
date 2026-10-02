package grit.eval.harness.log

import java.time.Instant

import grit.core.stitch.Tuning
import grit.dbos.engine.Build
import grit.eval.harness.corpus.Digest

/** What a run was, written as its log's first line: the corpus it ran over (`corpus`, its
  * directory's name, and the digest of its files), the labels in force when it started, the
  * variant (its `name`, the digest of the `wording` it asks in, the `model` it requests, and
  * the `tuning` it rebuilds stitch and triage inputs under), the build running it, how many
  * times each request was asked, whether the cache answered, the spend `cap` in USD, and the
  * digest of the adoption rule it was started under.
  *
  * @param labels
  *   `None` when no labels existed
  * @param tuning
  *   `None` when each case's own tuning was used
  * @param rule
  *   `None` when it was started under none
  */
final case class Header(
    corpus: String,
    corpusDigest: Digest,
    labels: Option[Digest],
    variant: String,
    wording: Digest,
    model: String,
    tuning: Option[Tuning],
    build: Build,
    repeats: Int,
    cache: Boolean,
    cap: BigDecimal,
    rule: Option[Digest],
    started: Instant
)

/** A run's last line: what its calls cost in USD (cached ones counted as free), and how many
  * of its rows were answered, from the cache among them, failed, and skipped at the cap.
  */
final case class Footer(
    spent: BigDecimal,
    answered: Int,
    cached: Int,
    failed: Int,
    skipped: Int
)

object Footer {

  /** The footer of a run that made `rows` and spent `spent`. */
  def of[A](spent: BigDecimal, rows: Vector[Row[A]]): Footer = {
    def count(f: Outcome[A] => Boolean) = rows.count(r => f(r.outcome))
    Footer(
      spent,
      count {
        case Outcome.Answered(_) => true
        case _ => false
      },
      rows.count(_.cached),
      count {
        case Outcome.Failed(_) => true
        case _ => false
      },
      count(_ == Outcome.Skipped)
    )
  }
}
