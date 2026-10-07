package grit.eval.harness.score

import grit.core.message.Tokens
import grit.core.period.Probability
import grit.eval.harness.capture.{Case, Live, SeenCheck}
import grit.eval.harness.log.{Footer, Log, Outcome}

/** How a run's answers agree with what triage and stitching kept live, case by case: each
  * whether a case agrees, for [[Proportions.of]], or a distance per case, for [[Clustered.of]].
  * A case triage failed live, or stitching did not place, has nothing to agree with and is
  * left out.
  */
object Agreement {

  /** Whether the run's likeliest kind is the live kind. */
  def kind(cases: Vector[Case], answers: Answers): Vector[(Case, Boolean)] =
    weighed(cases, answers).map((c, live, a) => c -> (a.likeliest == live.kind))

  /** |the run's probability of yes for `t` − the live one|. */
  def tag(t: Tag, cases: Vector[Case], answers: Answers): Vector[(Case, Double)] =
    weighed(cases, answers).map((c, live, a) => c -> math.abs(Tag.of(t, a) - kept(t, live)))

  /** Whether the run and live decide `t` alike, yes at or above `threshold`. */
  def decided(
      t: Tag,
      threshold: Double,
      cases: Vector[Case],
      answers: Answers
  ): Vector[(Case, Boolean)] =
    weighed(cases, answers).map((c, live, a) =>
      c -> ((Tag.of(t, a) >= threshold) == (kept(t, live) >= threshold))
    )

  /** The largest |the run's probability − the live one| over the exchanges offered with a live
    * probability, where the case's seen check matched and the run's placing offers as many
    * exchanges as were offered live.
    */
  def place(cases: Vector[Case], answers: Answers): Vector[(Case, Double)] =
    cases.flatMap(c =>
      for {
        s <- c.stitch.filter(_.seen == SeenCheck.Match)
        a <- answers.stitch.get(c.id).filter(_.mean.ps.size == s.offered.size + 1)
      } yield c -> a.mean.ps
        .zip(s.offered)
        .flatMap((p, o) => o.p.map(live => math.abs(p - Probability.value(live))))
        .maxOption
        .getOrElse(0.0)
    )

  private def weighed(cases: Vector[Case], answers: Answers): Vector[(Case, Live.Weighed, Triage)] =
    cases.flatMap(c =>
      c.tags match {
        case w: Live.Weighed => answers.triage.get(c.id).map(a => (c, w, a.mean))
        case Live.Named(_, _, _) | Live.Unanswered(_) => None
      }
    )

  private def kept(t: Tag, w: Live.Weighed): Double = Probability.value(t match {
    case Tag.Waiting => w.waiting
    case Tag.Durable => w.durable
    case Tag.Helps => w.helps
  })
}

/** How far apart each case's repeats answered one question, over the cases answered at least
  * twice: a tag's own spread, and the largest among a kind's or a place's probabilities.
  */
object Repeated {

  def spread(q: Target, cases: Vector[Case], answers: Answers): Vector[(Case, Double)] =
    cases.flatMap(c => of(q, c, answers).map(c -> _))

  /** `c`'s spread on `q`; `None` when it was not answered at least twice. */
  def of(q: Target, c: Case, answers: Answers): Option[Double] = q match {
    case Target.Tagged(t) =>
      answers.triage.get(c.id).filter(_.repeats >= 2).map(a => Tag.of(t, a.spread))
    case Target.Kinds =>
      answers.triage
        .get(c.id)
        .filter(_.repeats >= 2)
        .map(a => a.spread.kinds.values.maxOption.getOrElse(0.0))
    case Target.Places =>
      answers.stitch.get(c.id).filter(_.repeats >= 2).map(_.spread.ps.maxOption.getOrElse(0.0))
  }
}

/** A run's answered rows' latency in milliseconds: the median and the 90th percentile (each
  * the nearest rank), and the largest.
  */
final case class Latency(median: Long, p90: Long, max: Long)

/** What a run consumed: the spend in USD its footer records (`None` when the run wrote none),
  * its rows by outcome (`counts`, whose `spent` is the footer's or 0), the input and output
  * tokens its answered rows reported (a cached row's as first asked), and their latency (`None`
  * when none was answered).
  */
final case class Spending(
    spent: Option[BigDecimal],
    rows: Int,
    counts: Footer,
    input: Long,
    output: Long,
    latency: Option[Latency]
)

object Spending {

  def of[A](log: Log[A]): Spending = {
    val answered = log.rows.filter(_.outcome match {
      case Outcome.Answered(_) => true
      case _ => false
    })
    val ms = answered.map(_.latency.toMillis).sorted
    def rank(p: Double): Long = ms.lift(math.max(0, math.ceil(p * ms.size).toInt - 1)).getOrElse(0L)
    Spending(
      log.footer.map(_.spent),
      log.rows.size,
      Footer.of(log.footer.fold(BigDecimal(0))(_.spent), log.rows),
      answered.map(r => Tokens.value(r.usage.input)).sum,
      answered.map(r => Tokens.value(r.usage.output)).sum,
      ms.lastOption.map(max => Latency(rank(0.5), rank(0.9), max))
    )
  }
}
