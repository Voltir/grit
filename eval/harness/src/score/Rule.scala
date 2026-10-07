package grit.eval.harness.score

import scala.util.Try

import grit.eval.harness.capture.{Case, Digest, Fields}
import grit.eval.harness.label.Context
import grit.eval.harness.log.Header
import grit.eval.harness.stats.Estimate

/** What a rule measures a question by, and which way is better. */
enum Metric {

  /** The Brier score ([[Brier]]): lower is better. */
  case Brier
}

/** What a rule's metric is taken of: one question, or triage's four together. */
enum Measure {

  /** `question` alone. */
  case Of(question: Target)

  /** Each case's mean over triage's four questions ([[Measure.Four]]), a case counted only
    * when it is labelled on all four.
    */
  case Mean
}

object Measure {

  /** Triage's four questions: its kind and its three tags. */
  val Four: Vector[Target] = Target.all.filterNot(_ == Target.Places)

  /** `m`'s written name: its question's ([[Target.written]]), or `mean`. */
  def written(m: Measure): String = m match {
    case Of(q) => Target.written(q)
    case Mean => "mean"
  }

  /** The measure written `name`; `None` for no measure's. */
  def read(name: String): Option[Measure] =
    if (name == "mean") Some(Mean) else Target.read(name).map(Of(_))

  /** Each case's Brier score on `m` in `s`, in its cases' order. */
  def brier(m: Measure, s: Scoring): Vector[(Case, Double)] = m match {
    case Of(q) => grit.eval.harness.score.Brier.of(q, s)
    case Mean =>
      val each =
        Four.map(q => grit.eval.harness.score.Brier.of(q, s).map((c, v) => c.id -> v).toMap)
      s.cases.flatMap { c =>
        val scores = each.flatMap(_.get(c.id))
        Option.when(scores.size == Four.size)(c -> scores.sum / Four.size)
      }
  }
}

/** A line a rule draws before its run: B is refused when, over the cases labelled `context`
  * (every case when `None`), its Brier score on any of `questions` is worse than A's by more
  * than `worseBy`, the paired difference's mean taken as it stands, not its interval's bound.
  */
final case class Guard(questions: Vector[Target], context: Option[Context], worseBy: Double)

/** An adoption rule, written before the run it judges: run B is adopted over run A on
  * `measure`, over the cases labelled `context` (every case when `None`), when B's `metric` is
  * better by at least `least` and by at least the paired comparison's MDE, and no guard is
  * broken. `digest` is the digest of the rule's file, what a run started under it records
  * ([[Header.rule]]).
  */
final case class Rule private (
    measure: Measure,
    metric: Metric,
    least: Double,
    context: Option[Context],
    guards: Vector[Guard],
    digest: Digest
)

object Rule {

  /** The rule `text` holds, JSON `{"question": <"mean", "kind", a tag's name or "place">,
    * "metric": "brier", "better": "lower", "least": <a number, at least 0>}`, and optionally
    * `"context": <"ok" or "short">`, a `"guard": {"context": <"ok" or "short">, "worse_by":
    * <n>}` over triage's four questions, and a `"not_worse": {"question": <a question>,
    * "worse_by": <n>}` in the rule's context, each `n` at least 0; why not, when it is not of
    * that form, or `better` is not the metric's way.
    */
  def read(text: String): Either[String, Rule] =
    for {
      json <- Try(ujson.read(text)).toOption.toRight("rule: not JSON")
      f = Fields("rule", json)
      measure <- f.str("question").flatMap(q => Measure.read(q).toRight(s"rule: no question $q"))
      metric <- f.str("metric").flatMap {
        case "brier" => Right(Metric.Brier)
        case m => Left(s"rule: no metric $m")
      }
      _ <- f.str("better").filterOrElse(_ == "lower", "rule: brier is better lower")
      least <- f.num("least").filterOrElse(_ >= 0, "rule: least is under 0")
      context <- f.added("context").flatMap(Fields.opt(_)(readContext))
      guard <- f
        .added("guard")
        .flatMap(Fields.opt(_) { g =>
          val x = Fields("rule: guard", g)
          for {
            c <- x.field("context").flatMap(readContext)
            w <- worseBy(x)
          } yield Guard(Measure.Four, Some(c), w)
        })
      notWorse <- f
        .added("not_worse")
        .flatMap(Fields.opt(_) { g =>
          val x = Fields("rule: not_worse", g)
          for {
            q <- x.str("question").flatMap(q => Target.read(q).toRight(s"rule: no question $q"))
            w <- worseBy(x)
          } yield Guard(Vector(q), context, w)
        })
    } yield new Rule(measure, metric, least, context, guard.toVector ++ notWorse, Digest.text(text))

  private def readContext(v: ujson.Value): Either[String, Context] =
    Fields
      .str("rule: context", v)
      .flatMap(c => Context.values.find(Context.written(_) == c).toRight(s"rule: no context $c"))

  private def worseBy(f: Fields): Either[String, Double] =
    f.num("worse_by").filterOrElse(_ >= 0, s"${f.what}: worse_by is under 0")
}

/** Why a rule decided nothing. */
enum Refusal {

  /** Run B was not started under this rule, so the rule may postdate it. */
  case OtherRule

  /** The paired difference has under two clusters: it has no error to judge it by. */
  case Unclustered

  /** The difference is under the paired comparison's MDE: this many cases cannot tell it from
    * none.
    */
  case UnderMde(difference: Estimate)

  /** The difference is under the rule's least. */
  case UnderLeast(difference: Estimate)

  /** B would be adopted by `difference`, but is worse than one of the rule's guards allows. */
  case GuardBroken(difference: Estimate, broken: Guarded.Broken)
}

/** What a rule made of two runs. */
enum Decision {
  case Refused(why: Refusal)

  /** B is better by enough: adopt it. */
  case Adopted(difference: Estimate)

  /** B is worse by enough: keep A. */
  case Kept(difference: Estimate)
}

object Decision {

  /** Whether `rule` adopts run B (`b`, its header) over run A, on `paired`, B's metric less A's
    * per case ([[Paired.of]]; `None` under two clusters), with `broken` the rule's guards B
    * breaks ([[Guarded.of]]). Refused, in this order, when `b` was not started under `rule`,
    * `paired` is `None`, or |its mean| is under its MDE or under the rule's least; kept when B
    * is worse; refused when B is better and breaks a guard, naming the first.
    */
  def of(
      rule: Rule,
      b: Header,
      paired: Option[Estimate],
      broken: Vector[Guarded.Broken]
  ): Decision =
    if (!b.rule.contains(rule.digest)) Refused(Refusal.OtherRule)
    else
      paired.fold(Refused(Refusal.Unclustered)) { d =>
        if (math.abs(d.mean) < d.mde) Refused(Refusal.UnderMde(d))
        else if (math.abs(d.mean) < rule.least) Refused(Refusal.UnderLeast(d))
        else
          rule.metric match {
            case Metric.Brier =>
              if (d.mean >= 0) Kept(d)
              else broken.headOption.fold(Adopted(d))(x => Refused(Refusal.GuardBroken(d, x)))
          }
      }
}

object Paired {

  /** `rule`'s metric on its measure, B's less A's, over the cases both answered and labelled
    * (in its context, when it has one), clustered by exchange; `None` under two clusters.
    */
  def of(rule: Rule, a: Scoring, b: Scoring): Option[Estimate] =
    rule.metric match {
      case Metric.Brier =>
        Clustered.of(differences(rule.measure, rule.context, a, b)).exchange
    }

  /** Each case's Brier score on `m` in `b` less that in `a`, over the cases both answered and
    * labelled (labelled `context`, when given), in `b`'s cases' order.
    */
  private[score] def differences(
      m: Measure,
      context: Option[Context],
      a: Scoring,
      b: Scoring
  ): Vector[(Case, Double)] = {
    def in(s: Scoring) =
      s.copy(cases = s.cases.filter(c => context.forall(s.labels.of(c.id).context.contains)))
    val was = Measure.brier(m, in(a)).map((c, v) => c.id -> v).toMap
    Measure.brier(m, in(b)).flatMap((c, v) => was.get(c.id).map(x => c -> (v - x)))
  }
}

/** A rule's guards, judged. */
object Guarded {

  /** A guard B broke: on `question`, over the cases labelled `context` (every case when
    * `None`), B's Brier score less A's, a mean over the cases both answered and labelled.
    */
  final case class Broken(question: Target, context: Option[Context], difference: Double)

  /** The lines of `rule`'s guards that B, scored by `b`, breaks against A, scored by `a`, in
    * the order the guards and their questions are listed; a guard over no case paired breaks
    * nothing.
    */
  def of(rule: Rule, a: Scoring, b: Scoring): Vector[Broken] =
    rule.guards.flatMap(g =>
      g.questions.flatMap { q =>
        val d = Paired.differences(Measure.Of(q), g.context, a, b).map(_._2)
        Option
          .when(d.nonEmpty)(d.sum / d.size)
          .filter(_ > g.worseBy)
          .map(Broken(q, g.context, _))
      }
    )
}
