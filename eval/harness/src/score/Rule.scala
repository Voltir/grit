package grit.eval.harness.score

import scala.util.Try

import grit.eval.harness.corpus.{Digest, Fields}
import grit.eval.harness.log.Header
import grit.eval.harness.stats.Estimate

/** What a rule measures a question by, and which way is better. */
enum Metric {

  /** The Brier score ([[Brier]]): lower is better. */
  case Brier
}

/** An adoption rule, written before the run it judges: run B is adopted over run A on
  * `question` when B's `metric` is better by at least `least` and by at least the paired
  * comparison's MDE. `digest` is the digest of the rule's file, what a run started under it
  * records ([[Header.rule]]).
  */
final case class Rule private (question: Target, metric: Metric, least: Double, digest: Digest)

object Rule {

  /** The rule `text` holds, JSON `{"question": <"kind", a tag's name or "place">, "metric":
    * "brier", "better": "lower", "least": <a number, at least 0>}`; why not, when it is not of
    * that form, or `better` is not the metric's way.
    */
  def read(text: String): Either[String, Rule] =
    for {
      json <- Try(ujson.read(text)).toOption.toRight("rule: not JSON")
      f = Fields("rule", json)
      question <- f.str("question").flatMap(q => Target.read(q).toRight(s"rule: no question $q"))
      metric <- f.str("metric").flatMap {
        case "brier" => Right(Metric.Brier)
        case m => Left(s"rule: no metric $m")
      }
      _ <- f.str("better").filterOrElse(_ == "lower", "rule: brier is better lower")
      least <- f.num("least").filterOrElse(_ >= 0, "rule: least is under 0")
    } yield new Rule(question, metric, least, Digest.text(text))
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
    * per case ([[Paired.of]]; `None` under two clusters). Refused, in this order, when `b`
    * was not started under `rule`, `paired` is `None`, or |its mean| is under its MDE or under
    * the rule's least.
    */
  def of(rule: Rule, b: Header, paired: Option[Estimate]): Decision =
    if (!b.rule.contains(rule.digest)) Refused(Refusal.OtherRule)
    else
      paired.fold(Refused(Refusal.Unclustered)) { d =>
        if (math.abs(d.mean) < d.mde) Refused(Refusal.UnderMde(d))
        else if (math.abs(d.mean) < rule.least) Refused(Refusal.UnderLeast(d))
        else
          rule.metric match {
            case Metric.Brier => if (d.mean < 0) Adopted(d) else Kept(d)
          }
      }
}

object Paired {

  /** `rule`'s metric on its question, B's less A's, over the cases both answered and labelled,
    * clustered by exchange; `None` under two clusters.
    */
  def of(rule: Rule, a: Scoring, b: Scoring): Option[Estimate] =
    rule.metric match {
      case Metric.Brier =>
        val was = Brier.of(rule.question, a).map((c, v) => c.id -> v).toMap
        Clustered
          .of(Brier.of(rule.question, b).flatMap((c, v) => was.get(c.id).map(x => c -> (v - x))))
          .exchange
    }
}
