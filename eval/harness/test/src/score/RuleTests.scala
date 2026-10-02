package grit.eval.harness.score

import grit.eval.harness.corpus.Digest
import grit.eval.harness.stats.Estimate

import utest.*
import Fixtures.*

/** An adoption rule, and what it decides of two runs. */
object RuleTests extends TestSuite {

  private val text =
    """{"question": "durable", "metric": "brier", "better": "lower", "least": 0.02}"""
  private val rule = Rule.read(text).fold(sys.error, identity)
  private val under = header(variant = "b", rule = Some(Digest.text(text)))

  /** Four clusters of one case each. */
  private def diff(values: Double*): Option[Estimate] =
    Estimate.clustered(values.toVector.zipWithIndex.map((v, i) => i -> v))

  val tests = Tests {
    test("a rule is read with the digest of its file, which a run started under it records") {
      (rule.question, rule.metric, rule.least, rule.digest) ==>
        (Target.Tagged(Tag.Durable), Metric.Brier, 0.02, Digest.text(text))
      Rule.read(text.replace("lower", "higher")) ==> Left("rule: brier is better lower")
    }

    test("adopts B when its Brier is lower by more than the MDE and the rule's least") {
      // −.5, −.52, −.48, −.5: mean −.5, se .0082; MDE (3.1824 + .9785) · .0082 = .034.
      val d = diff(-0.5, -0.52, -0.48, -0.5)
      Option.when(d.isDefined)(Decision.of(rule, under, d)) ==> d.map(Decision.Adopted(_))
    }

    test("keeps A when B's Brier is higher by as much") {
      val d = diff(0.5, 0.52, 0.48, 0.5)
      Option.when(d.isDefined)(Decision.of(rule, under, d)) ==> d.map(Decision.Kept(_))
    }

    test("refuses a difference under the MDE, in either direction") {
      // −.5, .4, −.3, .2: mean −.05, se .2102; MDE about .875.
      val d = diff(-0.5, 0.4, -0.3, 0.2)
      Option.when(d.isDefined)(Decision.of(rule, under, d)) ==> d.map(e =>
        Decision.Refused(Refusal.UnderMde(e))
      )
    }

    test("refuses a difference under the rule's least, however precise") {
      // −.01 ± .0001: mean −.01, well over its MDE, under .02.
      val d = diff(-0.0099, -0.0101, -0.01, -0.01)
      Option.when(d.isDefined)(Decision.of(rule, under, d)) ==> d.map(e =>
        Decision.Refused(Refusal.UnderLeast(e))
      )
    }

    test("refuses a run started under another rule, or under none") {
      val d = diff(-0.5, -0.52, -0.48, -0.5)
      Decision.of(rule, header(rule = Some(Digest.text("{}"))), d) ==>
        Decision.Refused(Refusal.OtherRule)
      Decision.of(rule, header(), d) ==> Decision.Refused(Refusal.OtherRule)
      Decision.of(rule, under, None) ==> Decision.Refused(Refusal.Unclustered)
    }
  }
}
