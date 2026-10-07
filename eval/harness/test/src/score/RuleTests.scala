package grit.eval.harness.score

import grit.core.triage.Kind
import grit.eval.harness.capture.{Case, Digest}
import grit.eval.harness.label.{Context, Labelled, Labels}
import grit.eval.harness.log.Suite
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

  private val scoped =
    """{"question": "mean", "metric": "brier", "better": "lower", "least": 0.03,
      | "context": "short",
      | "guard": {"context": "ok", "worse_by": 0.02},
      | "not_worse": {"question": "helps", "worse_by": 0}}""".stripMargin
  private val scopedRule = Rule.read(scoped).fold(sys.error, identity)

  /** Cases 1 and 2 labelled short, 3 and 4 ok, each its own exchange; every one a question
    * nobody waits on, durable, that grit's reply would not help; case 4's helps unlabelled.
    */
  private val cases: Vector[Case] = Vector.tabulate(4)(n => caseOf(n + 1, n + 1))
  private val labels = Labels(
    cases.zipWithIndex.map { (c, i) =>
      c.id -> Labelled(
        Some(Kind.Question),
        Some(false),
        Some(true),
        Option.when(i < 3)(false),
        Some(if (i < 2) Context.Short else Context.Ok),
        None,
        None
      )
    }.toMap,
    Set("v2")
  )

  /** Triage's answer: `kinds` in Kind's order, then waiting, durable and helps. */
  private final case class Asked(kinds: Vector[Double], w: Double, d: Double, h: Double)

  /** Each case answered `each` in turn. */
  private def scoring(each: Vector[Asked]): Scoring =
    Scoring(
      cases,
      Answers.of(
        cases.zip(each).map((c, x) => row(Suite.Triage, c.id, 0, triage(x.kinds, x.w, x.d, x.h)))
      ),
      labels
    )

  private val right: Asked = Asked(Vector(1.0, 0, 0, 0, 0), 0.0, 1.0, 0.0)
  // Kind .5 + .5 = .5 wrong by .25 twice; waiting .5: .25; durable and helps right. Mean of
  // the four Briers (.5 + .25 + 0 + 0) / 4 = .1875.
  private val unsure: Asked = Asked(Vector(0.5, 0.5, 0, 0, 0), 0.5, 1.0, 0.0)

  val tests = Tests {
    test("a rule is read with the digest of its file, which a run started under it records") {
      (rule.measure, rule.metric, rule.least, rule.context, rule.guards, rule.digest) ==>
        (
          Measure.Of(Target.Tagged(Tag.Durable)),
          Metric.Brier,
          0.02,
          None,
          Vector.empty,
          Digest.text(text)
        )
      Rule.read(text.replace("lower", "higher")) ==> Left("rule: brier is better lower")
    }

    test("a rule may judge the four questions' mean in one context, guarded on others") {
      (scopedRule.measure, scopedRule.context, scopedRule.guards) ==> (
        Measure.Mean,
        Some(Context.Short),
        Vector(
          Guard(Measure.Four, Some(Context.Ok), 0.02),
          Guard(Vector(Target.Tagged(Tag.Helps)), Some(Context.Short), 0.0)
        )
      )
      Rule.read(scoped.replace("\"short\"", "\"long\"")) ==> Left("rule: no context long")
    }

    test(
      "the mean scores a case by its four Briers' mean, only over the cases labelled on all four"
    ) {
      val all = """{"question": "mean", "metric": "brier", "better": "lower", "least": 0}"""
      val d = Paired.of(
        Rule.read(all).fold(sys.error, identity),
        scoring(Vector.fill(4)(right)),
        scoring(Vector.fill(4)(unsure))
      )
      d.map(e => (e.n, e.mean)) ==> Some((3, 0.1875))
    }

    test("a scoped rule pairs only the cases labelled in its context") {
      val d = Paired.of(
        scopedRule,
        scoring(Vector.fill(4)(right)),
        scoring(Vector(unsure, unsure, right, right))
      )
      d.map(e => (e.n, e.mean)) ==> Some((2, 0.1875))
    }

    test(
      "a guard breaks when B is worse on any of its questions by more than its line, on the mean"
    ) {
      val (a, b) = (
        scoring(Vector(unsure, unsure, right, right)),
        // Short: right throughout. Ok: case 3's helps .2, a Brier .04 worse; case 4's
        // unlabelled.
        scoring(Vector(right, right, right.copy(h = 0.2), right))
      )
      Guarded
        .of(scopedRule, a, b)
        .map(x => (x.question, x.context, math.round(x.difference * 1e9))) ==>
        Vector((Target.Tagged(Tag.Helps), Some(Context.Ok), 40_000_000L))
      Guarded.of(scopedRule, a, a) ==> Vector.empty
    }

    test("adopts B when its Brier is lower by more than the MDE and the rule's least") {
      // −.5, −.52, −.48, −.5: mean −.5, se .0082; MDE (3.1824 + .9785) · .0082 = .034.
      val d = diff(-0.5, -0.52, -0.48, -0.5)
      Option.when(d.isDefined)(Decision.of(rule, under, d, Vector.empty)) ==>
        d.map(Decision.Adopted(_))
    }

    test("a broken guard refuses what would be adopted, and changes nothing else") {
      val broken = Guarded.Broken(Target.Tagged(Tag.Helps), Some(Context.Ok), 0.04)
      val better = diff(-0.5, -0.52, -0.48, -0.5)
      val worse = diff(0.5, 0.52, 0.48, 0.5)
      Option.when(better.isDefined)(Decision.of(rule, under, better, Vector(broken))) ==>
        better.map(e => Decision.Refused(Refusal.GuardBroken(e, broken)))
      Option.when(worse.isDefined)(Decision.of(rule, under, worse, Vector(broken))) ==>
        worse.map(Decision.Kept(_))
    }

    test("keeps A when B's Brier is higher by as much") {
      val d = diff(0.5, 0.52, 0.48, 0.5)
      Option.when(d.isDefined)(Decision.of(rule, under, d, Vector.empty)) ==> d.map(
        Decision.Kept(_)
      )
    }

    test("refuses a difference under the MDE, in either direction") {
      // −.5, .4, −.3, .2: mean −.05, se .2102; MDE about .875.
      val d = diff(-0.5, 0.4, -0.3, 0.2)
      Option.when(d.isDefined)(Decision.of(rule, under, d, Vector.empty)) ==> d.map(e =>
        Decision.Refused(Refusal.UnderMde(e))
      )
    }

    test("refuses a difference under the rule's least, however precise") {
      // −.01 ± .0001: mean −.01, well over its MDE, under .02.
      val d = diff(-0.0099, -0.0101, -0.01, -0.01)
      Option.when(d.isDefined)(Decision.of(rule, under, d, Vector.empty)) ==> d.map(e =>
        Decision.Refused(Refusal.UnderLeast(e))
      )
    }

    test("refuses a run started under another rule, or under none") {
      val d = diff(-0.5, -0.52, -0.48, -0.5)
      Decision.of(rule, header(rule = Some(Digest.text("{}"))), d, Vector.empty) ==>
        Decision.Refused(Refusal.OtherRule)
      Decision.of(rule, header(), d, Vector.empty) ==> Decision.Refused(Refusal.OtherRule)
      Decision.of(rule, under, None, Vector.empty) ==> Decision.Refused(Refusal.Unclustered)
    }
  }
}
