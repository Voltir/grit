package grit.eval.harness.bench

import scala.concurrent.duration.DurationInt

import grit.core.id.{Harness, SessionId}
import grit.core.message.{Cost, Tokens, Usage}
import grit.core.model.ModelId
import grit.dbos.internal.TrialRows

import utest.*

/** Cents per passing trial over the bench's trials ([[PerPass]]). */
object PerPassTests extends TestSuite {

  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  /** Trial `id`, of `harness` on `task` with `model`, costing `usd` (unpriced when `None`). */
  private def trial(
      id: Int,
      task: String,
      harness: String,
      model: String,
      usd: Option[String],
      passed: Boolean,
      distance: Option[Long],
      thinking: Option[Long] = None,
      input: Long = 100
  ): TrialRows.Trial = {
    got(
      TrialRows.Trial.of(
        got(SessionId.of(got(Harness.of(harness)), s"s$id")),
        got(TrialRows.TaskName.of(task)),
        ModelId.of(model).getOrElse(throw new java.lang.AssertionError(model)),
        Usage(
          Tokens(input),
          Tokens(10000),
          Tokens.Zero,
          usd.map(BigDecimal(_)),
          thinking = thinking.map(Tokens(_))
        ),
        3,
        1.second,
        None,
        None,
        passed,
        distance,
        None
      )
    )
  }

  private val haiku = "anthropic/claude-haiku-5-5"

  val tests = Tests {
    test(
      "a group's pass costs every trial's spend over its passes, its distances the passing trials' alone"
    ) {
      val trials = Vector(
        trial(1, "blobs", "grit", haiku, Some("0.022"), passed = true, Some(58)),
        trial(2, "blobs", "claude-code", haiku, Some("0.003"), passed = false, Some(90)),
        trial(3, "blobs", "grit", haiku, Some("0.028"), passed = true, Some(53)),
        trial(4, "blobs", "claude-code", haiku, Some("0.032"), passed = true, Some(68))
      )
      PerPass.groups(trials).map(g => (g.harness, g.trials, g.passed, g.perPass, g.distances)) ==>
        Vector(
          (got(Harness.of("grit")), 2, 2, Some(Cost.Exact(BigDecimal("0.025"))), Vector(58L, 53L)),
          (
            got(Harness.of("claude-code")),
            2,
            1,
            Some(Cost.Exact(BigDecimal("0.035"))),
            Vector(68L)
          )
        )
    }

    test("a group with no pass has no cost a pass; one with an unpriced trial costs at least") {
      val trials = Vector(
        trial(5, "blobs", "grit", haiku, Some("0.01"), passed = false, Some(9)),
        trial(6, "other", "grit", haiku, None, passed = true, Some(5)),
        trial(7, "other", "grit", haiku, Some("0.02"), passed = true, None)
      )
      PerPass.groups(trials).map(g => (g.spent, g.perPass)) ==> Vector(
        (Cost.Exact(BigDecimal("0.01")), None),
        (Cost.AtLeast(BigDecimal("0.02")), Some(Cost.AtLeast(BigDecimal("0.01"))))
      )
    }

    test("a person reads a heading, then a line a group, in cents") {
      val trials = Vector(
        trial(8, "blobs", "grit", haiku, Some("0.022"), passed = true, Some(58)),
        trial(9, "blobs", "grit", haiku, Some("0.028"), passed = true, Some(53)),
        trial(10, "other", "claude-code", haiku, Some("0.55"), passed = true, Some(40)),
        trial(11, "blobs", "claude-code", haiku, Some("0.01"), passed = false, Some(9))
      )
      PerPass.lines(trials) ==> Vector(
        "task   harness      model                       trials  passed  ¢/pass   thinking      thinking ¢/pass  total  distance",
        "blobs  grit         anthropic/claude-haiku-5-5  2       2       2.5¢     not reported  not reported     5¢     53–58",
        "other  claude-code  anthropic/claude-haiku-5-5  1       1       55¢      not reported  not reported     55¢    40",
        "blobs  claude-code  anthropic/claude-haiku-5-5  1       0       no pass  not reported  not reported     1¢     —"
      )
    }

    test("a person reads a trial's thinking and its cost a pass, or that it is not reported") {
      val trials = Vector(
        trial(8, "blobs", "grit", haiku, Some("0.022"), passed = true, Some(58), Some(2000)),
        trial(9, "blobs", "grit", haiku, Some("0.028"), passed = true, Some(53), Some(4000)),
        trial(10, "other", "claude-code", haiku, Some("0.55"), passed = true, Some(40)),
        trial(11, "blobs", "claude-code", haiku, Some("0.01"), passed = false, Some(9), Some(1000))
      )
      PerPass.lines(trials) ==> Vector(
        "task   harness      model                       trials  passed  ¢/pass   thinking      thinking ¢/pass  total  distance",
        "blobs  grit         anthropic/claude-haiku-5-5  2       2       2.5¢     6000          0.15¢            5¢     53–58",
        "other  claude-code  anthropic/claude-haiku-5-5  1       1       55¢      not reported  not reported     55¢    40",
        "blobs  claude-code  anthropic/claude-haiku-5-5  1       0       no pass  1000          no pass          1¢     —"
      )
    }
    test(
      "a trial whose calls together passed a card's bound has its thinking priced at least at the short card, never all at the long"
    ) {
      // 4091 thinking tokens at Claude Haiku 5.5's short card (0.50 USD a million) are 0.2¢; its
      // calls' inputs summed pass 100,000, which one call's prompt need not have.
      val trials = Vector(
        trial(
          12,
          "blobs",
          "grit",
          haiku,
          Some("0.025"),
          passed = true,
          Some(69),
          Some(4091),
          input = 240000
        )
      )
      PerPass.groups(trials).map(_.thinkingSpent) ==> Vector(
        Some(Cost.AtLeast(BigDecimal("0.0020455")))
      )
    }
  }
}
