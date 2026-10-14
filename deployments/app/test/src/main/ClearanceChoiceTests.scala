package grit.app.main

import grit.core.model.{Policy, Upstream}
import grit.core.visibility.{Cleared, Compartments, Party}
import grit.kit.deployment.Offered
import grit.models.{JevClassifier, OpenRouterConfig, Seed, StubClassifier, StubModels}
import grit.slack.edge.{Backfill, SlackCommand, SlackEdge}
import grit.slack.event.TeamId

import utest.*

/** Which parties the reference deployment clears (ADR 0037), and what `grit serve` says of
  * the ones it cannot.
  */
object ClearanceChoiceTests extends TestSuite {

  private val seed: Policy =
    Seed.catalog.fold(e => throw new java.lang.AssertionError(e), identity).policy

  private val top = Compartments.Shipped.top

  /** What every policy clears: the stub's pair, and both classifiers' services. */
  private val always: Vector[Cleared] = Vector(
    Party.of(StubModels.Ref).getOrElse(throw new java.lang.AssertionError("the stub's party")),
    Party.Service(JevClassifier.Service),
    Party.Service(StubClassifier.Service)
  ).map(Cleared(_, top))

  val tests = Tests {
    test(
      "a pair the policy names an upstream for is cleared at the top label, once however many roles pin it; one OpenRouter routes is not"
    ) {
      val acme = Upstream.of("acme").getOrElse(throw new java.lang.AssertionError("acme"))
      val named = OpenRouterConfig
        .policy(Map("GRIT_PROVIDER" -> "acme"), seed)
        .fold(e => throw new java.lang.AssertionError(e.message), identity)
      (Main.cleared(seed), Main.cleared(named)) ==> (
        always,
        Cleared(Party.Model(seed.turn.first.assignment.ref.model, acme), top) +: always
      )
    }

    test("serve's visibility carries the clearances, with administrators named or not") {
      val grit = SlackCommand.of("/grit").fold(e => throw new java.lang.AssertionError(e), identity)
      def clearances(env: Map[String, String], slackIn: Option[TeamId]) =
        Main
          .deployment(
            env,
            Offered.Read,
            Vector(SlackEdge.serving(grit, Backfill.Default)),
            Vector.empty,
            java.time.ZoneOffset.UTC,
            slackIn
          )
          .map(d => always.map(c => d.visibility.clearances.of(c.party)))
      (
        clearances(Map.empty, None),
        clearances(Map("GRIT_ADMINS" -> "U012AB34"), Some(TeamId("T0FAKE")))
      ) ==> (Right(always.map(_.clearance)), Right(always.map(_.clearance)))
    }

    test(
      "under the seed, serve names each role's variable that would name its first upstream, and the clearance that pair then gets"
    ) {
      Main.uncleared(seed) ==> Vector(
        "GRIT_PROVIDER is unset: the turn's first model, whose upstream OpenRouter picks, is " +
          "cleared for public rooms alone; naming its one upstream there clears that pair for every label",
        "GRIT_SUMMARY_PROVIDER is unset: the summary's first model, whose upstream OpenRouter picks, is " +
          "cleared for public rooms alone; naming its one upstream there clears that pair for every label",
        "GRIT_QUERY_PROVIDER is unset: the query's first model, whose upstream OpenRouter picks, is " +
          "cleared for public rooms alone; naming its one upstream there clears that pair for every label",
        "GRIT_HEARD_PROVIDER is unset: the heard closing's first model, whose upstream OpenRouter picks, is " +
          "cleared for public rooms alone; naming its one upstream there clears that pair for every label"
      )
    }
  }
}
