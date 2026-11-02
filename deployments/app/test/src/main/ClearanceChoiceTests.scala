package grit.app.main

import grit.core.edge.Variable
import grit.core.model.{Policy, Upstream}
import grit.core.visibility.{Cleared, Compartments, Label, Party}
import grit.kit.deployment.Offered
import grit.models.{JevClassifier, OpenRouterConfig, Seed, StubClassifier, StubModels}
import grit.slack.edge.{Backfill, ReplyNote, SlackCommand, SlackEdge}
import grit.slack.event.TeamId

import utest.*

/** Which parties the reference deployment clears (ADR 0031), and what `grit serve` says of
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
      (Clearances.cleared(seed), Clearances.cleared(named)) ==> (
        always,
        Cleared(Party.Model(seed.turn.first.assignment.ref.model, acme), top) +: always
      )
    }

    test("a pair only the dream role names an upstream for is cleared at the top label") {
      val acme = Upstream.of("acme").getOrElse(throw new java.lang.AssertionError("acme"))
      val dreaming = OpenRouterConfig
        .policy(Map("GRIT_DREAM_MODEL" -> "d/m", "GRIT_DREAM_PROVIDER" -> "acme"), seed)
        .fold(e => throw new java.lang.AssertionError(e.message), identity)
      Clearances.cleared(dreaming) ==> Cleared(
        Party.Model(dreaming.dream.first.assignment.ref.model, acme),
        top
      ) +: always
    }

    test(
      "serve's visibility carries the clearances of the policy its environment names, with administrators named or not"
    ) {
      val grit = SlackCommand.of("/grit").fold(e => throw new java.lang.AssertionError(e), identity)
      val acme = Party.Model(
        seed.turn.first.assignment.ref.model,
        Upstream.of("acme").getOrElse(throw new java.lang.AssertionError("acme"))
      )
      val parties = always.map(_.party) :+ acme
      def clearances(env: Map[String, String], slackIn: Option[TeamId]) =
        Deploying
          .deployment(
            env,
            Offered.Read,
            Vector(SlackEdge.serving(grit, Backfill.Default, ReplyNote.Off)),
            Vector.empty,
            java.time.ZoneOffset.UTC,
            slackIn
          )
          .map(d => parties.map(d.visibility.clearances.of))
      val seeded = Right(always.map(_.clearance) :+ Label.Public)
      (
        clearances(Map.empty, None),
        clearances(Map("GRIT_ADMINS" -> "U012AB34"), Some(TeamId("T0FAKE"))),
        clearances(
          Map(Variable.value(OpenRouterConfig.KeyVar) -> "sk-test", "GRIT_PROVIDER" -> "acme"),
          None
        )
      ) ==> (seeded, seeded, Right(always.map(_.clearance) :+ top))
    }

    test(
      "under the seed, serve names each role's variable that would name its first upstream, and the clearance that pair then gets"
    ) {
      Clearances.uncleared(seed) ==> Vector(
        "GRIT_PROVIDER is unset: the turn's first model, whose upstream OpenRouter picks, is " +
          "cleared for public rooms alone; naming its one upstream there clears that pair for every label",
        "GRIT_SUMMARY_PROVIDER is unset: the summary's first model, whose upstream OpenRouter picks, is " +
          "cleared for public rooms alone; naming its one upstream there clears that pair for every label",
        "GRIT_QUERY_PROVIDER is unset: the query's first model, whose upstream OpenRouter picks, is " +
          "cleared for public rooms alone; naming its one upstream there clears that pair for every label",
        "GRIT_HEARD_PROVIDER is unset: the heard closing's first model, whose upstream OpenRouter picks, is " +
          "cleared for public rooms alone; naming its one upstream there clears that pair for every label",
        "GRIT_DREAM_PROVIDER is unset: the dream summary's first model, whose upstream OpenRouter picks, is " +
          "cleared for public rooms alone; naming its one upstream there clears that pair for every label " +
          "(the dream summary follows no other role's variables)"
      )
    }
  }
}
