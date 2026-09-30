package grit.kit.run

import grit.core.edge.{EdgeRefusal, Variable}
import grit.core.id.EdgeName
import grit.kit.deployment.{Deployments, Topics}
import grit.kit.environment.SecretsRefusal
import grit.models.OpenRouterConfig

import utest.*

/** What [[Kit.serve]] refuses before it opens the engine. */
object KitTests extends TestSuite {

  private def deployment(topics: Topics) =
    Deployments.accepted(
      edges = Vector(Deployments.edge("slack", asks = false, needs = Vector("SLACK_BOT_TOKEN"))),
      topics = topics
    )

  // Never reached when refused before the engine opens: nothing listens on port 1.
  private val nowhere = Map("GRIT_DATABASE_URL" -> "jdbc:postgresql://127.0.0.1:1/none")

  val tests = Tests {
    test(
      "an edge's variable unset refuses the start before the engine opens, naming the edge and the variable"
    ) {
      Kit.serve(deployment(Topics.Stub), nowhere) ==>
        Left(KitFailure.Edge(EdgeName("slack"), EdgeRefusal.Missing(Variable("SLACK_BOT_TOKEN"))))
    }

    test("Jev's topics without JEV_API_KEY refuse the start before the engine opens") {
      Kit.serve(deployment(Topics.Jev), nowhere + ("SLACK_BOT_TOKEN" -> "xoxb-1")) ==>
        Left(
          KitFailure.Unconfigured(
            SecretsRefusal.Key(OpenRouterConfig.Invalid.Missing("JEV_API_KEY"))
          )
        )
    }
  }
}
