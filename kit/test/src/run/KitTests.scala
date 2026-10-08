package grit.kit.run

import grit.core.edge.{EdgeRefusal, Variable}
import grit.core.id.{AttesterName, EdgeName}
import grit.core.identity.{Domain, Identities, Realm, Vouching}
import grit.kit.deployment.{Deployments, Topics}
import grit.kit.environment.SecretsRefusal
import grit.models.OpenRouterConfig

import utest.*

/** What [[Kit.serve]] refuses before it opens the engine, and what each edge it serves
  * attests for.
  */
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

    test(
      "an edge records for the realms its own attester is trusted for, and one attesting nothing for none; each for every claimed domain"
    ) {
      def ok[A](e: Either[String, A]): A =
        e.fold(why => throw new java.lang.AssertionError(why), identity)
      val (t1, t2) = (ok(Realm.of("slack", "T1")), ok(Realm.of("slack", "T2")))
      val example = ok(Domain.of("example.com"))
      val (chat, directory) = (AttesterName("chat"), AttesterName("directory"))
      val identities =
        Identities
          .of(Vector(Vouching(chat, t1), Vouching(directory, t2)), Set(example))
          .fold(r => throw new java.lang.AssertionError(r.message), identity)
      Vector(Some(chat), Some(directory), None).map(Kit.trustOf(identities, _)) ==> Vector(
        (Set(t1), Set(example)),
        (Set(t2), Set(example)),
        (Set(), Set(example))
      )
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
