package grit.kit.environment

import grit.kit.deployment.{Deployments, Placer}
import grit.models.OpenRouterConfig

import utest.*

/** What [[Secrets.of]] reads from the environment, and refuses. */
object SecretsTests extends TestSuite {

  private val db = Map("GRIT_DATABASE_URL" -> "jdbc:postgresql://localhost:5432/grit_test")

  val tests = Tests {
    test("Jev's settings are read only for Jev as placer, and a blank key is refused, naming it") {
      val jev = Deployments.accepted(placer = Placer.Jev)
      val stub = Deployments.accepted(placer = Placer.Stub)
      (
        Secrets.of(db + ("JEV_API_KEY" -> "sk-jev-1"), jev).map(_.jev.map(_.apiKey)),
        Secrets.of(db + ("JEV_API_KEY" -> "sk-jev-1"), stub).map(_.jev.map(_.apiKey)),
        Secrets.of(db + ("JEV_API_KEY" -> " "), jev).map(_ => ())
      ) ==> (
        Right(Some("sk-jev-1")),
        Right(None),
        Left(SecretsRefusal.Key(OpenRouterConfig.Invalid.Empty("JEV_API_KEY")))
      )
    }

    test("without OpenRouter's key the model is the stub; a blank key is refused, naming it") {
      val d = Deployments.accepted()
      (
        Secrets.of(db, d).map(_.openRouter),
        Secrets.of(db + ("OPENROUTER_API_KEY" -> "sk-or-1"), d).map(_.openRouter),
        Secrets.of(db + ("OPENROUTER_API_KEY" -> " "), d).map(_ => ())
      ) ==> (
        Right(None),
        Right(Some("sk-or-1")),
        Left(SecretsRefusal.Key(OpenRouterConfig.Invalid.Empty("OPENROUTER_API_KEY")))
      )
    }
  }
}
