package grit.kit.environment

import grit.kit.deployment.{Deployments, Topics}
import grit.models.OpenRouterConfig

import utest.*

/** What [[Secrets.of]] reads from the environment, and refuses. */
object SecretsTests extends TestSuite {

  val tests = Tests {
    test("Jev's settings are read only for Jev's topics, and a blank key is refused, naming it") {
      val jev = Deployments.accepted(topics = Topics.Jev)
      val stub = Deployments.accepted(topics = Topics.Stub)
      (
        Secrets.of(Map("JEV_API_KEY" -> "sk-jev-1"), jev).map(_.jev.map(_.apiKey)),
        Secrets.of(Map("JEV_API_KEY" -> "sk-jev-1"), stub).map(_.jev.map(_.apiKey)),
        Secrets.of(Map("JEV_API_KEY" -> " "), jev).map(_ => ())
      ) ==> (
        Right(Some("sk-jev-1")),
        Right(None),
        Left(SecretsRefusal.Key(OpenRouterConfig.Invalid.Empty("JEV_API_KEY")))
      )
    }

    test("without OpenRouter's key the model is the stub; a blank key is refused, naming it") {
      val d = Deployments.accepted()
      (
        Secrets.of(Map.empty, d).map(_.openRouter),
        Secrets.of(Map("OPENROUTER_API_KEY" -> "sk-or-1"), d).map(_.openRouter),
        Secrets.of(Map("OPENROUTER_API_KEY" -> " "), d).map(_ => ())
      ) ==> (
        Right(None),
        Right(Some("sk-or-1")),
        Left(SecretsRefusal.Key(OpenRouterConfig.Invalid.Empty("OPENROUTER_API_KEY")))
      )
    }
  }
}
