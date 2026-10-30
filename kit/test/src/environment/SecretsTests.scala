package grit.kit.environment

import grit.core.model.{Assignment, ModelId, ModelRef, Policy, Upstream}
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

    test("without a model key the model is the stub; a blank key is refused, naming it") {
      val d = Deployments.accepted()
      (
        Secrets.of(db, d).map(_.keys.map(_.toString)),
        Secrets.of(db + ("OPENROUTER_API_KEY" -> "sk-or-1"), d).map(_.keys.map(_.toString)),
        Secrets.of(db + ("OPENROUTER_API_KEY" -> " "), d).map(_ => ()),
        Secrets.of(db + ("ANTHROPIC_API_KEY" -> ""), d).map(_ => ())
      ) ==> (
        Right(None),
        Right(Some("Keys(openRouter: set, anthropic: unset)")),
        Left(SecretsRefusal.Key(OpenRouterConfig.Invalid.Empty("OPENROUTER_API_KEY"))),
        Left(SecretsRefusal.Key(OpenRouterConfig.Invalid.Empty("ANTHROPIC_API_KEY")))
      )
    }

    test(
      "either model key alone serves a policy whose every pair it reaches; a pair whose key is unset is refused, naming the variable and the pair"
    ) {
      val direct = (for {
        id <- ModelId.of("anthropic/claude-haiku-5-5")
        turn = Assignment(ModelRef(id, Some(Upstream.AnthropicApi)), 100, None)
        d <- Deployments.of(policy = Policy.one(turn, turn, turn, turn)).toOption
      } yield d).getOrElse(sys.error("the direct deployment"))
      val viaOpenRouter = Deployments.accepted()
      val anthropic = db + ("ANTHROPIC_API_KEY" -> "sk-ant-1")
      val openRouter = db + ("OPENROUTER_API_KEY" -> "sk-or-1")
      def refusal(s: Either[SecretsRefusal, Secrets]): Option[String] =
        s.left.toOption.map(_.message)
      (
        refusal(Secrets.of(anthropic, direct)),
        refusal(Secrets.of(openRouter, viaOpenRouter)),
        refusal(Secrets.of(openRouter, direct)),
        refusal(Secrets.of(anthropic, viaOpenRouter))
      ) ==> (
        None,
        None,
        Some(
          "ANTHROPIC_API_KEY is not set, and anthropic/claude-haiku-5-5 @ api.anthropic.com is called with it"
        ),
        Some("OPENROUTER_API_KEY is not set, and openai/gpt-oss-120b is called with it")
      )
    }

    test(
      "the file area's directory is GRIT_FILES_DIR when set, an absolute path; unset, none is named"
    ) {
      val d = Deployments.accepted()
      (
        Secrets.of(db, d).map(_.files),
        Secrets.of(db + ("GRIT_FILES_DIR" -> "/var/cache/grit-files"), d).map(_.files),
        Secrets.of(db + ("GRIT_FILES_DIR" -> "cache/files"), d).map(_ => ())
      ) ==> (
        Right(None),
        Right(Some(java.nio.file.Path.of("/var/cache/grit-files"))),
        Left(SecretsRefusal.FilesDir("GRIT_FILES_DIR must be an absolute path"))
      )
    }
  }
}
