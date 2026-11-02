package grit.models

import grit.core.secret.{Named, Secret, Variable}

import utest.*

/** [[Keys.of]]: the gateways' keys, read from the environment as secrets. */
object KeysTests extends TestSuite {

  val tests = Tests {
    test(
      "every gateway's key variable is named like a secret, so reading one is never refused for its name"
    ) {
      Vector(OpenRouterConfig.KeyVar, AnthropicProvider.KeyVar, JevConfig.KeyVar)
        .map(v => Variable.value(v) -> Named.variable(Variable.value(v))) ==> Vector(
        "OPENROUTER_API_KEY" -> true,
        "ANTHROPIC_API_KEY" -> true,
        "JEV_API_KEY" -> true
      )
    }

    test(
      "an unset key is no key and a blank one is refused naming its variable; none set is the stub"
    ) {
      (
        Keys.of(Map.empty),
        Keys.of(Map("OPENROUTER_API_KEY" -> " \n")),
        Keys.of(Map("ANTHROPIC_API_KEY" -> " sk-ant-1 ")).map(_.map(_.toString))
      ) ==> (
        Right(None),
        Left(Secret.Refusal.Blank(OpenRouterConfig.KeyVar)),
        Right(Some("Keys(openRouter: unset, anthropic: set)"))
      )
    }
  }
}
