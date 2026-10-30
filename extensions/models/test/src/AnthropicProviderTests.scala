package grit.models

import grit.core.model.{Assignment, CatalogVersion, Grade, ModelId, ModelRef, Pinned, Settings}
import grit.core.provider.ProviderError

import utest.*

object AnthropicProviderTests extends TestSuite {

  private def pinned(model: String): Option[Pinned] =
    ModelId.of(model).map { id =>
      Pinned(
        CatalogVersion("v"),
        Grade.Standard,
        Assignment(ModelRef(id, None), 64, None),
        Settings.of(None)
      )
    }

  val tests = Tests {
    test("a model under anthropic/ is called by the id after it; any other is refused") {
      pinned("anthropic/claude-haiku-5-5").map(AnthropicProvider.modelOf) ==>
        Some(Right("claude-haiku-5-5"))
      pinned("openai/claude-haiku-5-5").map(AnthropicProvider.modelOf) ==> Some(
        Left(
          ProviderError.Refused(
            "openai/claude-haiku-5-5 is not one of Anthropic's models (anthropic/...)"
          )
        )
      )
    }
  }
}
