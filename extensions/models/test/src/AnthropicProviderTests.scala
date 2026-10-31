package grit.models

import grit.core.message.Message
import grit.core.model.{Assignment, CatalogVersion, Grade, ModelId, ModelRef, Pinned, Settings}
import grit.core.provider.{ModelRequest, ProviderError}

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

    test(
      "a Claude Haiku 5.5 request asks a mismatched thinking block dropped, under the binding beta; a Sonnet 5.5 one neither"
    ) {
      // A live call (AnthropicLiveProbe, 2026-10-29) found Haiku 5.5 refusing an edited history
      // without drop_block ("The block is bound to a different conversation") and answering
      // with it.
      val asked = ModelRequest("s", Vector(Message.User("hi")))
      def sent(model: String): Option[(Option[String], ujson.Value)] =
        pinned(s"anthropic/$model").map { p =>
          val (headers, body) = AnthropicProvider.wire("k", model, p, asked)
          (
            headers.collectFirst { case ("anthropic-beta", beta) => beta },
            body("thinking").obj.getOrElse("block_binding", ujson.Null)
          )
        }
      (sent("claude-haiku-5-5"), sent("claude-sonnet-5-5")) ==> (
        Some(
          (
            Some("thinking-binding-controls-2026-08-01"),
            ujson.Obj("prefix_mismatch_behavior" -> "drop_block")
          )
        ),
        Some((None, ujson.Null))
      )
    }

    test("a dated snapshot asks a mismatched thinking block dropped as its alias does") {
      val asked = ModelRequest("s", Vector(Message.User("hi")))
      val alias = "claude-haiku-5-5"
      val snapshot = s"$alias-20261001"
      def sent(model: String) =
        pinned(s"anthropic/$model").map(p => AnthropicProvider.wire("k", model, p, asked))
      // The alias's headers and body, sent under the snapshot's id.
      val asAlias = sent(alias).map { (headers, body) =>
        val renamed = ujson.copy(body)
        renamed("model") = snapshot
        (headers.toVector, renamed)
      }
      sent(snapshot).map((headers, body) => (headers.toVector, body)) ==> asAlias
    }
  }
}
