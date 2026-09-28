package grit.models

import java.time.LocalDate

import grit.core.model.{
  Assignment,
  Catalog,
  Effort,
  Known,
  ModelId,
  ModelRef,
  Policy,
  Profile,
  ReasoningReplay,
  Settings,
  Source,
  StrictSchemas,
  Upstream
}

import utest.*

object OpenRouterConfigTests extends TestSuite {

  private def ref(model: String, upstream: String*): ModelRef =
    ModelRef(
      ModelId.of(model).getOrElse(throw new java.lang.AssertionError(model)),
      upstream.headOption.map(u => Upstream.of(u).getOrElse(throw new java.lang.AssertionError(u)))
    )

  private val seedLike = Policy(
    Assignment(ref("big/one", "cerebras/fp16"), 4096, Some(Effort.Low)),
    Assignment(ref("small/one", "fireworks"), 1024, None),
    Assignment(ref("small/one", "fireworks"), 512, None),
    Assignment(ref("small/one", "fireworks"), 1024, None)
  )

  private def overlay(env: (String, String)*) = OpenRouterConfig.policy(env.toMap, seedLike)

  val tests = Tests {
    test("key: required and not blank; toString never shows it") {
      OpenRouterConfig.key(Map.empty) ==> Left(
        OpenRouterConfig.Invalid.Missing("OPENROUTER_API_KEY")
      )
      OpenRouterConfig.key(Map("OPENROUTER_API_KEY" -> " ")) ==>
        Left(OpenRouterConfig.Invalid.Empty("OPENROUTER_API_KEY"))
      OpenRouterConfig.key(Map("OPENROUTER_API_KEY" -> "sk-or-secret")) ==> Right("sk-or-secret")
      val shown =
        OpenRouterConfig.of("sk-or-secret", Catalog.of(seedLike, Vector.empty).pin.turn).toString
      assert(!shown.contains("sk-or-secret"))
    }

    test("policy: with no variables set, the policy stands") {
      overlay() ==> Right(seedLike)
      overlay("GRIT_MODEL" -> " ", "GRIT_SUMMARY_PROVIDER" -> "") ==> Right(seedLike)
    }

    test(
      "policy: a model variable names the model and leaves it open, unless an upstream is named too"
    ) {
      overlay("GRIT_MODEL" -> "x/y").map(_.turn) ==> Right(
        Assignment(ref("x/y"), 4096, Some(Effort.Low))
      )
      overlay("GRIT_MODEL" -> "x/y", "GRIT_PROVIDER" -> "coreweave").map(_.turn.ref) ==>
        Right(ref("x/y", "coreweave"))
      overlay("GRIT_PROVIDER" -> "coreweave").map(_.turn.ref) ==> Right(ref("big/one", "coreweave"))
      overlay("GRIT_MAX_TOKENS" -> "2000").map(_.turn) ==>
        Right(Assignment(ref("big/one", "cerebras/fp16"), 2000, Some(Effort.Low)))
    }

    test("policy: a role that names nothing follows the turn's variables; its own win") {
      val followed = overlay("GRIT_MODEL" -> "x/y")
      followed.map(p => (p.summary, p.query)) ==>
        Right((Assignment(ref("x/y"), 1024, None), Assignment(ref("x/y"), 512, None)))
      overlay("GRIT_MODEL" -> "x/y", "GRIT_QUERY_PROVIDER" -> "cerebras").map(p =>
        (p.summary.ref, p.query.ref)
      ) ==>
        Right((ref("x/y"), ref("small/one", "cerebras")))
      overlay("GRIT_SUMMARY_MODEL" -> "tiny/one", "GRIT_SUMMARY_MAX_TOKENS" -> "300").map(p =>
        (p.turn, p.summary, p.query)
      ) ==> Right((seedLike.turn, Assignment(ref("tiny/one"), 300, None), seedLike.query))
      // A budget variable alone is not naming a model: the role keeps its own pair.
      overlay("GRIT_MAX_TOKENS" -> "2000").map(_.summary) ==> Right(seedLike.summary)
    }

    test("policy: a bad variable is refused by name, never by value") {
      for (bad <- Seq("0", "-5", "lots", "99999999999"))
        overlay("GRIT_SUMMARY_MAX_TOKENS" -> bad) ==>
          Left(OpenRouterConfig.Invalid.NotPositive("GRIT_SUMMARY_MAX_TOKENS"))
      for (bad <- Seq("gpt-oss", "Open/AI", "a b/c"))
        overlay("GRIT_MODEL" -> bad) ==> Left(OpenRouterConfig.Invalid.NotAModel("GRIT_MODEL"))
      // One upstream per role: a list would fall back to another upstream's behaviour.
      for (bad <- Seq("open-inference/fp8, cerebras", "Open-Inference", "a/b/c", "/fp8"))
        overlay("GRIT_QUERY_PROVIDER" -> bad) ==>
          Left(OpenRouterConfig.Invalid.NotAnUpstream("GRIT_QUERY_PROVIDER"))
      val messages = Seq(
        OpenRouterConfig.Invalid.NotPositive("GRIT_MAX_TOKENS"),
        OpenRouterConfig.Invalid.NotAModel("GRIT_MODEL"),
        OpenRouterConfig.Invalid.NotAnUpstream("GRIT_PROVIDER")
      ).map(_.message)
      messages ==> Seq(
        "GRIT_MAX_TOKENS is not a positive whole number",
        "GRIT_MODEL is not an OpenRouter model id, such as openai/gpt-oss-120b",
        "GRIT_PROVIDER is not one OpenRouter upstream slug, such as open-inference/fp8"
      )
    }

    test("of: a role's model, budget, upstream and effort, and its pair's reasoning replay") {
      val nick = Source.Declared("nick", LocalDate.of(2026, 9, 25))
      val dropped = Profile(seedLike.turn.ref, replay = Known.Of(ReasoningReplay.Dropped, nick))
      val pin = Catalog.of(seedLike, Vector(dropped)).pin
      val t = OpenRouterConfig.of("k", pin.turn)
      (t.model, t.maxTokens, t.upstream.map(Upstream.value), t.effort, t.replay) ==>
        ("big/one", 4096, Some("cerebras/fp16"), Some(Effort.Low), ReasoningReplay.Dropped)
      val s = OpenRouterConfig.of("k", pin.summary)
      (s.model, s.maxTokens, s.upstream.map(Upstream.value), s.effort, s.replay) ==>
        ("small/one", 1024, Some("fireworks"), None, ReasoningReplay.Details)
    }

    test(
      "seed: every role is gpt-oss-120b at cerebras/fp16, a pair measured not to hold strict schemas"
    ) {
      val seed = Seed.catalog
      val gptOss = ref("openai/gpt-oss-120b", "cerebras/fp16")
      seed.map(_.policy) ==> Right(
        Policy(
          Assignment(gptOss, 4096, None),
          // The summary role's budget holds a reasoning model's thinking and the closing: at
          // 1024, deepseek-v4.1-flash was cut off in 5 of 10 closings.
          Assignment(gptOss, 4096, None),
          Assignment(gptOss, 1024, None),
          // Until a cheaper pair passes the probe on the closing's format.
          Assignment(gptOss, 4096, None)
        )
      )
      seed.map(c => Settings.of(c.profile(gptOss)).strict) ==> Right(StrictSchemas.Ignored)
      OpenRouterConfig
        .forRole(Map("OPENROUTER_API_KEY" -> "k"), ModelRole.Query)
        .map(c => (c.model, c.maxTokens, c.upstream.map(Upstream.value))) ==> Right(
        ("openai/gpt-oss-120b", 1024, Some("cerebras/fp16"))
      )
    }
  }
}
