package grit.models

import java.time.LocalDate

import grit.core.model.{
  Assignment,
  Catalog,
  Effort,
  Grade,
  Known,
  Ladder,
  ModelId,
  ModelRef,
  Policy,
  Profile,
  ReasoningReplay,
  Rung,
  Source,
  Upstream
}

import utest.*

object OpenRouterConfigTests extends TestSuite {

  private def ref(model: String, upstream: String*): ModelRef =
    ModelRef(
      ModelId.of(model).getOrElse(throw new java.lang.AssertionError(model)),
      upstream.headOption.map(u => Upstream.of(u).getOrElse(throw new java.lang.AssertionError(u)))
    )

  private val seedTurn = Assignment(ref("big/one", "cerebras/fp16"), 4096, Some(Effort.Low))
  private val seedSummary = Assignment(ref("small/one", "fireworks"), 1024, None)
  private val seedQuery = Assignment(ref("small/one", "fireworks"), 512, None)
  private val seedLike = Policy.one(
    seedTurn,
    seedSummary,
    seedQuery,
    Assignment(ref("small/one", "fireworks"), 1024, None)
  )

  private def overlay(env: (String, String)*) = OpenRouterConfig.policy(env.toMap, seedLike)

  /** Each role's ladder in `p`, as its rungs' grades and assignments. */
  private def rungs(p: Policy) =
    Vector(p.turn, p.summary, p.query).map(_.rungs.map(r => (r.grade, r.assignment)))

  /** A role of one standard rung: its assignment. */
  private def one(l: Ladder): Option[Assignment] =
    Option.when(l.rest.isEmpty && l.first.grade == Grade.Standard)(l.first.assignment)

  val tests = Tests {
    test("key: required and not blank; toString never shows it") {
      OpenRouterConfig.key(Map.empty) ==> Left(
        OpenRouterConfig.Invalid.Missing("OPENROUTER_API_KEY")
      )
      OpenRouterConfig.key(Map("OPENROUTER_API_KEY" -> " ")) ==>
        Left(OpenRouterConfig.Invalid.Empty("OPENROUTER_API_KEY"))
      OpenRouterConfig.key(Map("OPENROUTER_API_KEY" -> "sk-or-secret")) ==> Right("sk-or-secret")
      val shown =
        OpenRouterConfig
          .of("sk-or-secret", Catalog.of(seedLike, Vector.empty).pin.turn.first)
          .toString
      assert(!shown.contains("sk-or-secret"))
    }

    test("policy: with no variables set, the policy stands") {
      overlay() ==> Right(seedLike)
      overlay("GRIT_MODEL" -> " ", "GRIT_SUMMARY_PROVIDER" -> "") ==> Right(seedLike)
    }

    test(
      "policy: a model variable names the model and leaves it open, unless an upstream is named too"
    ) {
      overlay("GRIT_MODEL" -> "x/y").map(p => one(p.turn)) ==> Right(
        Some(
          Assignment(ref("x/y"), 4096, Some(Effort.Low))
        )
      )
      overlay("GRIT_MODEL" -> "x/y", "GRIT_PROVIDER" -> "coreweave").map(p =>
        one(p.turn).map(_.ref)
      ) ==>
        Right(Some(ref("x/y", "coreweave")))
      overlay("GRIT_PROVIDER" -> "coreweave").map(p => one(p.turn).map(_.ref)) ==>
        Right(Some(ref("big/one", "coreweave")))
      overlay("GRIT_MAX_TOKENS" -> "2000").map(p => one(p.turn)) ==>
        Right(Some(Assignment(ref("big/one", "cerebras/fp16"), 2000, Some(Effort.Low))))
    }

    test("policy: a role that names nothing follows the turn's variables; its own win") {
      val followed = overlay("GRIT_MODEL" -> "x/y")
      followed.map(p => (one(p.summary), one(p.query))) ==>
        Right((Some(Assignment(ref("x/y"), 1024, None)), Some(Assignment(ref("x/y"), 512, None))))
      overlay("GRIT_MODEL" -> "x/y", "GRIT_QUERY_PROVIDER" -> "cerebras").map(p =>
        (one(p.summary).map(_.ref), one(p.query).map(_.ref))
      ) ==>
        Right((Some(ref("x/y")), Some(ref("small/one", "cerebras"))))
      overlay("GRIT_SUMMARY_MODEL" -> "tiny/one", "GRIT_SUMMARY_MAX_TOKENS" -> "300").map(p =>
        (p.turn, one(p.summary), p.query)
      ) ==> Right((seedLike.turn, Some(Assignment(ref("tiny/one"), 300, None)), seedLike.query))
      // A budget variable alone is not naming a model: the role keeps its own pair.
      overlay("GRIT_MAX_TOKENS" -> "2000").map(_.summary) ==> Right(seedLike.summary)
    }

    test(
      "policy: a grade's variables add a rung after the role's others, with its first rung's budget"
    ) {
      overlay(
        "GRIT_STRONG_MODEL" -> "big/two",
        "GRIT_STRONG_PROVIDER" -> "anthropic",
        "GRIT_SUMMARY_LIGHT_MODEL" -> "tiny/one",
        "GRIT_MAX_TOKENS" -> "2000"
      ).map(rungs) ==> Right(
        Vector(
          Vector(
            Grade.Standard -> seedTurn.copy(maxTokens = 2000),
            Grade.Strong -> Assignment(ref("big/two", "anthropic"), 2000, Some(Effort.Low))
          ),
          Vector(
            Grade.Standard -> seedSummary,
            Grade.Light -> seedSummary.copy(ref = ref("tiny/one"))
          ),
          Vector(Grade.Standard -> seedQuery)
        )
      )
      // A graded variable is not a standard one: the summary does not follow it.
      overlay("GRIT_STRONG_MODEL" -> "big/two").map(p => one(p.summary)) ==> Right(
        Some(seedSummary)
      )
    }

    test("policy: a grade's variables lay over the role's rungs at that grade, and only those") {
      val laddered = seedLike.copy(turn =
        Ladder
          .of(Vector(Rung(Grade.Standard, seedTurn), Rung(Grade.Strong, seedSummary)))
          .getOrElse(throw new java.lang.AssertionError("ladder"))
      )
      OpenRouterConfig
        .policy(Map("GRIT_STRONG_PROVIDER" -> "coreweave"), laddered)
        .map(p => p.turn.rungs.map(_.assignment.ref)) ==>
        Right(Vector(seedTurn.ref, ref("small/one", "coreweave")))
      // An upstream with no rung at its grade has no model to keep.
      overlay("GRIT_QUERY_STRONG_PROVIDER" -> "coreweave") ==>
        Left(OpenRouterConfig.Invalid.Missing("GRIT_QUERY_STRONG_MODEL"))
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
      val dropped = Profile(seedTurn.ref, replay = Known.Of(ReasoningReplay.Dropped, nick))
      val pin = Catalog.of(seedLike, Vector(dropped)).pin
      val t = OpenRouterConfig.of("k", pin.turn.first)
      (t.model, t.maxTokens, t.upstream.map(Upstream.value), t.effort, t.replay) ==>
        ("big/one", 4096, Some("cerebras/fp16"), Some(Effort.Low), ReasoningReplay.Dropped)
      val s = OpenRouterConfig.of("k", pin.summary.first)
      (s.model, s.maxTokens, s.upstream.map(Upstream.value), s.effort, s.replay) ==>
        ("small/one", 1024, Some("fireworks"), None, ReasoningReplay.Details)
    }

    test(
      "policy: the heard role reads its own variables; naming neither, it follows the summary's"
    ) {
      overlay(
        "GRIT_HEARD_MODEL" -> "h/m",
        "GRIT_HEARD_PROVIDER" -> "groq",
        "GRIT_HEARD_MAX_TOKENS" -> "700"
      )
        .map(p => one(p.heard)) ==> Right(Some(Assignment(ref("h/m", "groq"), 700, None)))
      overlay("GRIT_SUMMARY_MODEL" -> "s/m", "GRIT_SUMMARY_PROVIDER" -> "groq")
        .map(p => one(p.heard).map(_.ref)) ==> Right(Some(ref("s/m", "groq")))
      // The summary follows the turn, so the heard role does too, through it.
      overlay("GRIT_MODEL" -> "x/y", "GRIT_PROVIDER" -> "cerebras").map(p => one(p.heard)) ==>
        Right(Some(Assignment(ref("x/y", "cerebras"), 1024, None)))
      overlay("GRIT_MODEL" -> "x/y", "GRIT_HEARD_PROVIDER" -> "groq")
        .map(p => one(p.heard).map(_.ref)) ==> Right(Some(ref("small/one", "groq")))
    }
    test(
      "policy: the dream role reads its own variables; naming neither, it follows the summary's"
    ) {
      overlay(
        "GRIT_DREAM_MODEL" -> "d/m",
        "GRIT_DREAM_PROVIDER" -> "groq",
        "GRIT_DREAM_MAX_TOKENS" -> "900"
      )
        .map(p => one(p.dream)) ==> Right(Some(Assignment(ref("d/m", "groq"), 900, None)))
      overlay("GRIT_SUMMARY_MODEL" -> "s/m", "GRIT_SUMMARY_PROVIDER" -> "groq")
        .map(p => one(p.dream).map(_.ref)) ==> Right(Some(ref("s/m", "groq")))
      overlay("GRIT_MODEL" -> "x/y", "GRIT_PROVIDER" -> "cerebras").map(p => one(p.dream)) ==>
        Right(Some(Assignment(ref("x/y", "cerebras"), 1024, None)))
    }
    test("forRole: each role runs on its own variables, over the seed's budgets") {
      val env = Map(
        "OPENROUTER_API_KEY" -> "k",
        "GRIT_MODEL" -> "t/m",
        "GRIT_SUMMARY_MODEL" -> "s/m",
        "GRIT_QUERY_MODEL" -> "q/m",
        "GRIT_HEARD_MODEL" -> "h/m",
        "GRIT_DREAM_MODEL" -> "d/m"
      )
      ModelRole.values.toSeq.map(role =>
        OpenRouterConfig.forRole(env, role).map(c => (c.model, c.maxTokens, c.upstream))
      ) ==> Seq(
        Right(("t/m", 4096, None)),
        Right(("s/m", 4096, None)),
        Right(("q/m", 1024, None)),
        Right(("h/m", 4096, None)),
        Right(("d/m", 4096, None))
      )
      // With nothing set but the key, the query runs on the seed's own budget.
      OpenRouterConfig
        .forRole(Map("OPENROUTER_API_KEY" -> "k"), ModelRole.Query)
        .map(_.maxTokens) ==> Right(1024)
    }
  }
}
