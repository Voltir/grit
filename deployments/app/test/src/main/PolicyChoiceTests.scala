package grit.app.main

import utest.*

/** The model policy a run reads from its environment ([[Deploying.policy]]). */
object PolicyChoiceTests extends TestSuite {

  private val haiku =
    Map("GRIT_MODEL" -> "anthropic/claude-haiku-5-5", "GRIT_PROVIDER" -> "api.anthropic.com")

  private def turnPair(env: Map[String, String]): Either[String, String] =
    Deploying.policy(env).map(_.turn.first.assignment.ref.toString)

  val tests: Tests = Tests {

    test(
      "the roles' variables lay over the seed policy with Anthropic's key alone, or with no key, as with OpenRouter's"
    ) {
      (
        turnPair(haiku + ("ANTHROPIC_API_KEY" -> "k")),
        turnPair(haiku),
        turnPair(haiku + ("OPENROUTER_API_KEY" -> "k"))
      ) ==> (
        Right("anthropic/claude-haiku-5-5 @ api.anthropic.com"),
        Right("anthropic/claude-haiku-5-5 @ api.anthropic.com"),
        Right("anthropic/claude-haiku-5-5 @ api.anthropic.com")
      )
    }
  }
}
