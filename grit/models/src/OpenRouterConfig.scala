package grit.models

import java.net.URI
import java.time.Duration

/** How grit reaches OpenRouter. `toString` never shows the key. */
final case class OpenRouterConfig(
    apiKey: String,
    model: String,
    maxTokens: Int,
    endpoint: URI,
    timeout: Duration
) {
  override def toString: String = s"OpenRouterConfig($model, $maxTokens, $endpoint, <key redacted>)"
}

object OpenRouterConfig {

  val KeyVar = "OPENROUTER_API_KEY"

  /** The cheapest tool-capable model on OpenRouter as of 2026-09-23, and it reasons, so the
    * `reasoning_details` round trip is exercised.
    */
  val DefaultModel = "openai/gpt-oss-20b"

  val Endpoint: URI = URI.create("https://openrouter.ai/api/v1/chat/completions")

  enum Invalid {
    case Missing(variable: String)
    case Empty(variable: String)
    case NotPositive(variable: String)

    /** Names the variable, never its value. */
    def message: String = this match {
      case Missing(v) => s"$v is not set"
      case Empty(v) => s"$v is empty"
      case NotPositive(v) => s"$v is not a positive whole number"
    }
  }

  /** `role`'s configuration. The key is `OPENROUTER_API_KEY` (required) for every role.
    * The model is the role's variable; unset or blank, the turn uses [[DefaultModel]] and
    * every other role the turn's model. The output budget is the role's `max_tokens` variable,
    * or its default.
    */
  def fromEnv(env: Map[String, String], role: ModelRole): Either[Invalid, OpenRouterConfig] =
    for {
      key <- env.get(KeyVar).toRight(Invalid.Missing(KeyVar))
      _ <- Either.cond(key.trim.nonEmpty, (), Invalid.Empty(KeyVar))
      maxTokens <- env.get(role.maxTokensVar) match {
        case None => Right(role.defaultMaxTokens)
        case Some(raw) =>
          raw.trim.toIntOption.filter(_ > 0).toRight(Invalid.NotPositive(role.maxTokensVar))
      }
    } yield OpenRouterConfig(key, model(env, role), maxTokens, Endpoint, Duration.ofMinutes(5))

  private def model(env: Map[String, String], role: ModelRole): String =
    env.get(role.modelVar).filter(_.trim.nonEmpty).getOrElse {
      role match {
        case ModelRole.Turn => DefaultModel
        case ModelRole.Summary | ModelRole.Query => model(env, ModelRole.Turn)
      }
    }
}
