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
  val ModelVar = "GRIT_MODEL"

  /** The cheapest tool-capable model on OpenRouter as of 2026-09-23, and it reasons, so the
    * `reasoning_details` round trip is exercised.
    */
  val DefaultModel = "openai/gpt-oss-20b"

  val DefaultMaxTokens = 4096

  val Endpoint: URI = URI.create("https://openrouter.ai/api/v1/chat/completions")

  enum Invalid {
    case Missing(variable: String)
    case Empty(variable: String)

    /** Names the variable, never its value. */
    def message: String = this match {
      case Missing(v) => s"$v is not set"
      case Empty(v) => s"$v is empty"
    }
  }

  /** The key from `OPENROUTER_API_KEY` (required), the model from `GRIT_MODEL` (default
    * [[DefaultModel]]).
    */
  def fromEnv(env: Map[String, String]): Either[Invalid, OpenRouterConfig] =
    for {
      key <- env.get(KeyVar).toRight(Invalid.Missing(KeyVar))
      _ <- Either.cond(key.trim.nonEmpty, (), Invalid.Empty(KeyVar))
      model = env.get(ModelVar).filter(_.trim.nonEmpty).getOrElse(DefaultModel)
    } yield OpenRouterConfig(key, model, DefaultMaxTokens, Endpoint, Duration.ofMinutes(5))
}
