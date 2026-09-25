package grit.models

import java.net.URI
import java.time.Duration

/** How grit reaches Jev (typesafe.ai's System One API). `toString` never shows the key. */
final case class JevConfig(apiKey: String, model: String, endpoint: URI, timeout: Duration) {
  override def toString: String = s"JevConfig($model, $endpoint, <key redacted>)"
}

object JevConfig {

  val KeyVar = "JEV_API_KEY"

  /** The alias for TypeSafe's current stable release (`jev-1.13.0` on 2026-09-24). */
  val DefaultModel = "jev-latest"

  val Endpoint: URI = URI.create("https://api.typesafe.ai/v1/systemone")

  /** US dollars per million input tokens; output is not billed (docs, 2026-09-24). The API
    * reports tokens, not cost, so [[JevJson]] prices a request with this.
    */
  val UsdPerMillionInput: BigDecimal = BigDecimal("0.042")

  /** The key is `JEV_API_KEY` (required); the model is always [[DefaultModel]]. */
  def fromEnv(env: Map[String, String]): Either[OpenRouterConfig.Invalid, JevConfig] =
    for {
      key <- env.get(KeyVar).toRight(OpenRouterConfig.Invalid.Missing(KeyVar))
      _ <- Either.cond(key.trim.nonEmpty, (), OpenRouterConfig.Invalid.Empty(KeyVar))
    } yield JevConfig(key, DefaultModel, Endpoint, Duration.ofSeconds(30))
}
