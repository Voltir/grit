package grit.models

import java.net.URI
import java.time.Duration

import grit.core.secret.{Secret, Variable}

/** How grit reaches Jev (typesafe.ai's System One API). */
final case class JevConfig(apiKey: Secret, model: String, endpoint: URI, timeout: Duration) {
  override def toString: String = s"JevConfig($model, $endpoint, <key redacted>)"
}

object JevConfig {

  val KeyVar: Variable = Variable("JEV_API_KEY")

  /** The alias for TypeSafe's current stable release (`jev-1.13.0` on 2026-09-24). */
  val DefaultModel = "jev-latest"

  val Endpoint: URI = URI.create("https://api.typesafe.ai/v1/systemone")

  /** US dollars per million input tokens; output is not billed (docs, 2026-09-24). The API
    * reports tokens, not cost, so [[JevJson]] prices a request with this.
    */
  val UsdPerMillionInput: BigDecimal = BigDecimal("0.042")

  /** The key is `JEV_API_KEY` (required, as [[Secret.read]] reads it); the model is always
    * [[DefaultModel]].
    */
  def fromEnv(env: Map[String, String]): Either[Secret.Refusal, JevConfig] =
    Secret.read(env, KeyVar).map(JevConfig(_, DefaultModel, Endpoint, Duration.ofSeconds(30)))
}
