package grit.models

import java.net.URI
import java.time.Duration

/** How grit reaches OpenRouter. `toString` never shows the key. */
final case class OpenRouterConfig(
    apiKey: String,
    model: String,
    maxTokens: Int,
    endpoint: URI,
    timeout: Duration,
    routing: Routing = Routing.Open
) {
  override def toString: String =
    s"OpenRouterConfig($model, $maxTokens, $routing, $endpoint, <key redacted>)"
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
    case NotUpstreams(variable: String)
    case NotABoolean(variable: String)

    /** `strict` is `true` but `upstreams` pins nothing. */
    case StrictUnpinned(strict: String, upstreams: String)

    /** Names the variable, never its value. */
    def message: String = this match {
      case Missing(v) => s"$v is not set"
      case Empty(v) => s"$v is empty"
      case NotPositive(v) => s"$v is not a positive whole number"
      case NotUpstreams(v) =>
        s"$v is not a comma-separated list of OpenRouter upstream slugs, such as open-inference/fp8"
      case NotABoolean(v) => s"$v is neither true nor false"
      case StrictUnpinned(s, u) =>
        s"$s is true but $u is unset: strict schemas are enforced per upstream, so pin one"
    }
  }

  /** `role`'s configuration. The key is `OPENROUTER_API_KEY` (required) for every role.
    * The model is the role's variable; unset or blank, the turn uses [[DefaultModel]] and
    * every other role the turn's model. The output budget is the role's `max_tokens` variable,
    * or its default.
    *
    * The routing is the role's upstreams variable, a comma-separated list of [[Upstream]]
    * slugs ([[Routing.Pinned]], in that order), and its strict variable, `true` or `false`
    * (unset: `false`; `true` needs upstreams). Unset or blank upstreams are
    * [[Routing.Open]]. A role other than the turn that sets none of its model, upstreams
    * and strict variables routes as the turn does.
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
      routing <- routing(env, role)
    } yield OpenRouterConfig(
      key,
      model(env, role),
      maxTokens,
      Endpoint,
      Duration.ofMinutes(5),
      routing
    )

  private def model(env: Map[String, String], role: ModelRole): String =
    env.get(role.modelVar).filter(_.trim.nonEmpty).getOrElse {
      role match {
        case ModelRole.Turn => DefaultModel
        case ModelRole.Summary | ModelRole.Query => model(env, ModelRole.Turn)
      }
    }

  private def routing(env: Map[String, String], role: ModelRole): Either[Invalid, Routing] = {
    def set(variable: String): Option[String] = env.get(variable).map(_.trim).filter(_.nonEmpty)
    val own = Vector(role.modelVar, role.upstreamsVar, role.strictVar).exists(set(_).isDefined)
    if (role != ModelRole.Turn && !own) routing(env, ModelRole.Turn)
    else
      for {
        strict <- set(role.strictVar) match {
          case None | Some("false") => Right(false)
          case Some("true") => Right(true)
          case Some(_) => Left(Invalid.NotABoolean(role.strictVar))
        }
        upstreams <- set(role.upstreamsVar) match {
          case None => Right(Vector.empty[Upstream])
          case Some(list) =>
            val slugs = list.split(",", -1).toVector.map(s => Upstream.of(s.trim))
            if (slugs.forall(_.isDefined)) Right(slugs.flatten)
            else Left(Invalid.NotUpstreams(role.upstreamsVar))
        }
        routed <- upstreams match {
          case first +: rest => Right(Routing.Pinned(first, rest, strict))
          case _ if strict => Left(Invalid.StrictUnpinned(role.strictVar, role.upstreamsVar))
          case _ => Right(Routing.Open)
        }
      } yield routed
  }
}
