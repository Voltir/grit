package grit.models

import java.net.URI
import java.time.Duration

import grit.core.model.{
  Assignment,
  Effort,
  ModelId,
  ModelRef,
  Pinned,
  Policy,
  ReasoningReplay,
  Upstream
}

/** How grit reaches OpenRouter for one role: `upstream` alone serves its calls, or whichever
  * upstream OpenRouter picks when `None`; `effort` is asked of the model when set; earlier
  * reasoning goes back as `replay` says. `toString` never shows the key.
  */
final case class OpenRouterConfig(
    apiKey: String,
    model: String,
    maxTokens: Int,
    endpoint: URI,
    timeout: Duration,
    upstream: Option[Upstream] = None,
    effort: Option[Effort] = None,
    replay: ReasoningReplay = ReasoningReplay.Details
) {
  override def toString: String =
    s"OpenRouterConfig($model, $maxTokens, ${upstream.fold("open")(Upstream.value)}, $endpoint, <key redacted>)"
}

object OpenRouterConfig {

  val KeyVar = "OPENROUTER_API_KEY"

  val Endpoint: URI = URI.create("https://openrouter.ai/api/v1/chat/completions")

  enum Invalid {
    case Missing(variable: String)
    case Empty(variable: String)
    case NotPositive(variable: String)
    case NotAModel(variable: String)
    case NotAnUpstream(variable: String)

    /** Names the variable, never its value. */
    def message: String = this match {
      case Missing(v) => s"$v is not set"
      case Empty(v) => s"$v is empty"
      case NotPositive(v) => s"$v is not a positive whole number"
      case NotAModel(v) => s"$v is not an OpenRouter model id, such as openai/gpt-oss-120b"
      case NotAnUpstream(v) =>
        s"$v is not one OpenRouter upstream slug, such as open-inference/fp8"
    }
  }

  /** The key, `OPENROUTER_API_KEY`: required, and not blank. */
  def key(env: Map[String, String]): Either[Invalid, String] =
    for {
      key <- env.get(KeyVar).toRight(Invalid.Missing(KeyVar))
      _ <- Either.cond(key.trim.nonEmpty, (), Invalid.Empty(KeyVar))
    } yield key

  /** `policy` with each role's variables ([[ModelRole]]) laid over its assignment for this run;
    * a blank variable is unset. A role's model variable names its model and leaves it open
    * unless its upstream variable names one upstream; its upstream variable alone keeps its
    * model. A summary or query role that sets neither follows the turn's model and upstream
    * whenever the turn's variables set either. A budget variable replaces only the budget.
    */
  def policy(env: Map[String, String], policy: Policy): Either[Invalid, Policy] = {
    def set(variable: String): Option[String] = env.get(variable).map(_.trim).filter(_.nonEmpty)
    def names(role: ModelRole) = set(role.modelVar).isDefined || set(role.upstreamVar).isDefined

    def ref(role: ModelRole, own: ModelRef): Either[Invalid, ModelRef] =
      for {
        model <- set(role.modelVar) match {
          case None => Right(None)
          case Some(m) => ModelId.of(m).map(Some(_)).toRight(Invalid.NotAModel(role.modelVar))
        }
        upstream <- set(role.upstreamVar) match {
          case None => Right(None)
          case Some(u) =>
            Upstream.of(u).map(Some(_)).toRight(Invalid.NotAnUpstream(role.upstreamVar))
        }
      } yield (model, upstream) match {
        case (None, None) => own
        case (Some(m), u) => ModelRef(m, u)
        case (None, Some(u)) => own.copy(upstream = Some(u))
      }

    def assignment(role: ModelRole, own: Assignment, turn: ModelRef): Either[Invalid, Assignment] =
      for {
        r <-
          if (role != ModelRole.Turn && !names(role) && names(ModelRole.Turn)) Right(turn)
          else ref(role, own.ref)
        max <- set(role.maxTokensVar) match {
          case None => Right(own.maxTokens)
          case Some(raw) =>
            raw.toIntOption.filter(_ > 0).toRight(Invalid.NotPositive(role.maxTokensVar))
        }
      } yield own.copy(ref = r, maxTokens = max)

    for {
      turnRef <- ref(ModelRole.Turn, policy.turn.ref)
      turn <- assignment(ModelRole.Turn, policy.turn, turnRef)
      summary <- assignment(ModelRole.Summary, policy.summary, turnRef)
      query <- assignment(ModelRole.Query, policy.query, turnRef)
    } yield Policy(turn, summary, query, policy.heard)
  }

  /** A role's configuration under `key`: its pinned assignment's model, budget, upstream and
    * effort, its settings' reasoning replay, and a five-minute timeout.
    */
  def of(key: String, pinned: Pinned): OpenRouterConfig = {
    val a = pinned.assignment
    OpenRouterConfig(
      key,
      ModelId.value(a.ref.model),
      a.maxTokens,
      Endpoint,
      Duration.ofMinutes(5),
      a.ref.upstream,
      a.effort,
      pinned.settings.replay
    )
  }

  /** `role`'s configuration for this run: the key, and the [[Seed]]'s policy with `env` laid
    * over it ([[policy]]); `Left` says what is wrong, naming no value.
    */
  def forRole(env: Map[String, String], role: ModelRole): Either[String, OpenRouterConfig] =
    for {
      k <- key(env).left.map(_.message)
      seed <- Seed.catalog
      p <- policy(env, seed.policy).left.map(_.message)
    } yield of(k, role.in(seed.withPolicy(p).pin))
}
