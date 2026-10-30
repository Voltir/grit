package grit.core.model

/** A model as OpenRouter names it in a request: an alias (`deepseek/deepseek-v4.1-flash`) or a
  * dated snapshot (`deepseek/deepseek-v4.1-flash-20260910`). An alias moves to newer snapshots
  * without notice and a response names the alias, never the snapshot that served it, so the
  * policy names a snapshot where one exists. At [[Upstream.AnthropicApi]], `anthropic/` and
  * the id Anthropic's own API names it by (`anthropic/claude-haiku-5-5`, where OpenRouter
  * writes `anthropic/claude-haiku-5.5`). `vendor/name`, lower-case letters, digits, `.`, `_`,
  * `-` and `:`.
  */
opaque type ModelId = String

object ModelId {

  private val Rule = "[a-z0-9][a-z0-9._-]*/[a-z0-9][a-z0-9._:-]*".r

  /** `id` as a model id, or `None` when it is not of the form above. */
  def of(id: String): Option[ModelId] = Option.when(Rule.matches(id))(id)

  def value(id: ModelId): String = id
}

/** An OpenRouter upstream's slug, with an optional variant: `open-inference/fp8`,
  * `fireworks`; or [[Upstream.AnthropicApi]], Anthropic's API reached directly. Lower-case
  * letters, digits, `.`, `_` and `-`, at most one `/`.
  */
opaque type Upstream = String

object Upstream {

  private val Slug = "[a-z0-9][a-z0-9._-]*(/[a-z0-9][a-z0-9._-]*)?".r

  /** `slug` as an upstream, or `None` when it is not of the form above. */
  def of(slug: String): Option[Upstream] = Option.when(Slug.matches(slug))(slug)

  def value(upstream: Upstream): String = upstream

  /** `api.anthropic.com`: Anthropic's Messages API, reached directly rather than through
    * OpenRouter. A pair it serves names its model `anthropic/` and Anthropic's own id
    * (`anthropic/claude-haiku-5-5`); OpenRouter's own slug for Anthropic is `anthropic`.
    */
  val AnthropicApi: Upstream = "api.anthropic.com"
}

/** `model` served by `upstream` alone, or by whichever upstream OpenRouter picks when `None`:
  * the pair a [[Profile]] describes. Upstreams differ in what they enforce, so the same model
  * at two upstreams is two pairs.
  */
final case class ModelRef(model: ModelId, upstream: Option[Upstream]) derives CanEqual {
  override def toString: String =
    ModelId.value(model) + upstream.fold("")(u => s" @ ${Upstream.value(u)}")
}
