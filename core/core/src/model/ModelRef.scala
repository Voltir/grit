package grit.core.model

/** A model as OpenRouter names it in a request: an alias (`deepseek/deepseek-v4.1-flash`) or a
  * dated snapshot (`deepseek/deepseek-v4.1-flash-20260910`). An alias moves to newer snapshots
  * without notice and a response names the alias, never the snapshot that served it, so the
  * policy names a snapshot where one exists. `vendor/name`, lower-case letters, digits, `.`,
  * `_`, `-` and `:`.
  */
opaque type ModelId = String

object ModelId {

  private val Rule = "[a-z0-9][a-z0-9._-]*/[a-z0-9][a-z0-9._:-]*".r

  /** `id` as a model id, or `None` when it is not of the form above. */
  def of(id: String): Option[ModelId] = Option.when(Rule.matches(id))(id)

  def value(id: ModelId): String = id
}

/** An OpenRouter upstream's slug, with an optional variant: `open-inference/fp8`,
  * `fireworks`. Lower-case letters, digits, `.`, `_` and `-`, at most one `/`.
  */
opaque type Upstream = String

object Upstream {

  private val Slug = "[a-z0-9][a-z0-9._-]*(/[a-z0-9][a-z0-9._-]*)?".r

  /** `slug` as an upstream, or `None` when it is not of the form above. */
  def of(slug: String): Option[Upstream] = Option.when(Slug.matches(slug))(slug)

  def value(upstream: Upstream): String = upstream
}

/** `model` served by `upstream` alone, or by whichever upstream OpenRouter picks when `None`:
  * the pair a [[Profile]] describes. Upstreams differ in what they enforce, so the same model
  * at two upstreams is two pairs.
  */
final case class ModelRef(model: ModelId, upstream: Option[Upstream]) {
  override def toString: String =
    ModelId.value(model) + upstream.fold("")(u => s" @ ${Upstream.value(u)}")
}
