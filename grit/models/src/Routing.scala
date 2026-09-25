package grit.models

/** Which of OpenRouter's upstreams may serve a role's calls. Whether an upstream holds a
  * model to a `strict` tool schema is a property of that upstream, not of the model, and a
  * fallback to another upstream drops it without saying so
  * (`.local/backlog/tool-decoding-findings-2026-09-25.md`).
  */
enum Routing {

  /** OpenRouter picks the upstream, and falls back to another when one fails. */
  case Open

  /** Only `first`, then each of `rest`, in that order; never another, so a call fails when
    * none of them can serve it. `strict`: this role's tool schemas are to be built strict
    * (`ToolSpec.schema(strict = true)`); an upstream that does not enforce them ignores it.
    */
  case Pinned(first: Upstream, rest: Vector[Upstream], strict: Boolean)

  /** Whether this role's tool schemas are to be built strict: never unless pinned. */
  def strictTools: Boolean = this match {
    case Open => false
    case Pinned(_, _, strict) => strict
  }
}

/** An OpenRouter upstream's slug, with an optional variant: `open-inference/fp8`,
  * `coreweave`. Lower-case letters, digits, `.`, `_` and `-`, at most one `/`.
  */
opaque type Upstream = String

object Upstream {

  private val Slug = "[a-z0-9][a-z0-9._-]*(/[a-z0-9][a-z0-9._-]*)?".r

  /** `slug` as an upstream, or `None` when it is not of the form above. */
  def of(slug: String): Option[Upstream] = Option.when(Slug.matches(slug))(slug)

  def value(upstream: Upstream): String = upstream
}
