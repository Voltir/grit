package grit.models

import grit.core.message.{Cost, Tokens, Usage}
import grit.core.model.{Catalog, Known, ModelId, Pricing, Upstream}

/** What Anthropic bills a recorded call, which has no pin: the prices of the [[Seed]]'s profiles
  * of Anthropic's models at its API. A live call is priced by its pin's settings
  * ([[AnthropicJson.response]]). Pure.
  */
object AnthropicRates {

  /** What the seed's profile of `model` at Anthropic's API bills (`model` Anthropic's own id, or
    * a dated snapshot of one, `claude-haiku-5-5-20261001`, which takes its alias's); `None` for a
    * model it does not price.
    */
  def pricing(model: String): Option[Pricing] =
    priced.collectFirst { case (id, p) if AnthropicJson.isOrSnapshotOf(model, id) => p }

  /** What the thinking of `calls`, each a model's id and its usage, cost together: each call's
    * at [[Pricing.thinking]] under [[pricing]]; at least the priced ones' (`≥ 0` for none) where
    * a call's model has no rates or its thinking is not reported.
    */
  def thinkingCost(calls: Iterable[(String, Usage)]): Cost =
    calls.foldLeft(Cost.Zero) { case (sum, (model, usage)) =>
      sum + pricing(model)
        .flatMap(_.thinking(usage))
        .fold(Cost.AtLeast(BigDecimal(0)))(Cost.Exact(_))
    }

  /** What the thinking of calls on `model` whose usage together is `total` cost, when only the
    * total is known: at the first tier's rates, exact when every set of prompts of that total
    * takes the first tier ([[Pricing.oneTier]]), else at least that; `≥ 0` for a model with no
    * rates or no thinking reported. Where exact it is [[thinkingCost]] of the calls; elsewhere
    * below it.
    */
  def thinkingCostOfTotal(model: String, total: Usage): Cost =
    // A sum's input is no call's prompt, which is what picks a tier: a sum past a step may
    // hold only calls under it.
    thinkingCost(Vector(model -> total.copy(input = Tokens.Zero))) match {
      case Cost.Exact(usd) if !pricing(model).exists(_.oneTier(total.input)) => Cost.AtLeast(usd)
      case cost => cost
    }

  /** Each priced seed profile at Anthropic's API, by Anthropic's own id. An unreadable seed
    * prices nothing; `CatalogModelsTests` reads the seed, so a build with one fails there.
    */
  private lazy val priced: Vector[(String, Pricing)] =
    Seed.catalog.toOption.toVector.flatMap(pricedIn)

  /** Each priced profile of `catalog` at Anthropic's API, by Anthropic's own id; a profile of
    * another upstream, or of a model not Anthropic's, is not among them.
    */
  private[models] def pricedIn(catalog: Catalog): Vector[(String, Pricing)] =
    catalog.profiles.flatMap { p =>
      val id = ModelId.value(p.ref.model)
      val direct = p.ref.upstream.contains(Upstream.AnthropicApi) &&
        id.startsWith(AnthropicProvider.Vendor)
      p.pricing match {
        case Known.Of(pricing, _) if direct =>
          Some(id.stripPrefix(AnthropicProvider.Vendor) -> pricing)
        case _ => None
      }
    }
}
