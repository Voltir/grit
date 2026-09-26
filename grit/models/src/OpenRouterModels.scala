package grit.models

import grit.core.model.{Catalog, Pinned}
import grit.core.provider.{Models, Provider}

/** [[Models]] over OpenRouter, `inForce` the catalog for the run: each pin's calls are made
  * with `key` as [[OpenRouterConfig.of]] says. The providers for `inForce`'s own pins are made
  * once, here; any other pin gets a new provider on each call.
  */
final class OpenRouterModels(key: String, inForce: Catalog) extends Models {

  private val pins = inForce.pin
  private val turn: Provider = new OpenRouterProvider(OpenRouterConfig.of(key, pins.turn))
  private val summary: Provider = new OpenRouterProvider(OpenRouterConfig.of(key, pins.summary))
  private val query: Provider = new OpenRouterProvider(OpenRouterConfig.of(key, pins.query))

  def catalog(): Either[String, Catalog] = Right(inForce)

  def provider(pinned: Pinned): Provider^ =
    if (pinned == pins.turn) turn
    else if (pinned == pins.summary) summary
    else if (pinned == pins.query) query
    else new OpenRouterProvider(OpenRouterConfig.of(key, pinned))
}

/** [[Models]] that call nothing: every role's calls are the stub's, under [[StubModels.Catalog]];
  * the turn's are answered after `delayMs`.
  */
final class StubModels(delayMs: Long = 0) extends Models {

  private val turn = new StubProvider(delayMs)
  private val rest = new StubProvider()

  def catalog(): Either[String, Catalog] = Right(StubModels.Catalog)

  def provider(pinned: Pinned): Provider^ =
    if (pinned.assignment == StubModels.Catalog.policy.turn) turn else rest
}

object StubModels {

  /** Every role on the stub ([[StubProvider.Model]]), with the seed's budgets; no profiles. */
  val Catalog: Catalog = {
    val stub = grit.core.model.ModelRef(
      grit.core.model.ModelId.of(StubProvider.Model).getOrElse(sys.error("the stub's model id")),
      None
    )
    grit.core.model.Catalog.of(
      grit.core.model.Policy(
        grit.core.model.Assignment(stub, 4096, None),
        grit.core.model.Assignment(stub, 1024, None),
        grit.core.model.Assignment(stub, 1024, None)
      ),
      Vector.empty
    )
  }
}
