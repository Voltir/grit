package grit.models

import grit.core.model.{Catalog, Pinned}
import grit.core.provider.{Models, Provider}
import grit.core.store.{Db, ModelSettingStore}

/** [[Models]] over the catalog: the catalog in force is `seed` with every setting kept in
  * `settings` laid over it, read through `db` each time it is asked for
  * ([[KeptCatalog.read]]). Each pin's calls go through the gateway that serves its pair, with
  * that gateway's key, as [[Keys.provider]] says; the providers for `seed`'s own pins are
  * made once, here, and any other pin gets a new provider on each call.
  */
final class CatalogModels(keys: Keys, seed: Catalog, db: Db^, settings: ModelSettingStore)
    extends Models {

  private val pins = seed.pin
  private val turn: Provider = keys.provider(pins.turn.first)
  private val summary: Provider = keys.provider(pins.summary.first)
  private val query: Provider = keys.provider(pins.query.first)

  def catalog(): Either[String, Catalog] = KeptCatalog.read(seed, db, settings)

  def provider(pinned: Pinned): Provider^ =
    if (pinned == pins.turn.first) turn
    else if (pinned == pins.summary.first) summary
    else if (pinned == pins.query.first) query
    else keys.provider(pinned)
}

/** [[Models]] that call nothing: every role's calls are the stub's, under [[StubModels.Catalog]];
  * the turn's are answered after `delayMs`.
  */
final class StubModels(delayMs: Long = 0) extends Models {

  private val turn = new StubProvider(delayMs)
  private val rest = new StubProvider()

  def catalog(): Either[String, Catalog] = Right(StubModels.Catalog)

  def provider(pinned: Pinned): Provider^ =
    if (pinned.assignment == StubModels.Catalog.policy.turn.first.assignment) turn else rest
}

object StubModels {

  /** The stub's pair: [[StubProvider.Model]] at an upstream named `stub`, so a deployment can
    * clear it as a party (ADR 0031); nothing is sent outside grit.
    */
  val Ref: grit.core.model.ModelRef =
    grit.core.model.ModelRef(
      grit.core.model.ModelId.of(StubProvider.Model).getOrElse(sys.error("the stub's model id")),
      grit.core.model.Upstream.of("stub")
    )

  /** Every role on the stub ([[Ref]]), with the seed's budgets; no profiles. */
  val Catalog: Catalog = {
    val stub = Ref
    grit.core.model.Catalog.of(
      grit.core.model.Policy.one(
        grit.core.model.Assignment(stub, 4096, None),
        grit.core.model.Assignment(stub, 1024, None),
        grit.core.model.Assignment(stub, 1024, None),
        grit.core.model.Assignment(stub, 1024, None)
      ),
      Vector.empty
    )
  }
}
