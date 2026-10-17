package grit.models

import grit.core.model.Catalog
import grit.core.store.{Db, ModelSettingStore}
import grit.core.visibility.Subject

/** The catalog in force over a seed: what every [[grit.core.provider.Models]] over a real
  * catalog answers.
  */
object KeptCatalog {

  /** `seed` with every setting kept in `settings` laid over it, in the order kept, read
    * through `db` now; `Left` when the kept settings cannot be read.
    */
  def read(seed: Catalog, db: Db^, settings: ModelSettingStore): Either[String, Catalog] =
    db.read(Subject.Public)(settings.all())
      .map(kept => seed.overlaid(kept.map(_.settings)))
      .left
      .map(e => s"the kept model settings cannot be read: $e")
}
