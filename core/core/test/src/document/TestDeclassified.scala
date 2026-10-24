package grit.core.document

import grit.core.flow.TestDowngrades
import grit.core.id.{DocKey, DocumentVersionId, PluginName}
import grit.core.period.Probability
import grit.core.place.Place
import grit.core.visibility.Label

/** Test fixture: a declassified rewrite, as the declassification makes one, for suites outside
  * core that keep one.
  */
object TestDeclassified {

  /** `body`, a rewrite of version 1 of `plugin`'s document `key` at `label` and `place`, which
    * `read` reads, under `applied`, judged to leak nothing; throws when the declassification
    * makes none.
    */
  def of(
      body: String,
      plugin: String,
      key: String,
      place: Place,
      label: Label,
      read: Pointer,
      applied: PolicyApplication
  ): Declassified = {
    def got[A](e: Either[String, A]): A =
      e.fold(why => throw new java.lang.AssertionError(why), identity)
    TestDowngrades.raised(
      Declassified.declassification,
      (
        rewrite = got(DocText.of(body)),
        source = Declassified.Source(
          got(DocumentVersionId.of(1).toRight("a version")),
          got(PluginName.of(plugin)),
          got(DocKey.of(key)),
          place,
          label,
          read
        ),
        applied = applied,
        leak = Probability.Zero
      )
    ) match {
      case Some(d) => d
      case None => throw new java.lang.AssertionError(s"not declassified: $applied")
    }
  }
}
