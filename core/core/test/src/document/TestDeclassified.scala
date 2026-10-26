package grit.core.document

import grit.core.classify.Answer
import grit.core.flow.TestDowngrades
import grit.core.id.{DocKey, DocumentVersionId, PluginName}
import grit.core.place.Place
import grit.core.screen.{Must, Screen}
import grit.core.visibility.Label

/** Test fixture: a declassified rewrite, as the declassification makes one, for suites outside
  * core that keep one.
  */
object TestDeclassified {

  /** `body`, a rewrite of version 1 of `plugin`'s document `key` at `label` and `place`, which
    * `read` reads, its file at `revision`, under `applied`, on its `attempt`, its screen of one
    * absent signal answering no to everything; throws when the declassification makes none.
    */
  def of(
      body: String,
      plugin: String,
      key: String,
      place: Place,
      label: Label,
      read: Pointer,
      revision: String,
      applied: PolicyApplication,
      attempt: Declassified.Attempt = Declassified.Attempt.First
  ): Declassified = {
    def got[A](e: Either[String, A]): A =
      e.fold(why => throw new java.lang.AssertionError(why), identity)
    val screened = Screen
      .read(Screen.Shape(0, Vector(Must.Absent)), Vector(Answer.YesNo(0.0)))
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)
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
          read,
          revision
        ),
        applied = applied,
        screened = screened,
        baseline = None,
        attempt = attempt
      )
    ) match {
      case Some(d) => d
      case None => throw new java.lang.AssertionError(s"not declassified: $applied")
    }
  }
}
