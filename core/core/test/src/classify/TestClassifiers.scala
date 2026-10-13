package grit.core.classify

import grit.core.act.MovesFixtures.FakeDb
import grit.core.store.Db
import grit.core.visibility.{Clearance, Label, Level, Visibility}

/** Test fixture: a fake classifier as a deployment holds one, behind its clearance check. */
object TestClassifiers {

  /** The service a fake's states go to, `service:test`: cleared for public alone unless a
    * test's visibility clears it.
    */
  val Service: grit.core.place.Service =
    grit.core.place.Service.of("test").fold(e => throw new java.lang.AssertionError(e), identity)

  /** `classifier`, its states going to [[Service]]. */
  def cleared(classifier: Classifier^): ClearedClassifier^ =
    ClearedClassifier.of(Service, classifier)

  /** What [[above]]'s reads are classified at: internal. */
  val Above: Label = Label.at(Level.Internal)

  /** Reads at [[Above]] under `visibility`, which clears [[Service]] for public alone. */
  def above(visibility: Visibility = Visibility.Shipped): Db^ =
    new FakeDb(Clearance.of(Above), visibility)

  /** Why a fake behind [[cleared]] does not answer about what was read through [[above]]. */
  val Uncleared: String =
    s"its service, ${Service.place.written}, is not cleared for what was read, " +
      Label.shown(Above)
}
