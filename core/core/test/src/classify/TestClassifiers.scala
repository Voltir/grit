package grit.core.classify

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
}
