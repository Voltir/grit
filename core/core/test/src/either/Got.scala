package grit.core.either

/** A value a test expects present. */
object Got {

  /** `e`'s right, or the test failed with its left. */
  def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), a => a)
}
