package grit.std.either

/** An `Either` whose `Left` the source or build calling it rules out. */
object Proven {

  /** `proven`'s right. Its `Left` is a bug in the caller's source or build, not a failure a
    * caller handles: it is thrown as an `IllegalStateException` whose message is
    * `"ruled out, yet: " + left`. A call states beside it why its `Left` cannot be taken.
    */
  def right[A](proven: Either[String, A]): A =
    proven.fold(left => throw new IllegalStateException(s"ruled out, yet: $left"), identity)
}
