package grit.std.collect

/** Questions of a sequence that ask nothing of its elements but `==`. */
object Collect {

  /** The first element of `as` to occur a second time, by `==`: of `[a, b, b, a]`, `b`; `None`
    * when no element repeats.
    */
  def firstRepeat[A](as: Seq[A]): Option[A] = as.diff(as.distinct).headOption
}
