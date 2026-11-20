package grit.std.collect

/** Questions of a sequence that ask nothing of its elements but `==`. */
object Collect {

  /** The first element of `as` to occur a second time, by `==`: of `[a, b, b, a]`, `b`; `None`
    * when no element repeats.
    */
  def firstRepeat[A](as: Seq[A]): Option[A] = as.diff(as.distinct).headOption

  /** Each distinct element of `as` once, in order of first occurrence, with its count: of
    * `[a, b, a]`, `(a, 2)` then `(b, 1)`.
    */
  def counted[A](as: Seq[A]): Vector[(A, Int)] =
    as.distinct.map(a => (a, as.count(_ == a))).toVector

  /** Every index where `needle` starts in `hay`, in order, overlapping ones included: of `aa` in
    * `aaa`, 0 and 1. An empty `needle` is at none.
    */
  def positions(hay: String, needle: String): Vector[Int] =
    if (needle.isEmpty) Vector.empty
    else
      Iterator
        .iterate(hay.indexOf(needle))(i => hay.indexOf(needle, i + 1))
        .takeWhile(_ >= 0)
        .toVector
}
