package grit.core.either

/** A vector of `Either`s turned inside out, failing at the first `Left`. */
object Traversal {

  /** Each of `as` through `f`, in order: every `Right`, or the first `Left`, after which `f` is
    * not called again.
    */
  def traverse[A, E, B](as: Vector[A])(f: A => Either[E, B]): Either[E, Vector[B]] =
    as.foldLeft[Either[E, Vector[B]]](Right(Vector.empty))((acc, a) =>
      acc.flatMap(bs => f(a).map(bs :+ _))
    )

  /** Every `Right` of `es`, in order, or the first `Left`. */
  def sequence[E, A](es: Vector[Either[E, A]]): Either[E, Vector[A]] = traverse(es)(identity)
}
