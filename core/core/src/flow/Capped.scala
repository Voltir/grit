package grit.core.flow

/** At most `N` values. */
opaque type Capped[+T, N <: Int] = Vector[T]

object Capped {

  /** `values`, or `None` when there are more than `N`. */
  def of[T, N <: Int](values: Vector[T])(using n: ValueOf[N]): Option[Capped[T, N]] =
    Option.when(values.size <= n.value)(values)

  extension [T, N <: Int](c: Capped[T, N]) {
    def values: Vector[T] = c
  }
}
