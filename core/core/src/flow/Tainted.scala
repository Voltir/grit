package grit.core.flow

import grit.core.visibility.Integrity

/** A value not yet checked, believed at most at `integrity`: a tool's result, a guest's words,
  * the model's reply after reading either. Stays unchecked through any pure computation and any
  * combination. Leaves only through [[Trusted.check]] and [[Trusted.endorse]].
  */
final class Tainted[+T] private[flow] (value: T, val integrity: Integrity) {

  def map[U](f: T -> U): Tainted[U] = new Tainted(f(value), integrity)

  /** Both, at the meet of their integrities. */
  def zip[U](other: Tainted[U]): Tainted[(T, U)] =
    new Tainted((value, other.open), integrity.meet(other.integrity))

  private[flow] def open: T = value
}
