package grit.core.flow

import scala.compiletime.ops.int.<=

import grit.core.visibility.Integrity

/** A value checked to `integrity`, free to use anywhere. Made only by [[Trusted.check]],
  * [[Trusted.endorse]], [[Trusted.literal]], and [[Mint]].
  */
final class Trusted[+T] private[flow] (value: T, val integrity: Integrity) {

  def extract: T = value

  def map[U](f: T -> U): Trusted[U] = new Trusted(f(value), integrity)

  def coflatMap[U](f: Trusted[T] -> U): Trusted[U] = new Trusted(f(this), integrity)

  def zip[U](other: Trusted[U]): Trusted[(T, U)] =
    new Trusted((value, other.extract), integrity.meet(other.integrity))

  /** This value, unchecked again: to combine with a [[Tainted]] one. */
  def tainted: Tainted[T] = new Tainted(value, integrity)
}

object Trusted {

  /** `proposed`, trusted when its integrity dominates `required`; else `Left` of the same
    * value, unchanged.
    */
  def check[A](proposed: Tainted[A], required: Integrity): Either[Tainted[A], Trusted[A]] =
    if (proposed.integrity.dominates(required))
      Right(new Trusted(proposed.open, proposed.integrity))
    else Left(proposed)

  /** `proposed`, trusted at the integrity it is believed at: the check against
    * [[Integrity.Untrusted]], the least, which every value passes. For reading what picks the
    * requirement a value is then checked against (the tool a call names), never for acting on.
    */
  def asIs[A](proposed: Tainted[A]): Trusted[A] =
    new Trusted(proposed.open, proposed.integrity)

  /** A string constant in grit's source, at `Authority` with no compartment. A compile error
    * unless `text` is a constant.
    */
  inline def literal(inline text: String): Trusted[String] = {
    compiletime.requireConst(text)
    at(text, Integrity.at(Integrity.Level.Authority))
  }

  /** `t`'s value, trusted at the endorser's level ([[Endorse.at]]). `T` must be [[Bounded]]
    * to at most [[Bounded.K]] bits: free text is never endorsed.
    */
  def endorse[T, B <: Int](t: Tainted[T])(using
      Bounded[T, B],
      (B <= Bounded.K) =:= true
  )(using by: Endorse^): Trusted[T] =
    new Trusted(t.open, by.at)

  private[flow] def at[T](value: T, integrity: Integrity): Trusted[T] =
    new Trusted(value, integrity)
}
