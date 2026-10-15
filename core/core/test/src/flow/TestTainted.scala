package grit.core.flow

import grit.core.visibility.Integrity

/** Stamped values for tests outside this package, which may not name [[Mint]]. */
object TestTainted {

  /** `value`, stamped at `integrity`. */
  def at[A](value: A, integrity: Integrity): Tainted[A] = Mint.tainted(value, integrity)

  /** `value`, stamped at [[Integrity.Authority]]: above every requirement naming no
    * compartment, as a turn stamps its calls before its level is recorded.
    */
  def authority[A](value: A): Tainted[A] = Mint.tainted(value, Integrity.Authority)

  /** `value`, checked to `integrity`: [[Integrity.Authority]] unless given, as a declared
    * schedule's run starts.
    */
  def trusted[A](value: A, integrity: Integrity = Integrity.Authority): Trusted[A] =
    Mint.trusted(value, integrity)

  /** The endorsement an extraction makes in a run at `at` ([[Mint.extraction]]). */
  def extraction(at: Integrity): Endorse^ = Mint.extraction(at)

  /** The integrity tree's endorsement ([[Mint.sanitiser]]). */
  def sanitiser: Endorse^ = Mint.sanitiser
}
