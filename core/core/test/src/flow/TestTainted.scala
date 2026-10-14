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
}
