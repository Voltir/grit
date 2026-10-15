package grit.core.flow

import grit.core.visibility.Integrity

/** The two capabilities that raise a [[Tainted]] value, one per role, each minted only by
  * [[Mint]].
  */
object Endorse {

  /** An extraction's: raises a bounded value to [[Trusted]] at `at`, by [[Trusted.endorse]]. */
  final class Extraction private[flow] (val at: Integrity) extends caps.SharedCapability

  /** The integrity tree's: raises a text believed `Untrusted` to `Outside`, by [[sanitised]]. */
  final class Sanitiser private[flow] () extends caps.SharedCapability {

    /** `t` at `Outside`, in the same compartments, when it is `Untrusted`; else `t`. Named only
      * by [[grit.core.sanitise.IntegrityTree.vet]] (the law), which calls it on a `Raised`
      * verdict alone.
      */
    def sanitised[T](t: Tainted[T]): Tainted[T] =
      if (!t.integrity.belowOutside) t
      else new Tainted(t.open, t.integrity.join(Integrity.Outside))
  }
}
