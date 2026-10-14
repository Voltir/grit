package grit.core.flow

import grit.core.visibility.Integrity

/** Lets its holder raise a [[Tainted]] value: to [[Trusted]] by [[Trusted.endorse]], at its
  * own level, or, for a sanitiser's, an `Untrusted` one to `Outside` by [[sanitised]]. Minted
  * only by [[Mint]].
  */
sealed abstract class Endorse private[flow] (val at: Integrity) extends caps.SharedCapability {

  /** `t` at `Outside`, in the same compartments, when it is `Untrusted` and this is a
    * sanitiser's; else `t`.
    */
  def sanitised[T](t: Tainted[T]): Tainted[T]
}

private[flow] object Endorse {

  /** An extraction's: raises to [[Trusted]] at `at`, and sanitises nothing. */
  final class Extraction(at: Integrity) extends Endorse(at) {
    def sanitised[T](t: Tainted[T]): Tainted[T] = t
  }

  /** The integrity tree's: `Untrusted` to `Outside`, keeping its compartments. */
  final class Sanitiser extends Endorse(Integrity.Outside) {
    def sanitised[T](t: Tainted[T]): Tainted[T] =
      if (t.integrity.dominates(Integrity.Outside)) t
      else new Tainted(t.open, t.integrity.join(Integrity.Outside))
  }
}
