package grit.core.flow

import grit.core.visibility.Integrity

/** Where wrapped values and the cell are made from raw ones. Named only by `grit.turn`,
  * `grit.act` and `grit.kit` (the law).
  */
object Mint {

  def taint(start: Integrity): Taint^ = new Taint(start)

  /** `value`, believed at `integrity`, as a reader that knows its source says. */
  def tainted[A](value: A, integrity: Integrity): Tainted[A] = new Tainted(value, integrity)

  /** `value`, checked to `integrity`, as a reader that knows its source says. */
  def trusted[A](value: A, integrity: Integrity): Trusted[A] = Trusted.at(value, integrity)

  /** The endorsement `extract` makes: at `at` met with [[Integrity.Member]]. */
  def extraction(at: Integrity): Endorse.Extraction^ =
    new Endorse.Extraction(at.meet(Integrity.Member))

  /** The integrity tree's: `Untrusted` to `Outside`, never higher. */
  def sanitiser: Endorse.Sanitiser^ = new Endorse.Sanitiser
}
