package grit.core.flow

import scala.annotation.unused

import grit.core.durable.Durable
import grit.core.visibility.Integrity

/** A turn's integrity: Biba's low-water mark (ADR 0036). Starts at its trigger's and only
  * falls.
  */
final class Taint private[flow] (start: Integrity)
    extends caps.ExclusiveCapability,
      caps.Stateful {

  private var current: Integrity = start

  def level: Integrity = current

  /** Falls to the meet of its level and `to`; never rises. Takes the [[Durable]] so that it is
    * called outside any step's body (a body cannot hold its `Durable`); which level it is given
    * is the caller's.
    */
  update def lower(to: Integrity)(using @unused d: Durable^): Unit =
    current = current.meet(to)

  /** `value`, the model's, at [[level]]. */
  def stamp[A](value: A): Tainted[A] = new Tainted(value, current)
}
