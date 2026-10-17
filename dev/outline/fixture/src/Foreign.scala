package grit.outline.fixture

/** A value whose readers live in another object. */
final case class Spot(name: String)

/** Reads a Spot from outside it. */
object Spots {

  /** Whether `spot` has a name. */
  def isNamed(spot: Spot): Boolean = spot.name.nonEmpty
}
