package grit.core.visibility

/** The deployment clears `party` for `clearance`: what grit sends it is classified at most this. */
final case class Cleared(party: Party, clearance: Label) derives CanEqual

/** What each party is cleared for, by the deployment's declaration. */
final case class Clearances private (declared: Vector[Cleared]) {

  /** `party`'s declared clearance; [[Label.Public]] when none is declared. */
  def of(party: Party): Label = declared.find(_.party == party).fold(Label.Public)(_.clearance)

  /** Whether content at `classification` may be sent to `party`: its clearance dominates it
    * (the simple security property, the party reading).
    */
  def clears(party: Party, classification: Classification): Boolean =
    of(party).dominates(classification.label)
}

object Clearances {

  val Empty: Clearances = new Clearances(Vector.empty)

  /** `declared`, or the first party declared twice. */
  def of(declared: Vector[Cleared]): Either[Party, Clearances] = {
    val parties = declared.map(_.party)
    parties.diff(parties.distinct).headOption.toLeft(new Clearances(declared))
  }
}
