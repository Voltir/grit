package grit.core.admin

import grit.core.identity.Principal
import grit.core.visibility.{Compartment, Compartments, Label, Level, RoomAccess}

/** Who may make which [[Change]]: one rule, deciding on what a change lowers and removes, never
  * on which command asked, so a later sort of change is decided by the same rule.
  */
object Authority {

  /** A change [[decide]] allowed: what applying one takes. */
  final class Allowed private[Authority] (val change: Change)

  /** Whether `by` may make `change`. `by` must be a person a trusted realm vouches a full
    * member; grit and anyone else are refused ([[Refusal.NotVouched]]). A label or compartment
    * the deployment's `compartments` do not declare is refused ([[Refusal.Undeclared]]), and a
    * direct message's room is never relabelled or quieted ([[Refusal.InDirectMessage]]). Then:
    *  - a room's quiet flag, either way: allowed;
    *  - a room's label, where its `access` is not [[RoomAccess.Invited]] (a public room, or one
    *    not reported): only when `administers` ([[Refusal.PublicRoom]]), since raising a public
    *    room would make it a destination higher rooms write down to (ADR 0031);
    *  - in an invited room: allowed when the new label dominates the one in force; otherwise a
    *    lower level needs `administers`, each compartment removed needs `administers` or to be
    *    in `stewards`, and replacing [[Compartment.Unmapped]] (a room's first label) needs
    *    `administers`, or a new label holding a compartment, each in `stewards`;
    *  - clearing or removing a person for `c`: `administers`, or `c` in `stewards`; and
    *    clearing is refused for a `c` not in `ownGroups` ([[Refusal.NoOwnGroup]]): the
    *    compartments whose own group the deployment declares, granted it.
    * `administers` and `stewards` must count only the deployment's declared members of its
    * administrators' and stewards' groups, never people added through grit
    * ([[grit.core.visibility.Visibility]]).
    */
  def decide(
      change: Change,
      by: Principal,
      access: Option[RoomAccess],
      administers: Boolean,
      stewards: Set[Compartment],
      compartments: Compartments,
      ownGroups: Set[Compartment]
  ): Either[Refusal, Allowed] = {
    def steward(c: Compartment): Option[Refusal] =
      Option.unless(administers || stewards.contains(c))(Refusal.NotSteward(c))
    def declared(c: Compartment): Option[Refusal] =
      Option.unless(compartments.declared.contains(c))(Refusal.Undeclared(c))
    val refusal: Option[Refusal] =
      if (!vouched(by)) Some(Refusal.NotVouched)
      else
        change match {
          case Change.Quiet(room, _) => Option.when(room.direct)(Refusal.InDirectMessage)
          case Change.Relabel(room, from, to) =>
            if (room.direct) Some(Refusal.InDirectMessage)
            else
              compartments
                .undeclared(to.label)
                .map(Refusal.Undeclared(_))
                .orElse {
                  if (!access.contains(RoomAccess.Invited))
                    Option.unless(administers)(Refusal.PublicRoom)
                  else if (administers || to.label.dominates(from)) None
                  else
                    Option
                      .when(lowered(from, to.label))(Refusal.NotAdministrator)
                      .orElse(removed(from, to.label, compartments).flatMap { c =>
                        if (c != Compartment.Unmapped) steward(c)
                        else
                          held(to.label, compartments) match {
                            case Vector() => Some(Refusal.NotAdministrator)
                            case cs => cs.flatMap(steward).headOption
                          }
                      }.headOption)
                }
          case Change.Clear(_, c) =>
            declared(c)
              .orElse(Option.unless(ownGroups.contains(c))(Refusal.NoOwnGroup(c)))
              .orElse(steward(c))
          case Change.Remove(_, c) => declared(c).orElse(steward(c))
        }
    refusal.toLeft(new Allowed(change))
  }

  /** Whether `by` is a person a trusted realm vouches a full member through any account. */
  private def vouched(by: Principal): Boolean = by match {
    case Principal.Grit => false
    case Principal.Person(_, accounts) => accounts.exists(_.member)
  }

  /* A label is opaque outside its package: what a change lowers and removes is read by
   * dominance alone, against each level and each declared compartment. */

  /** Whether some level `from` holds, `to` does not. */
  private def lowered(from: Label, to: Label): Boolean =
    Level.values.exists(l => from.dominates(Label.at(l)) && !to.dominates(Label.at(l)))

  /** The declared compartments `from` holds and `to` does not, by name. */
  private def removed(from: Label, to: Label, compartments: Compartments): Vector[Compartment] =
    held(from, compartments).filterNot(c => to.dominates(Label.at(Level.Public, c)))

  /** The declared compartments `label` holds, by name. */
  private def held(label: Label, compartments: Compartments): Vector[Compartment] =
    compartments.declared.toVector
      .filter(c => label.dominates(Label.at(Level.Public, c)))
      .sortBy(Compartment.name)
}
