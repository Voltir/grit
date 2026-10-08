package grit.core.admin

import grit.core.visibility.Compartment

/** Why a command was not run; [[text]] is the line its asker reads. */
enum Refusal {

  /** The asker is grit, or a person no trusted realm vouches a full member. */
  case NotVouched

  /** The change lowers a level, or labels a room's first label with no compartment: an
    * administrator's alone.
    */
  case NotAdministrator

  /** The room is public, or its access is not reported: only an administrator relabels it. */
  case PublicRoom

  /** The change removes `compartment` from a room, replaces a first label holding it, or
    * clears or removes a person for it: its steward's or an administrator's alone.
    */
  case NotSteward(compartment: Compartment)

  /** The deployment declares no `compartment`. */
  case Undeclared(compartment: Compartment)

  /** No one is cleared for `compartment` through grit: the deployment declares no group named
    * as it and granted it.
    */
  case NoOwnGroup(compartment: Compartment)

  /** A direct message's room is neither relabelled nor quieted. */
  case InDirectMessage

  /** Another person's clearance, asked by someone not an administrator. */
  case NotForYou

  def text: String = this match {
    case NotVouched =>
      "Only a person grit knows as a full member of a source it trusts can change this."
    case NotAdministrator => "Only an administrator can make this change."
    case PublicRoom =>
      "This channel is public: make it private to label it, or ask an administrator."
    case NotSteward(c) =>
      s"Only a steward of ${Compartment.name(c)}, or an administrator, can make this change."
    case Undeclared(c) =>
      s"${Compartment.name(c)} is not a compartment here: compartments lists the ones you can name."
    case NoOwnGroup(c) =>
      s"No one is cleared for ${Compartment.name(c)} through grit: no group of its own is " +
        "declared, granted it."
    case InDirectMessage =>
      "A direct message's label is its person's clearance, and it is never quiet: neither can " +
        "be changed."
    case NotForYou => "Only an administrator can see another person's clearance."
  }
}
