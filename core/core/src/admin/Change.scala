package grit.core.admin

import grit.core.identity.Account
import grit.core.place.Place
import grit.core.visibility.{Compartment, Label}

/** What a command changes, with what it replaces: the unit of authority ([[Authority.decide]])
  * and of the audit row ([[ChangeJson]]). [[kind]] names its sort as stored; a new sort is a new case and a new
  * kind, never a reuse.
  */
enum Change {

  /** `room`'s label from `from`, the label in force there, to `to`. */
  case Relabel(room: Place, from: Label, to: Change.To)

  /** `room` made quiet (`on`): nothing posted there unasked; or no longer. */
  case Quiet(room: Place, on: Boolean)

  /** `person` cleared for `compartment`: added, through grit, to its own group. */
  case Clear(person: Account, compartment: Compartment)

  /** `person` taken out of `compartment`'s own group, where grit added them. */
  case Remove(person: Account, compartment: Compartment)

  /** `relabel`, `quiet`, `clear` or `remove`. */
  def kind: String = this match {
    case Relabel(_, _, _) => "relabel"
    case Quiet(_, _) => "quiet"
    case Clear(_, _) => "clear"
    case Remove(_, _) => "remove"
  }
}

object Change {

  /** What a room's label becomes: [[label]] in either case, so `unlabel` is decided as any
    * other relabel, to its default's label.
    */
  enum To {

    /** `label`, set through grit. */
    case Set(label: Label)

    /** The room's default, the label it takes with none set: `label` now. */
    case Default(label: Label)

    def label: Label
  }
}
