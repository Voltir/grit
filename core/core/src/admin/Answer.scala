package grit.core.admin

import grit.core.model.Grade
import grit.core.visibility.{
  Compartment,
  Compartments,
  Explanation,
  GroupName,
  Label,
  Level,
  RoomAccess
}

/** What a command did; [[text]] is what its asker is shown. The words for each are its
  * companion's: a label shown says where it comes from, a label changed that conversations
  * begun before keep theirs, a `clear` or `remove` the person's clearance after it, a
  * clearance asked the asker's own in full or another's label alone.
  */
enum Answer {

  /** What a command that only reads shows. */
  case Shown(shown: String)

  /** `change`, made. */
  case Changed(change: Change, shown: String)

  /** Not run, for `refusal`. */
  case Refused(refusal: Refusal)

  def text: String = this match {
    case Shown(shown) => shown
    case Changed(_, shown) => shown
    case Refused(refusal) => refusal.text
  }
}

object Answer {

  /** Where a room's label in force comes from. */
  enum Source {

    /** Set through grit. */
    case Set

    /** Declared by the deployment, for the room or a place it is within. */
    case Declared

    /** Its access's default ([[grit.core.visibility.RoomLabels]]): none is set or declared. */
    case Default
  }

  /** A room's label in force, `label`, from `source`, given its `access` as reported: a default
    * says for which kind of room, and an unreported access is said to be not yet known.
    */
  def label(label: Label, source: Source, access: Option[RoomAccess]): Answer = {
    val from = (source, access) match {
      case (Source.Set, _) => "set through grit"
      case (Source.Declared, _) => "as the deployment declares it"
      case (Source.Default, Some(RoomAccess.Open)) => "the default for a public room"
      case (Source.Default, Some(RoomAccess.Invited)) => "the default for a private room"
      case (Source.Default, None) => "the deployment's default"
    }
    val unknown = if (access.isEmpty) " Its access is not yet known." else ""
    Shown(s"This room is labelled ${Label.shown(label)}, $from.$unknown")
  }

  /** `change` made: the room's label now and before. */
  def relabelled(change: Change.Relabel): Answer = {
    val now = change.to match {
      case Change.To.Set(to) => s"now labelled ${Label.shown(to)}"
      case Change.To.Default(to) => s"back to its default label, ${Label.shown(to)}"
    }
    Changed(
      change,
      s"This room is $now; it was ${Label.shown(change.from)}. $KeepTheirs"
    )
  }

  /** A room's baseline in force (`None`: none), as `baseline` shows it to any member: the least
    * grade its turns are routed to, never above what a turn is cleared for or what the day's
    * spend allows.
    */
  def baseline(inForce: Option[Grade]): Answer =
    Shown(
      inForce.fold(s"This room has no baseline: $Unfloored")(g =>
        s"This room's baseline is ${g.word}: ${floored(g)}"
      )
    )

  /** `change` made: the baseline now, and what it was. */
  def rebased(change: Change.Baseline): Answer = {
    val was = change.from.fold("it had none")(g => s"it was ${g.word}")
    Changed(
      change,
      change.to.fold(s"This room has no baseline now; $was. ${Unfloored.capitalize}")(g =>
        s"This room's baseline is now ${g.word}; $was. ${floored(g).capitalize}"
      )
    )
  }

  /** What a room with no baseline does. */
  private val Unfloored = "each turn is answered by the model its task calls for."

  /** What a baseline of `g` does. */
  private def floored(g: Grade): String =
    s"its turns are answered by a ${g.word} model or stronger, as far as what they read and " +
      "the day's spend allow."

  /** `change` made: what a quiet room stops, and what it keeps. */
  def quieted(change: Change.Quiet): Answer =
    Changed(
      change,
      if (change.on)
        "This room is quiet: I post nothing here unasked, and still answer when mentioned."
      else "This room is no longer quiet: I may post here unasked."
    )

  /** `change` made, the person's clearance `after` it. */
  def cleared(change: Change.Clear, after: Label): Answer =
    Changed(
      change,
      s"Cleared for ${Compartment.name(change.compartment)}. ${clearance(after)}"
    )

  /** `change` made, the person's clearance `after` it, and `still`: the groups the deployment
    * declares them in, by account or as a realm's full member, whose grant still holds the
    * compartment, in its order.
    */
  def removed(change: Change.Remove, after: Label, still: Vector[GroupName]): Answer = {
    val c = Compartment.name(change.compartment)
    val kept =
      if (still.isEmpty) ""
      else
        s" They are still cleared for $c through ${Command.and(still.map(GroupName.value))}, as the " +
          "deployment declares: only a change to the deployment takes that away."
    Changed(change, s"Removed from $c. ${clearance(after)}$kept")
  }

  /** The compartments the deployment's `compartments` declare, by name, never
    * [[Compartment.Unmapped]]: every one when the asker `administers`; otherwise those their
    * clearance, `cleared`, holds, and those they steward, `stewards`.
    */
  def compartments(
      compartments: Compartments,
      administers: Boolean,
      stewards: Set[Compartment],
      cleared: Label
  ): Answer = {
    val named =
      compartments.declared.toVector.filter(_ != Compartment.Unmapped).sortBy(Compartment.name)
    def names(cs: Vector[Compartment]): String = Command.and(cs.map(Compartment.name))
    if (administers)
      Shown(
        if (named.isEmpty) "No compartment is declared."
        else s"Every compartment: ${names(named)}."
      )
    else {
      val held = named.filter(c => cleared.dominates(Label.at(Level.Public, c)))
      val stewarded = named.filter(stewards.contains)
      val clearedFor =
        if (held.isEmpty) "You are cleared for no compartment."
        else s"You are cleared for ${names(held)}."
      val steward = if (stewarded.isEmpty) "" else s" You steward ${names(stewarded)}."
      Shown(clearedFor + steward)
    }
  }

  /** A direct message's label, shown to its person: their `clearance`, which no command
    * changes.
    */
  def direct(clearance: Label): Answer =
    Shown(
      s"This is a direct message: its label is your clearance, ${Label.shown(clearance)}, and " +
        "it cannot be changed."
    )

  /** Another person's clearance, `clearance`, shown to an administrator. */
  def theirs(clearance: Label): Answer =
    Shown(s"Their clearance is ${Label.shown(clearance)}.")

  /** The asker's own clearance, as `e` explains it, in [[Explanation.cleared]]'s words. */
  def clearance(e: Explanation): Answer = Shown(Explanation.cleared(e))

  /** [[Command.Usage]]. */
  val Help: Answer = Shown(Command.Usage)

  private val KeepTheirs = "Conversations begun before keep theirs."

  private def clearance(after: Label): String =
    s"Their clearance is now ${Label.shown(after)}."
}
