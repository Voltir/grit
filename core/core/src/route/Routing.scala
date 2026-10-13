package grit.core.route

import grit.core.model.Pins
import grit.core.visibility.{Classification, Clearances, Label, Party}

/** No pair is cleared for `classification`, so the call is not made. */
final case class Uncleared(classification: Label)

object Uncleared {

  /** What a turn said to grit replies when no turn pair is cleared: "No model this deployment
    * uses is cleared for this conversation's classification, <shown>. An administrator can
    * clear one; until then I can't answer here."
    */
  def reply(u: Uncleared): String =
    "No model this deployment uses is cleared for this conversation's classification, " +
      s"${Label.shown(u.classification)}. An administrator can clear one; until then I can't " +
      "answer here."
}

/** Which of a role's pins a call may be made with (ADR 0037). */
object Routing {

  /** `pins` whose pair is cleared for `classification`, in order; `None` when none is. A pair
    * whose upstream OpenRouter picks ([[Party.of]] is `None`) is cleared for public alone.
    */
  def cleared(pins: Pins, clearances: Clearances, classification: Classification): Option[Pins] =
    Pins
      .of(pins.all.filter { p =>
        Party.of(p.assignment.ref).fold(Label.Public)(clearances.of).dominates(classification.label)
      })
      .toOption
}
