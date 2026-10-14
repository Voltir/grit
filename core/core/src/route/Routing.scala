package grit.core.route

import grit.core.model.{Grade, Pinned, Pins}
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

  /** The pin `wanted` routes to among `cleared`: the first, in their order, of the least grade
    * at or above `wanted`; when none is, the first of the greatest grade below.
    */
  def pick(cleared: Pins, wanted: Grade): Pinned = {
    val (above, below) = cleared.all.partition(_.grade.ordinal >= wanted.ordinal)
    def first(at: Vector[Pinned], grade: Option[Grade]) = grade.flatMap(g => at.find(_.grade == g))
    first(above, above.map(_.grade).minByOption(_.ordinal))
      .orElse(first(below, below.map(_.grade).maxByOption(_.ordinal)))
      .getOrElse(cleared.first)
  }

  /** For `picked` = `pick(pins, g)`, `pick(pins, g max floor)`: `picked` when its grade is at or
    * above `floor`, otherwise the pin [[pick]] gives `floor` among `pins`. Never a pin outside
    * `pins` but `picked`.
    */
  def floored(pins: Pins, picked: Pinned, floor: Grade): Pinned =
    if (picked.grade.ordinal >= floor.ordinal) picked else pick(pins, floor)

  /** `pins` at or below `ceiling`, in order; all of `pins` when none is. */
  def atMost(pins: Pins, ceiling: Grade): Pins =
    Pins.of(pins.all.filter(_.grade.ordinal <= ceiling.ordinal)).getOrElse(pins)
}
