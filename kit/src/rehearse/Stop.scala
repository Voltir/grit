package grit.kit.rehearse

import scala.util.control.ControlThrowable

/** A rehearsal's end at the turn's own model call, made in the step `step`
  * ([[grit.turn.Turn.Step.answering]]): the call is not answered, and nothing after it runs.
  * [[RehearsalRunner]] catches it; nothing else does.
  */
final case class Stop(step: String) extends ControlThrowable
