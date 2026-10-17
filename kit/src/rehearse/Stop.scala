package grit.kit.rehearse

import scala.util.control.ControlThrowable

/** A rehearsal's end at the turn's own model call, made in the step `step`
  * ([[grit.turn.Turn.Step.answering]]): thrown by a tap's provider ([[Taps.models]]) in place
  * of an answer, once the call is on the tape, so the call is not answered and nothing after
  * it runs. [[RehearsalRunner]] catches it.
  *
  * A `ControlThrowable`, so no `NonFatal` handler on its way out (a rehearsal's `transact`, a
  * session's savepoint) takes it for a failure. The one handler in a shipped body that
  * swallows a throw, [[grit.lifecycle.triage.Mentions]]' bounded weighing, runs only a
  * classifier call, so it is off this path. Thrown from inside
  * [[grit.act.phase.Asking.reply]], it skips the hearing's `done()`: the pieces heard so far
  * are kept nowhere in a rehearsal.
  */
final case class Stop(step: String) extends ControlThrowable
