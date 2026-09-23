package grit.core.interop

import scala.annotation.implicitNotFound
import scala.util.NotGiven

/** Evidence that `A` can be the output of a DBOS step.
  *
  * DBOS persists every step's return value through Jackson and replays it from
  * the row on retry, so `Unit` — which Jackson has no representation for — is
  * not a legal step output.
  */
@implicitNotFound(
  "A step body must return a value DBOS can persist and replay. ${A} cannot be a step output: return a value describing what the step did."
)
sealed trait StepResult[A]

object StepResult {
  given [A](using NotGiven[A =:= Unit]): StepResult[A] = new StepResult[A] {}
}
