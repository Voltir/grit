package grit.act.moves

import grit.core.act.MoveName

/** The names of a planner's steps, as DBOS records them: `move:{name}` for the move itself, then
  * `move:{name}:{phase}` for the phases that follow it. A stored form: a run in flight replays
  * by them.
  */
object MoveSteps {

  /** An ask's model call, admitted first. */
  def ask(n: MoveName): String = s"move:${MoveName.value(n)}"

  /** An ask's cost recorded, after [[ask]]. */
  def record(n: MoveName): String = s"move:${MoveName.value(n)}:record"

  /** A call's request, written for its edge. */
  def call(n: MoveName): String = s"move:${MoveName.value(n)}"

  /** A sent call settled when no edge claimed it within `grit.act.phase.Calling.ServeWithin`. */
  def expire(n: MoveName): String = s"move:${MoveName.value(n)}:expire"

  /** A claimed call abandoned when its edge did not answer within
    * `grit.act.phase.Calling.RunWithin` more.
    */
  def abandon(n: MoveName): String = s"move:${MoveName.value(n)}:abandon"

  /** A call's answer read, once its edge rang. */
  def answer(n: MoveName): String = s"move:${MoveName.value(n)}:answer"
}
