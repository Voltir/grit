package grit.dbos.engine

import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.id.{JobName, PluginName, TurnRef, WorkflowId}
import grit.core.inbox.InboxError
import grit.core.job.ScheduleDesk
import grit.core.plugin.Plugin
import grit.core.stitch.Placements
import grit.core.store.{Db, Jot}
import grit.core.triage.Shadowing

/** What a launch runs on: the stores, openers and clock of a database's [[Engine]], or of a
  * [[Rehearsing]] session over it, and the workflows' bodies, handed over by [[launch]]. Every
  * workflow runs with the body it was given; what runs one, and when, is the implementation's:
  * an engine runs each as a durable workflow, a session's rehearsal runs each once, inside the
  * session.
  */
trait Runtime extends Stores, caps.SharedCapability {

  /** Short read transactions. */
  val db: Db

  /** Short write transactions, each one's own. */
  val jot: Jot

  /** The time, by which every body and desk built over this dates. */
  val clock: Clock^

  /** Where each opening is placed, by the `stitch` body [[launch]] is given. */
  val placements: Placements

  /** Starts `turn`, a turn already recorded, with the `turn` body [[launch]] is given, if it has
    * not been started; [[grit.core.inbox.InboxError.Unavailable]] when that cannot be recorded,
    * and [[grit.core.inbox.InboxError.Invalid]] when a session's runner refuses it
    * ([[Rehearsing.startTurn]]).
    */
  def startTurn(turn: TurnRef): Either[InboxError, Unit]

  /** `plugin`'s desk (ADR 0029): the schedules its tools write, of the jobs named `jobs`, each
    * in a transaction of [[jot]]'s, dated by [[clock]].
    */
  def desk(plugin: PluginName, jobs: Vector[JobName]): ScheduleDesk^ =
    sqlSchedules.desk(plugin, jobs, jot, clock)

  /** Hands over `turn` as the body of every turn, `close` of every attempt to close a period,
    * `settle` of every question whether anyone is waiting on a quiet period, `post` of every
    * posting run, `triage` of every heard message's triage, `stitch` of every opening's
    * placement, `shadow` of every shadow and `run` of every job's run; `plugins` are the ones
    * enabled and `shadowing` the shadow variants declared. Once: no workflow runs before it,
    * and how many run before it returns is the implementation's (as above). Throws when they
    * cannot be taken ([[Engine.launch]] and [[Rehearsing.launch]] say when).
    */
  def launch(
      turn: WorkflowId => Durable^ ?=> String,
      close: WorkflowId => Durable^ ?=> String,
      settle: WorkflowId => Durable^ ?=> String,
      post: WorkflowId => Durable^ ?=> String,
      triage: WorkflowId => Durable^ ?=> String,
      stitch: WorkflowId => Durable^ ?=> String,
      plugins: Vector[Plugin],
      shadow: WorkflowId => Durable^ ?=> String = Runtime.Unshadowed,
      shadowing: Vector[Shadowing] = Vector.empty,
      run: WorkflowId => Durable^ ?=> String = Runtime.Unrun
  ): Unit
}

object Runtime {

  /** The shadow body of a launch that declares no shadows: it asks nothing and keeps nothing. */
  val Unshadowed: WorkflowId -> Durable^ ?-> String =
    id => (_: Durable^) ?=> s"no shadows are declared: ${WorkflowId.value(id)}"

  /** The run body of a launch given none: it runs no job and writes nothing, so its run ends
    * without a reply ([[grit.core.job.InFlight.Ended]]).
    */
  val Unrun: WorkflowId -> Durable^ ?-> String =
    id => (_: Durable^) ?=> s"no job runs here: ${WorkflowId.value(id)}"
}
