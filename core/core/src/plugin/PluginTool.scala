package grit.core.plugin

import grit.core.id.CallSlot
import grit.core.job.{NotOwn, OwnJobs, ScheduleDesk}
import grit.core.store.Db
import grit.core.tool.{Hosted, Outcome}

/** A tool a plugin runs over grit's store: `described`, what it is without a capability (so a
  * deployment can check its name), bound to its run when the engine starts.
  */
trait PluginTool[A] extends caps.Pure {
  def described: Hosted[A]

  /** Its run over `own`, the services it takes from `needs` and the jobs it books from `jobs`;
    * `Left` when it asks `needs` for a plugin its own does not list, or `jobs` for a job not its
    * plugin's, either of which a deployment refuses.
    */
  def bind(own: PluginReads, needs: Needs, jobs: OwnJobs): Either[Unneeded | NotOwn, PluginRun[A]]
}

/** A plugin tool's run, bound at start. */
trait PluginRun[A] extends caps.Pure {

  /** What the call at `call` with `args` comes to, read through `db`, writing its plugin's
    * bookings' schedules through `desk`. Never throws: every failure is an
    * [[grit.core.tool.Outcome]].
    */
  def run(args: A, call: CallSlot, db: Db^, desk: ScheduleDesk^): Outcome
}
