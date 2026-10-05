package grit.core.plugin

import grit.core.store.Db
import grit.core.tool.{Hosted, Outcome}

/** A tool a plugin runs over grit's store: `described`, what it is without a capability (so a
  * deployment can check its name), bound to its run when the engine starts.
  */
trait PluginTool[A] extends caps.Pure {
  def described: Hosted[A]

  /** Its run over `own` and the services it takes from `needs`; `Left` when it asks `needs`
    * for a plugin its own does not list, which a deployment refuses.
    */
  def bind(own: PluginReads, needs: Needs): Either[Unneeded, PluginRun[A]]
}

/** A plugin tool's run, bound at start. */
trait PluginRun[A] extends caps.Pure {

  /** What a call with `args` comes to, read through `db`. Never throws: every failure is an
    * [[grit.core.tool.Outcome]].
    */
  def run(args: A, db: Db^): Outcome
}
