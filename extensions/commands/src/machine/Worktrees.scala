package grit.commands.machine

import grit.core.place.{Directory, Namespace, Place}

/** Where a machine's work is done, as grit places it. */
object Worktrees {

  /** `dir` on `machine`, as a place: `machine:<name>/<dir>` ([[Namespace.Machine]]); one path on
    * two machines is two places.
    */
  def at(machine: MachineName, dir: Directory): Place =
    Place.under(Namespace.Machine, Vector(MachineName.value(machine), Directory.value(dir)))
}
