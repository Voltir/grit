package grit.commands.worker

import grit.core.place.{Directory, Namespace, Place}

/** Where a worker's work is done, as grit places it. */
object Worktrees {

  /** `dir` on `worker`, as a place: `worker:<name>/<dir>` ([[Namespace.Worker]]); one path on
    * two workers is two places.
    */
  def at(worker: WorkerName, dir: Directory): Place =
    Place.under(Namespace.Worker, Vector(WorkerName.value(worker), Directory.value(dir)))
}
