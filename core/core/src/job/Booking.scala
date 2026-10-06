package grit.core.job

import grit.core.id.{JobName, PluginName}

/** A plugin's own jobs, as its tools book them when bound (ADRs 0027, 0029). */
trait OwnJobs extends caps.Pure {

  /** `job`'s booking; [[NotOwn]] when `job` is not among the plugin's jobs, which a deployment
    * refuses. Reached only in `grit.core.plugin.PluginTool.bind`.
    */
  def of[P <: caps.Pure](job: Job[P]): Either[NotOwn, Booking[P]]
}

object OwnJobs {

  /** `plugin`'s, its jobs `jobs`, each known by its name. */
  def over(plugin: PluginName, jobs: Vector[Job[?]]): OwnJobs = {
    val names = jobs.map(_.name).toSet
    new OwnJobs {
      def of[P <: caps.Pure](job: Job[P]): Either[NotOwn, Booking[P]] =
        Either.cond(names.contains(job.name), new Booking(job), NotOwn(plugin, job.name))
    }
  }
}

/** A job a plugin's tools may write schedules of, through their own plugin's desk
  * ([[ScheduleDesk]]), which refuses a booking of any other plugin's job.
  */
final class Booking[P <: caps.Pure] private[job] (val job: Job[P])

/** `plugin`'s tool asked to book `job`, which is not among `plugin`'s jobs. */
final case class NotOwn(plugin: PluginName, job: JobName)
