package grit.core.job

import java.time.Instant

import grit.core.id.JobName

/** A job (ADRs 0021, 0029): versioned code each slot of its schedules runs, as a turn of the
  * slot's own conversation at `task:{name}`, closed like any other. A plugin contributes it, or
  * a deployment declares it.
  */
trait Job[P <: caps.Pure] extends caps.Pure {
  def name: JobName

  /** The code its runs are made by. A run started at another version and not yet replied is
    * superseded: it ends with no reply, and its slot runs again at this one.
    */
  def version: Int

  /** `params` as a schedule keeps them. */
  def write(params: P): ujson.Value

  /** Parameters a schedule kept, or why they are not this job's; a run that cannot read its
    * parameters replies saying so.
    */
  def read(params: ujson.Value): Either[String, P]

  /** What a run replies: kept as its turn's reply, and posted where its schedule reports. */
  def reply(run: JobRun[P]): String
}

/** One run as its job sees it: its parameters, its slot's instant, and when it started (after
  * `nominal`, late, when grit was down).
  */
final case class JobRun[P <: caps.Pure](params: P, nominal: Instant, started: Instant)

/** A deployment's jobs, by name. */
final class Jobs private (byName: Map[JobName, Job[?]]) {
  def named(name: JobName): Option[Job[?]] = byName.get(name)
}

object Jobs {

  /** `jobs`, or the first name two share. */
  def of(jobs: Vector[Job[?]]): Either[JobName, Jobs] =
    jobs
      .foldLeft[Either[JobName, Map[JobName, Job[?]]]](Right(Map.empty)) { (acc, j) =>
        acc.flatMap(seen =>
          if (seen.contains(j.name)) Left(j.name) else Right(seen.updated(j.name, j))
        )
      }
      .map(new Jobs(_))
}
