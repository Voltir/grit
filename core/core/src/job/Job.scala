package grit.core.job

import java.time.Instant

import grit.core.act.{MoveLimits, Moves}
import grit.core.id.{JobName, PluginName}

/** A job (ADRs 0021, 0029): versioned code each slot of its schedules runs, as a turn of the
  * slot's own conversation at `task:{name}`, closed like any other. A plugin contributes it, or
  * a deployment declares it. A [[PlainJob]].
  */
sealed trait Job[P <: caps.Pure] extends caps.Pure {
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

  /** How many asks and calls one run may make: [[grit.core.act.MoveLimits.Zero]] unless it says. */
  def limits: MoveLimits = MoveLimits.Zero
}

/** A job whose runs keep nothing of their own: the deployment's, or a plugin's. */
trait PlainJob[P <: caps.Pure] extends Job[P] {

  /** What the run `run` replies, after the moves it makes through `moves`: kept as its turn's
    * reply, and posted where its schedule reports.
    */
  def run(run: JobRun[P], moves: Moves^): String
}

/** One run as its job sees it: its parameters, its slot's instant, and when it started (after
  * `nominal`, late, when grit was down).
  */
final case class JobRun[P <: caps.Pure](params: P, nominal: Instant, started: Instant)

/** A job, and whose it is. */
enum Owned {
  case Deployments(plain: PlainJob[?])
  case Plugins(plugin: PluginName, owned: Job[?])

  def job: Job[?] = this match {
    case Deployments(j) => j
    case Plugins(_, j) => j
  }
}

/** A deployment's jobs, by name. */
final class Jobs private (byName: Map[JobName, Owned]) {

  /** The job named `name`, and whose it is; `None` when the deployment has none. */
  def named(name: JobName): Option[Owned] = byName.get(name)
}

object Jobs {

  /** `owned`, or the first name two share. */
  def of(owned: Vector[Owned]): Either[JobName, Jobs] =
    owned
      .foldLeft[Either[JobName, Map[JobName, Owned]]](Right(Map.empty)) { (acc, o) =>
        val name = o.job.name
        acc.flatMap(seen => if (seen.contains(name)) Left(name) else Right(seen.updated(name, o)))
      }
      .map(new Jobs(_))
}
