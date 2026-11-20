package grit.core.act

import java.time.Instant

import grit.core.id.JobName

/** What a booking names of a job: its name, and its parameters as a schedule keeps them. Every
  * [[grit.core.job.Job]] is one.
  */
trait Bookable[P <: caps.Pure] extends caps.Pure {
  def name: JobName
  def write(params: P): ujson.Value
}

/** A run of `job` at `at` with `params`, missed unless it starts by `until` (`at` when `until`
  * is before it).
  */
final case class Booked[P <: caps.Pure](job: Bookable[P], at: Instant, until: Instant, params: P)
    extends caps.Pure
