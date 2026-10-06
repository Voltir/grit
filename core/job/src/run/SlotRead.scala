package grit.job.run

import java.time.Instant

import grit.core.id.JobName
import grit.core.job.{Report, Slot}

/** What a run's `read-slot` step recorded: the slot it runs, or why it runs none. */
enum SlotRead {

  /** A run of `job` for `slot`, started at its job's `version` at `started` (when its opening
    * was written), its schedule's parameters as kept `params`, reporting as `report` says.
    */
  case Read(
      slot: Slot,
      job: JobName,
      version: Int,
      started: Instant,
      params: ujson.Value,
      report: Report
  )

  /** `why` it runs none: its conversation is no slot's, its opening names no version, its
    * schedule is gone, or the store could not be read.
    */
  case Unreadable(why: String)
}

/** What a run's `reply` step recorded. */
enum RunEnd {

  /** Its turn's reply, `text`: written by this step, or kept already by an earlier run of it. */
  case Replied(text: String)

  /** Its job is at `current` now, not at the version the run started at: nothing written. */
  case Superseded(current: Int)

  /** The deployment lacks its job: nothing written. */
  case Jobless

  /** The store failed, `why`: nothing written. */
  case Failed(why: String)
}
