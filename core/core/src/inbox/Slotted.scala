package grit.core.inbox

import grit.core.id.TurnRef
import grit.core.job.Slot

/** What [[Inbox.startSlot]] did with a schedule. */
enum Slotted {

  /** Its due slot started as a run, the turn `turn`. */
  case Started(turn: TurnRef, slot: Slot)

  /** Its run's lost start enqueued again. */
  case Restarted(turn: TurnRef)

  /** A run at another version, going or ended without a reply, left to end superseded, and
    * `slot` started at the current one as the turn `turn`.
    */
  case Superseding(turn: TurnRef, slot: Slot)

  /** Its run of `slot` ended without a reply at its job's current version, or with its job
    * gone: a once schedule ended failed; a recurrence goes on to its next slot.
    */
  case Failed(slot: Slot)

  /** Its once slot, more than its grace past, recorded as missed and never run. */
  case Missed(slot: Slot)

  /** Its next slot was run already, by its last run, of `slot`: a once schedule declared again
    * after its run replied while it was undeclared. A once schedule ends `ran`, never started
    * again; a recurrence goes on to the slot after `slot`.
    */
  case Ran(slot: Slot)

  /** Nothing to do: the schedule is ended, gone, or not due yet; its run is going at the
    * current version; or its job is not the deployment's and its once slot is within its grace.
    */
  case Idle
}
