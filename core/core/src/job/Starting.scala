package grit.core.job

import java.time.Instant

/** The run a schedule last started, for its slot at `slot`: `flight`, that run's state while it
  * is in flight; `None` once it replied, or failed and the schedule went on.
  */
final case class LastRun(slot: Instant, flight: Option[InFlight])

/** What a pending schedule gets when the inbox starts what it has waiting
  * (`grit.core.inbox.Inbox.startSlot`), decided by [[Due]] and [[Resume]].
  */
enum Starting {

  /** Nothing: no slot is due yet, or none is left; its run is going at the current version, or
    * replied; or the deployment lacks its job and its slot is not past its grace.
    */
  case Idle

  /** Start the slot at `slot` as a run at `version`, the schedule's next slot then `following`.
    * `superseding`: a run at another version is in flight, and left to end superseded.
    */
  case Start(slot: Instant, following: Option[Instant], version: Int, superseding: Boolean)

  /** Enqueue the run in flight again: its start was lost. */
  case Restart

  /** The run in flight ended without a reply: a once schedule ends `failed`; a recurrence lets it
    * go and keeps its next slot.
    */
  case Fail

  /** The once slot at `slot` is more than its grace past: the schedule ends `missed`. */
  case Miss(slot: Instant)

  /** Its next slot is not after the slot its last run started for, and that run is not in
    * flight: it was run already (a once schedule ended undeclared while its run went on, and
    * declared again). A once schedule ends `ran`; a recurrence's next slot becomes `following`.
    */
  case Passed(following: Option[Instant])
}

object Starting {

  /** What a pending schedule of rule `rule` gets at `now`, its next slot at `next` (`None`: it
    * has none left), its last run `last` (`None`: it never started one), its job's current
    * version `current` (`None`: the deployment lacks its job).
    */
  def of(
      rule: SlotRule,
      next: Option[Instant],
      last: Option[LastRun],
      current: Option[Int],
      now: Instant
  ): Starting =
    last.flatMap(l => l.flight.map(l.slot -> _)) match {
      case Some((started, flight)) =>
        Resume.of(flight, current, rule, started, next, now) match {
          case Resume.Leave => Idle
          case Resume.Enqueue => Restart
          case Resume.Fail => Fail
          case Resume.Supersede(slot, following) =>
            current.fold(Idle)(Start(slot, following, _, superseding = true))
        }
      case None =>
        (next, last) match {
          case (None, _) => Idle
          case (Some(n), Some(l)) if !n.isAfter(l.slot) => Passed(rule.after(l.slot))
          case (Some(n), _) =>
            Due.of(rule, n, now) match {
              case Due.NotYet => Idle
              case Due.Missed(slot) => Miss(slot)
              case Due.Run(slot, following) =>
                current.fold(Idle)(Start(slot, following, _, superseding = false))
            }
        }
    }
}
