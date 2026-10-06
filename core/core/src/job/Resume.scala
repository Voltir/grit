package grit.core.job

import java.time.Instant

/** The state of the run a schedule has in flight, as the inbox reads it. */
enum InFlight {
  case Replied

  /** Its workflow, a run at `version`, has ended without a reply, however it ended: in error,
    * cancelled, or cleanly, as a run superseded or whose job left the deployment does.
    */
  case Ended(version: Int)

  /** DBOS does not know its workflow: its start was lost before it was enqueued. */
  case Unknown
  case Going(version: Int)
}

/** What a schedule with a run in flight gets. */
enum Resume {

  /** Nothing: the run replied, is going at the current version, or is going while the
    * deployment lacks its job (no version to start another at).
    */
  case Leave

  /** The run's start was lost: enqueue it again. */
  case Enqueue

  /** The run ended without a reply at the current version, or with its job gone: a once
    * schedule ends `failed`; a recurrence clears it and goes on to `next`.
    */
  case Fail

  /** Start `slot` at the current version, the run in flight being at another, going or ended
    * without a reply. It is the same slot (a) when no later one is due, or the latest due one
    * (b) when a recurrence has one; the stale run ends superseded either way. A slot started
    * in time is never missed for running again.
    */
  case Supersede(slot: Instant, following: Option[Instant])
}

object Resume {

  /** What a schedule gets at `now` whose run is `flight`, started for its slot at `started`,
    * with its next slot at `next` (`None`: it has none left) and its job's current version
    * `current` (`None`: the deployment lacks its job).
    */
  def of(
      flight: InFlight,
      current: Option[Int],
      rule: SlotRule,
      started: Instant,
      next: Option[Instant],
      now: Instant
  ): Resume = flight match {
    case InFlight.Replied => Leave
    case InFlight.Unknown => Enqueue
    // A run at another version ends with no reply when its reply step finds the job changed:
    // superseded, not failed, so its slot runs again whether or not it has ended yet.
    case InFlight.Ended(version) if current.exists(_ != version) =>
      supersede(rule, started, next, now)
    case InFlight.Ended(_) => Fail
    case InFlight.Going(version) =>
      current match {
        case None => Leave
        case Some(v) if v == version => Leave
        case Some(_) => supersede(rule, started, next, now)
      }
  }

  private def supersede(
      rule: SlotRule,
      started: Instant,
      next: Option[Instant],
      now: Instant
  ): Resume =
    next.map(Due.of(rule, _, now)) match {
      case Some(Due.Run(latest, following)) => Supersede(latest, following)
      case Some(Due.NotYet | Due.Missed(_)) | None => Supersede(started, next)
    }
}
