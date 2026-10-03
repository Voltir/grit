package grit.core.stitch

import scala.concurrent.duration.FiniteDuration

import grit.core.clock.Clock

/** Where each [[Opening]] is placed: by a workflow of its own, one at a time per room, in the
  * order queued; the inbox queues one as its message is recorded, so in the order openings
  * reached grit. A placement whose classifier call hangs holds every later opening of its
  * room until the classifier's own timeout ends the call.
  */
trait Placements extends caps.SharedCapability {

  /** Waits for `opening`'s placement to end, first queueing it if it is not queued (then it
    * runs after the room's queued ones, not in the order said): what it did, for logs; the
    * placement is in [[StitchStore]]. Blocks while the room's earlier placements run. `Left`
    * when the queue cannot be reached or the placement failed.
    */
  def awaited(opening: Opening): Either[String, String]

  /** [[awaited]], giving up once `within` has passed on `clock`: `Left` too when the placement
    * has not ended by then. It goes on, and a later wait finds it.
    */
  def awaitedWithin(opening: Opening, within: FiniteDuration, clock: Clock^): Either[String, String]
}
