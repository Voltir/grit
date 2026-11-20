package grit.core.occasion

import java.time.Instant

import grit.core.id.DocumentVersionId
import grit.core.place.Place

/** A job's ground for grit to post unasked at `room`'s top level, about its plugin's document
  * `about`, before `until`; at most once for `key` in its room's top-level conversation
  * ([[grit.core.act.Keeping.occasion]]).
  */
final case class Occasion(
    room: Place,
    key: OccasionKey,
    reason: Occasion.Reason,
    about: DocumentVersionId,
    until: Instant
) extends caps.Pure

object Occasion {

  /** Something the room planned is coming up (`Ahead`); or something it talked of was left open
    * (`Open`).
    */
  enum Reason extends caps.Pure {
    case Ahead, Open
  }
}
