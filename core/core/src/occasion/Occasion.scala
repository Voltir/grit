package grit.core.occasion

import java.time.Instant

import grit.core.id.DocumentVersionId
import grit.core.place.Place

/** A job's ground for grit to post unasked at `room`'s top level, worded by its plugin as
  * `brief`, about its plugin's document `about`, before `until`; at most once for `key` in its
  * room's top-level conversation ([[grit.core.act.Keeping.occasion]]).
  */
final case class Occasion(
    room: Place,
    key: OccasionKey,
    brief: OccasionBrief,
    about: DocumentVersionId,
    until: Instant
) extends caps.Pure
