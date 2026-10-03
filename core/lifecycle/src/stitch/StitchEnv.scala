package grit.lifecycle.stitch

import grit.core.classify.Classifier
import grit.core.clock.Clock
import grit.core.stitch.{StitchReads, Tuning}
import grit.core.store.Db

/** What a placement works with besides its `Durable`: where it reads the opening and its room
  * and keeps the placement (`reads`, through `db` outside a transaction, under `tuning`), the
  * `classifier` it asks, and `clock`, for when the placement was kept.
  */
final case class StitchEnv(
    reads: StitchReads,
    classifier: Classifier^,
    db: Db^,
    clock: Clock^,
    tuning: Tuning
)
