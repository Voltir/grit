package grit.lifecycle.stitch

import grit.act.moves.MovesEnv
import grit.core.clock.Clock
import grit.core.spend.Budget
import grit.core.stitch.{StitchReads, Tuning}
import grit.core.store.Db

/** What a placement works with besides its `Durable`: where it reads the opening and its room
  * and keeps the placement (`reads`, through `db` outside a transaction, under `tuning`),
  * `moves`, whose classifier its ask goes to, admitted by `budget`, and `clock`, for when the
  * placement was kept.
  */
final case class StitchEnv(
    reads: StitchReads,
    moves: MovesEnv^,
    budget: Budget,
    db: Db^,
    clock: Clock^,
    tuning: Tuning
)
