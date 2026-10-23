package grit.act.moves

import grit.core.classify.ClearedClassifier
import grit.core.clock.Clock
import grit.core.edge.{EdgeDirectory, ToolRequests}
import grit.core.files.Files
import grit.core.job.ScheduleStore
import grit.core.provider.{Models, TokenEstimator}
import grit.core.spend.Spending
import grit.core.store.{Askers, Db, Jot, Savepoints, UsageLedger}
import grit.core.tool.ToolSets

/** The stores a planner's moves read and write: the ledger and the day's spend, edges'
  * requests, who serves where and what they advertise, the schedules and askers (for
  * `actsFor`), how a request is priced, and the savepoints a keep's writes are undone by.
  */
final case class MoveRecords(
    ledger: UsageLedger,
    spending: Spending,
    requests: ToolRequests,
    edges: EdgeDirectory,
    toolSets: ToolSets,
    schedules: ScheduleStore,
    askers: Askers,
    estimator: TokenEstimator,
    savepoints: Savepoints
)

/** What a planner's moves are made with besides its `Durable`: `models`, for the summary
  * assignment's provider; `classifier`, the deployment's, for judgments; `db`, whose read admits
  * an ask; `clock`, which dates and paces them. Every write a move makes is in its own step's
  * transaction. A keeping job's moves are made with more ([[KeepingEnv]]).
  */
final case class MovesEnv(
    records: MoveRecords,
    models: Models^,
    classifier: ClearedClassifier^,
    db: Db^,
    clock: Clock^
)

/** What a keeping job's moves are made with: every move's `moves`, and `files`, the area a piece
  * move reads files from, and `jot`, through which it keeps their pieces in a transaction of its
  * own ([[grit.core.act.Keeping.piece]]).
  */
final case class KeepingEnv(moves: MovesEnv^, files: Files^, jot: Jot^)

object KeepingEnv {

  /** The [[MovesEnv]] in `env`, every move's. */
  // A value, not a lambda written where it is passed: in a class's super call, a lambda's root
  // capability cannot flow into the synthesized super-argument method's.
  val moves: (env: KeepingEnv^) -> MovesEnv^{env} = env => env.moves
}
