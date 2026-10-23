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
case class MovesEnv(
    records: MoveRecords,
    models: Models^,
    classifier: ClearedClassifier^,
    db: Db^,
    clock: Clock^
)

/** What a keeping job's moves are made with: `moves`' own, and `files`, the area a piece move
  * reads files from, and `jot`, through which it keeps their pieces in a transaction of its own
  * ([[grit.core.act.Keeping.piece]]).
  */
final class KeepingEnv(moves: MovesEnv^, val files: Files^, val jot: Jot^)
    extends MovesEnv(moves.records, moves.models, moves.classifier, moves.db, moves.clock)
