package grit.act.moves

import grit.core.clock.Clock
import grit.core.edge.{EdgeDirectory, ToolRequests}
import grit.core.job.ScheduleStore
import grit.core.provider.{Models, TokenEstimator}
import grit.core.spend.Spending
import grit.core.store.{Askers, Db, Savepoints, UsageLedger}
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
  * assignment's provider; `db`, whose read admits an ask; `clock`, which dates and paces them.
  * Every write a move makes is in its own step's transaction.
  */
final case class MovesEnv(records: MoveRecords, models: Models^, db: Db^, clock: Clock^)
