package grit.lifecycle.settle

import grit.act.moves.MovesEnv
import grit.core.clock.Clock
import grit.core.spend.Budget
import grit.core.store.{Db, EntryStore, LifecycleStore, PeriodStore, Principals}

/** What a settle works with besides its `Durable`: where it reads the period and keeps the
  * verdict ([[SettleRecords]]), `db` to read the period's transcript outside a transaction,
  * `clock` to say when the verdict was given, and `moves`, whose classifier its ask goes to,
  * admitted by `budget`.
  */
final case class SettleEnv(
    records: SettleRecords,
    db: Db^,
    clock: Clock^,
    moves: MovesEnv^,
    budget: Budget
)

/** Where a settle reads its period, who wrote it and the settings in force, and keeps its
  * verdict.
  */
final case class SettleRecords(
    entries: EntryStore,
    periods: PeriodStore,
    lifecycle: LifecycleStore,
    principals: Principals
)
