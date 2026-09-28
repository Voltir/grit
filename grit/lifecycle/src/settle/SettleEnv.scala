package grit.lifecycle.settle

import grit.core.classify.Classifier
import grit.core.clock.Clock
import grit.core.store.{Db, EntryStore, LifecycleStore, PeriodStore, Principals}

/** What a settle works with besides its `Durable`: where it reads the period and keeps the
  * verdict ([[SettleRecords]]), the `classifier` it asks, `db` to read the period's
  * transcript outside a transaction, and `clock` to say when the verdict was given.
  */
final case class SettleEnv(
    records: SettleRecords,
    classifier: Classifier^,
    db: Db^,
    clock: Clock^
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
