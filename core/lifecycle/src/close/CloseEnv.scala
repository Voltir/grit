package grit.lifecycle.close

import grit.core.classify.Classifier
import grit.core.clock.Clock
import grit.core.provider.{Models, TokenEstimator}
import grit.core.store.{
  ConversationStore,
  Db,
  EntryStore,
  LifecycleStore,
  PeriodStore,
  Principals,
  Tombstones,
  UsageLedger
}
import grit.core.triage.TriageStore

/** What a close works with besides its `Durable`: where it reads and writes ([[CloseRecords]]),
  * then the capabilities it calls. `classifier` decides which sections the closing needs
  * ([[CloseGate]]), `models` writes it under the catalog's summary pin, `db` reads the
  * period's transcript outside a transaction, and `clock` says when the check and the seal
  * happen.
  */
final case class CloseEnv(
    records: CloseRecords,
    classifier: Classifier^,
    models: Models^,
    db: Db^,
    clock: Clock^
)

/** Where a close reads its period, who wrote it, what triage made of its heard messages and
  * where its conversation happens, and writes its closing entry, the entry's cost, the
  * tombstones its seal writes, and how a request is priced.
  */
final case class CloseRecords(
    entries: EntryStore,
    periods: PeriodStore,
    lifecycle: LifecycleStore,
    ledger: UsageLedger,
    tombstones: Tombstones,
    estimator: TokenEstimator,
    principals: Principals,
    triage: TriageStore,
    conversations: ConversationStore
)
