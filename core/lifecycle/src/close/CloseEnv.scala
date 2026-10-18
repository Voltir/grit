package grit.lifecycle.close

import grit.act.moves.MovesEnv
import grit.core.clock.Clock
import grit.core.persona.Persona
import grit.core.spend.Budget
import grit.core.store.{
  ConversationStore,
  Db,
  EntryStore,
  LifecycleStore,
  PeriodStore,
  Principals,
  Tombstones
}
import grit.core.triage.TriageStore

/** What a close works with besides its `Durable`: where it reads and writes ([[CloseRecords]]),
  * then the capabilities it calls. `moves`' classifier decides which sections the closing
  * needs ([[CloseGate]]) and its models write it under the catalog's summary pin, each ask
  * admitted by `budget` and its cost kept in `moves`' ledger; `db` reads the period's
  * transcript outside a transaction, and `clock` says when the check and the seal happen. The
  * assistant presents as `persona`: a person's mention of it is read as speaking to it.
  */
final case class CloseEnv(
    records: CloseRecords,
    moves: MovesEnv^,
    budget: Budget,
    db: Db^,
    clock: Clock^,
    persona: Persona
)

/** Where a close reads its period, who wrote it, what triage made of its heard messages and
  * where its conversation happens, and writes its closing entry and the tombstones its seal
  * writes.
  */
final case class CloseRecords(
    entries: EntryStore,
    periods: PeriodStore,
    lifecycle: LifecycleStore,
    tombstones: Tombstones,
    principals: Principals,
    triage: TriageStore,
    conversations: ConversationStore
)
