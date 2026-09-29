package grit.lifecycle.triage

import grit.core.classify.Classifier
import grit.core.clock.Clock
import grit.core.store.{Db, EntryStore, Principals}
import grit.core.triage.TriageStore

/** What a triage works with besides its `Durable`: where it reads the heard message and
  * keeps its tags ([[TriageRecords]]), the `classifier` it asks, `db` to read the thread
  * outside a transaction, and `clock` to say when the tags were made.
  */
final case class TriageEnv(
    records: TriageRecords,
    classifier: Classifier^,
    db: Db^,
    clock: Clock^
)

/** Where a triage reads the heard message, its thread and who wrote them, and keeps its
  * tags.
  */
final case class TriageRecords(entries: EntryStore, triage: TriageStore, principals: Principals)
