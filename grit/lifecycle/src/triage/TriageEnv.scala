package grit.lifecycle.triage

import grit.core.classify.Classifier
import grit.core.clock.Clock
import grit.core.id.TurnRef
import grit.core.speech.{Speaking, SpeechStore}
import grit.core.spend.{Budget, Spending}
import grit.core.store.{ConversationStore, Db, EntryStore, Principals}
import grit.core.triage.TriageStore

/** What a triage works with besides its `Durable`: where it reads the heard message and
  * keeps its tags ([[TriageRecords]]), the `classifier` it asks, `db` to read the thread
  * outside a transaction, `clock` to say when the tags were made, and whether grit then
  * drafts a reply ([[TriageSpeech]]).
  */
final case class TriageEnv(
    records: TriageRecords,
    classifier: Classifier^,
    db: Db^,
    clock: Clock^,
    speech: TriageSpeech^
)

/** Where a triage reads the heard message, its thread, its conversation and who wrote them,
  * keeps its tags, and weighs and keeps its decision to speak: the speech ledger (`speech`)
  * and the day's spend (`spending`).
  */
final case class TriageRecords(
    entries: EntryStore,
    triage: TriageStore,
    principals: Principals,
    conversations: ConversationStore,
    speech: SpeechStore,
    spending: Spending
)

/** Whether and within what grit speaks where it was not addressed (`speaking`, read when a
  * message is considered), the deployment's `budget`, and `start`, which queues the heard
  * message's turn once grit drafts a reply to it; why not, when it could not.
  */
final case class TriageSpeech(
    speaking: Speaking,
    budget: Budget,
    start: TurnRef => Either[String, Unit]
)
