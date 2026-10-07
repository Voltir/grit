package grit.lifecycle.triage

import grit.core.classify.Classifier
import grit.core.clock.Clock
import grit.core.edge.{Acknowledgements, Deliveries}
import grit.core.id.TurnRef
import grit.core.recipe.RoomReads
import grit.core.speech.{Speaking, SpeechStore}
import grit.core.spend.{Budget, Spending}
import grit.core.stitch.{Placements, StitchStore, Tuning}
import grit.core.store.{ConversationStore, Db, EntrySearch, EntryStore, LifecycleStore, Principals}
import grit.core.triage.{Corpora, TriageStore}

/** What a triage works with besides its `Durable`: where it reads the heard message and
  * keeps its tags ([[TriageRecords]]), the `classifier` it asks, `db` to read the thread
  * outside a transaction, `clock` to say when the tags were made, whether grit then drafts a
  * reply ([[TriageSpeech]]), how its strand is read and, in a triage begun before openings
  * were placed on their own, its opening stitched (`tuning`), where its opening is placed
  * (`placements`), the deployment's knowledge `sources`, those covering a message's
  * conversation asked about one by one, and `questions`, the set it asks
  * ([[TriageQuestions.shipped]] of the deployment's persona).
  */
final case class TriageEnv(
    records: TriageRecords,
    classifier: Classifier^,
    db: Db^,
    clock: Clock^,
    speech: TriageSpeech^,
    tuning: Tuning,
    placements: Placements^,
    sources: Corpora,
    questions: TriageQuestions
)

/** Where a triage reads the heard message, its thread, its conversation and who wrote them,
  * keeps its tags, and weighs and keeps its decision to speak: the speech ledger (`speech`)
  * and the day's spend (`spending`); for a message it answers as said to grit
  * ([[grit.core.speech.Decision.Answering]]), the mark it wants while its turn runs
  * (`acknowledgements`) and its reply awaited (`deliveries`); where a heard first message is stitched: its room's
  * exchanges and strands (`stitches`), searched (`search`), in the scope in force
  * (`lifecycle`); and what its room said before it (`rooms`).
  */
final case class TriageRecords(
    entries: EntryStore,
    triage: TriageStore,
    principals: Principals,
    conversations: ConversationStore,
    speech: SpeechStore,
    spending: Spending,
    acknowledgements: Acknowledgements,
    deliveries: Deliveries,
    stitches: StitchStore,
    search: EntrySearch,
    lifecycle: LifecycleStore,
    rooms: RoomReads
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
