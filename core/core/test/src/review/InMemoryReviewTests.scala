package grit.core.review

import grit.core.id.ConversationId
import grit.core.identity.Account
import grit.core.speech.{InMemorySpeechStore, SpeechStore}
import grit.core.store.{
  ConversationStore,
  EntryStore,
  InMemoryConversationStore,
  InMemoryEntryStore,
  InMemoryPeriodStore,
  InMemoryUsageLedger,
  Origin,
  PeriodStore,
  Tx
}
import grit.core.triage.{InMemoryTriageShadows, InMemoryTriageStore, TriageShadows}
import grit.core.visibility.{Clearance, Label, Visibility}
import grit.dbos.sql.TestTx

/** The review contract, kept by the in-memory fake. */
object InMemoryReviewTests extends ReviewContract {

  private val store = new InMemoryEntryStore
  private val said = new InMemorySpeechStore(store, new InMemoryUsageLedger)
  private val kept = new InMemoryConversationStore(Some(store))
  private val spans = new InMemoryPeriodStore(store)
  private val answered = new InMemoryTriageShadows(store, new InMemoryTriageStore(store, spans))

  protected val entries: EntryStore = store
  protected val periods: PeriodStore = spans
  protected val conversations: ConversationStore = kept
  protected val speech: SpeechStore = said
  protected val shadows: TriageShadows = answered
  protected val reviews: ReviewStore = new InMemoryReviews(store, kept, said, answered)

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)

  protected def transactionUnder[A](visibility: Visibility)(body: (Tx^) ?=> A): A =
    body(using TestTx.fake(Clearance.of(Label.Public), visibility))

  protected def conversation(origin: Origin, label: Label): ConversationId =
    transaction(kept.findOrCreate(origin, Account.Local, label))
      .fold(e => throw new java.lang.AssertionError(s"$e"), _.id)
}
