package grit.core.speech

import java.time.{Instant, ZoneOffset}

import grit.core.id.{ConversationId, TurnRef}
import grit.core.identity.Account
import grit.core.spend.Day
import grit.core.store.{
  EntryStore,
  InMemoryConversationStore,
  InMemoryEntryStore,
  InMemoryPeriodStore,
  InMemoryUsageLedger,
  Origin,
  PeriodStore,
  Tx,
  UsageLedger
}
import grit.core.triage.Cues
import grit.core.visibility.Label
import grit.dbos.sql.TestTx

/** The speech contract, kept by the in-memory fake. */
object InMemorySpeechTests extends SpeechContract {

  private val store = new InMemoryEntryStore
  private val rows = new InMemoryUsageLedger
  private val now = Instant.parse("2026-09-30T12:00:00Z")
  rows.now = now

  protected val entries: EntryStore = store
  protected val periods: PeriodStore = new InMemoryPeriodStore(store)
  protected val ledger: UsageLedger = rows
  private val conversations = new InMemoryConversationStore(Some(store))
  private val kept = new InMemorySpeechStore(store, rows, conversations)
  protected val speech: SpeechStore = kept

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)

  protected def conversation(name: String): ConversationId = ConversationId(name)

  protected def conversationAt(origin: Origin): ConversationId =
    transaction(conversations.findOrCreate(origin, Account.Local, Label.Public))
      .fold(e => throw new java.lang.AssertionError(e.toString), _.id)

  protected def today: Day = Day.at(now, ZoneOffset.UTC)

  protected def cuesKept(turn: TurnRef): Option[Cues] =
    kept.decisions.collectFirst { case (h, _, _) if h.turn == turn => h.cues }
}
