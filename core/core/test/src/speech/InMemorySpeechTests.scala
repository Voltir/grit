package grit.core.speech

import java.time.{Instant, ZoneOffset}

import grit.core.id.ConversationId
import grit.core.spend.Day
import grit.core.store.{
  EntryStore,
  InMemoryEntryStore,
  InMemoryPeriodStore,
  InMemoryUsageLedger,
  PeriodStore,
  Tx,
  UsageLedger
}
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
  protected val speech: SpeechStore = new InMemorySpeechStore(store, rows)

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)

  protected def conversation(name: String): ConversationId = ConversationId(name)

  protected def today: Day = Day.at(now, ZoneOffset.UTC)
}
