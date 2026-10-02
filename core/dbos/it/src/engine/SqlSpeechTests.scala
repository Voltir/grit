package grit.dbos.engine

import java.time.{Instant, ZoneOffset}

import grit.core.id.ConversationId
import grit.core.speech.{SpeechContract, SpeechStore}
import grit.core.spend.Day
import grit.core.store.{EntryStore, Origin, PeriodStore, Tx, UsageLedger}
import grit.dbos.sql.{
  LiveDb,
  SqlEntryStore,
  SqlPeriodStore,
  SqlSpeechStore,
  SqlUsageLedger,
  TestPostgres
}

/** The speech contract, kept by the SQL store against a real Postgres. */
object SqlSpeechTests extends SpeechContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_speech")
    LiveEngine.open(c, "test").close()
    c
  }

  private val store = new SqlEntryStore()

  protected val entries: EntryStore = store
  protected val periods: PeriodStore = new SqlPeriodStore(store)
  protected val ledger: UsageLedger = new SqlUsageLedger
  protected val speech: SpeechStore = new SqlSpeechStore

  protected def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)

  protected def conversation(name: String): ConversationId =
    LiveDb.conversation(config, Origin.Task("contract", name)).id

  // A row is recorded at the transaction's time, now.
  protected def today: Day = Day.at(Instant.now(), ZoneOffset.UTC)
}
