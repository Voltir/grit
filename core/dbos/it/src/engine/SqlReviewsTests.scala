package grit.dbos.engine

import grit.core.id.ConversationId
import grit.core.review.{ReviewContract, ReviewStore}
import grit.core.speech.SpeechStore
import grit.core.store.{ConversationStore, EntryStore, Origin, PeriodStore, Tx}
import grit.core.triage.TriageShadows
import grit.dbos.sql.{
  LiveDb,
  SqlConversationStore,
  SqlEntryStore,
  SqlPeriodStore,
  SqlReviews,
  SqlSpeechStore,
  SqlTriageShadows,
  TestPostgres
}

/** The review contract, kept by the SQL store against a real Postgres. */
object SqlReviewsTests extends ReviewContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_reviews")
    LiveEngine.open(c, "test").close()
    c
  }

  private val store = new SqlEntryStore()

  protected val entries: EntryStore = store
  protected val periods: PeriodStore = new SqlPeriodStore(store)
  protected val conversations: ConversationStore = new SqlConversationStore()
  protected val speech: SpeechStore = new SqlSpeechStore
  protected val shadows: TriageShadows = new SqlTriageShadows
  protected val reviews: ReviewStore = new SqlReviews

  protected def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)

  protected def conversation(origin: Origin): ConversationId =
    LiveDb.conversation(config, origin).id
}
