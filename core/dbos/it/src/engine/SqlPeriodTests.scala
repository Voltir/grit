package grit.dbos.engine

import scala.util.Using

import grit.core.id.{ConversationId, PeriodRef, PeriodSeq}
import grit.core.store.{EntryStore, LifecycleStore, Origin, PeriodContract, PeriodStore, Tx}
import grit.dbos.sql.{LiveDb, SqlEntryStore, SqlLifecycleStore, SqlPeriodStore, TestPostgres}

/** The period contract, kept by the SQL stores against a real Postgres. */
object SqlPeriodTests extends PeriodContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_period")
    LiveEngine.open(c, "test").close()
    c
  }

  private val store = new SqlEntryStore()

  protected val entries: EntryStore = store
  protected val periods: PeriodStore = new SqlPeriodStore(store)
  protected val lifecycle: LifecycleStore = new SqlLifecycleStore()
  protected val requests: grit.core.edge.ToolRequests = new grit.dbos.sql.SqlToolRequests()
  protected val deliveries: grit.core.edge.Deliveries = new grit.dbos.sql.SqlDeliveries()
  protected val acknowledgements: grit.core.edge.Acknowledgements =
    new grit.dbos.sql.SqlAcknowledgements()

  protected def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)

  protected def conversation(name: String): ConversationId =
    LiveDb.conversation(config, origin(name)).id

  protected def origin(name: String): Origin = Origin.Task("contract", name)

  protected def verdictsOn(period: PeriodRef): Int =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement(
          "SELECT count(*) FROM grit.verdicts WHERE conversation_id = ?::uuid AND seq = ?"
        )
      ) { ps =>
        ps.setString(1, ConversationId.value(period.conversationId))
        ps.setLong(2, PeriodSeq.value(period.seq))
        Using.resource(ps.executeQuery())(rs => { rs.next(); rs.getInt(1) })
      }
    }
}
