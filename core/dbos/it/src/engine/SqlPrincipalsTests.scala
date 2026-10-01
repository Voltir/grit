package grit.dbos.engine

import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger

import scala.util.Using

import grit.core.id.{EntryId, PrincipalId, TurnSeq}
import grit.core.message.Message
import grit.core.store.{Entry, Origin, Payload, Principals, PrincipalsContract, Tx}
import grit.dbos.sql.{DbConfig, LiveDb, SqlEntryStore, SqlPrincipals, TestPostgres}

/** The principals contract, kept by the SQL store against a real Postgres. */
object SqlPrincipalsTests extends PrincipalsContract {

  // Each test enrolls from nobody, so each gets a database of its own: a shared one would
  // hold an earlier test's names. Only ever holds an immutable config; the suite's tests run
  // one at a time.
  @caps.unsafe.untrackedCaptures
  private var database: Option[DbConfig] = None

  private val made = new AtomicInteger

  private def config: DbConfig =
    database.getOrElse(throw new java.lang.AssertionError("no database"))

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  protected def fresh(): Principals = {
    val c = TestPostgres.freshDatabase(s"sql_principals_${made.incrementAndGet()}")
    LiveEngine.open(c, "test").close()
    database = Some(c)
    new SqlPrincipals()
  }

  protected def said(principals: Principals, by: PrincipalId): EntryId = {
    val c = LiveDb.conversation(config, Origin.Task("principals", UUID.randomUUID().toString)).id
    val id = EntryId(s"in:${UUID.randomUUID()}")
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      new SqlEntryStore()
        .insert(
          Entry(
            id,
            c,
            TurnSeq.First,
            None,
            0L,
            Payload.Message(Message.User("hi")),
            java.time.Instant.EPOCH
          )
        )
        .fold(e => sys.error(s"arranging an entry: $e"), identity)
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement("INSERT INTO grit.inbound (entry_id, author) VALUES (?, ?)")
      ) { ps =>
        ps.setString(1, EntryId.value(id))
        ps.setString(2, PrincipalId.value(by))
        val _ = ps.executeUpdate()
      }
    }
    id
  }

  protected def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)
}
