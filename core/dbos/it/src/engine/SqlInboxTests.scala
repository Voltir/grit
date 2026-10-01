package grit.dbos.engine

import java.time.Instant

import scala.util.Using

import grit.core.id.{CallSlot, ConversationId, PrincipalId, SourceId, TurnRef, TurnSeq}
import grit.core.inbox.InboxError
import grit.core.message.Message
import grit.core.store.{Origin, StoreError, Tx}
import grit.dbos.sql.{LiveDb, TestPostgres}

import utest.*

/** What only the inbox against a real Postgres shows; the inbox contract is kept by
  * SqlInboxContractTests in grit.app, under a launched engine. The stores' own contract is
  * [[SqlStoreTests]].
  */
object SqlInboxTests extends TestSuite {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_inbox")
    LiveEngine.open(c, "test").close()
    c
  }

  /** Runs `sql`, deleting rows as a purge would. */
  private def execute(sql: String)(using tx: Tx^): Unit = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    Using.resource(conn.createStatement())(_.execute(sql))
    ()
  }

  val tests = Tests {
    test("a post is searched by its text, and the call that made it goes with its entry") {
      val origin = Origin.Task("sql", "posted")
      val slot = CallSlot
        .of(TurnRef(ConversationId("0190a000-0000-7000-8000-000000000001"), TurnSeq.First), 0, 0)
        .getOrElse(throw new java.lang.AssertionError("a slot at 0, 0 reads"))
      val engine = LiveEngine.open(config, "test")
      try {
        engine.inbox.posted(
          origin,
          SourceId("root"),
          "the retries issue is open",
          Instant.parse("2026-09-25T09:00:00Z"),
          slot,
          PrincipalId.Local
        ) ==> Right(true)
        val c = LiveDb.conversation(config, origin).id
        LiveDb
          .transaction(config)(
            engine.search.search(c, TurnSeq.First, TurnSeq.First.next, "retries", 5)
          )
          .map(_.map(_.turn.turnSeq)) ==> Right(Vector(TurnSeq.First))
        LiveDb.transaction(config)(engine.conversations.postedBy(c)) ==> Right(Some(slot))
        LiveDb.transaction(config) {
          execute(
            s"DELETE FROM grit.entries WHERE conversation_id = '${ConversationId.value(c)}'"
          )
          engine.conversations.postedBy(c)
        } ==> (Right(None): Either[StoreError, Option[CallSlot]])
      } finally engine.close()
    }

    test("a turn's progress that cannot be read is Unavailable, never its end") {
      // DBOS never launched on this database, so its workflow tables do not exist.
      val origin = Origin.Task("sql", "progress")
      val engine = LiveEngine.open(config, "test")
      val progress =
        try
          engine.inbox
            .ingest(origin, SourceId("m1"), Message.User("one"), PrincipalId.Local)
            .flatMap(engine.inbox.progress)
        finally engine.close()
      progress.left.map {
        case InboxError.Unavailable(why) => why.contains("dbos.workflow_status")
        case other => false
      } ==> Left(true)
    }
  }
}
