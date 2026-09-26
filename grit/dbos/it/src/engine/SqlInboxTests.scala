package grit.dbos.engine

import grit.core.id.{SourceId, TurnSeq}
import grit.core.message.Message
import grit.core.store.Origin
import grit.dbos.sql.{LiveDb, SqlEntryStore, TestPostgres}

import utest.*

/** The inbox against a real Postgres. The stores' own contract is [[SqlStoreTests]]. */
object SqlInboxTests extends TestSuite {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_inbox")
    Engine.open(c, "test").close()
    c
  }

  val tests = Tests {

    test("ingest: the same source is the same turn, a new one the next turn") {
      val origin = Origin.Task("sql", "ingest")
      val engine = Engine.open(config, "test")
      val (first, again, second) =
        try {
          (
            engine.inbox.ingest(origin, SourceId("m1"), Message.User("one")),
            engine.inbox.ingest(origin, SourceId("m1"), Message.User("one, redelivered")),
            engine.inbox.ingest(origin, SourceId("m2"), Message.User("two"))
          )
        } finally engine.close()
      again ==> first
      (first.map(_.turnSeq), second.map(_.turnSeq)) ==> (Right(TurnSeq(0)), Right(TurnSeq(1)))
      val c = LiveDb.conversation(config, origin).id
      LiveDb.transaction(config)(new SqlEntryStore().list(c)).map(_.size) ==> Right(2)
    }
  }
}
