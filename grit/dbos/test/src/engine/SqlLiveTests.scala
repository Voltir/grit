package grit.dbos.engine

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, SourceId, TurnSeq, WorkflowId}
import grit.core.message.{Message, Tokens, Usage}
import grit.core.store.{Entry, EntryStore, Origin, Payload, StoreError}
import grit.dbos.sql.{LiveDb, SqlEntryStore, SqlUsageLedger, TestPostgres}

import utest.*

/** grit.dbos's stores and inbox against a real Postgres. */
object SqlLiveTests extends TestSuite {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_live")
    Engine.open(c, "test").close()
    c
  }

  private val entries = new SqlEntryStore()

  private def entry(conversation: ConversationId, id: String, seq: Long): Entry =
    Entry(
      EntryId(id),
      conversation,
      TurnSeq.First,
      None,
      seq,
      Payload.Message(Message.User(id)),
      Instant.EPOCH
    )

  val tests = Tests {

    test("entries come back in seq order, and lockNext is past them") {
      val c = LiveDb.conversation(config, Origin.Task("sql", "order")).id
      LiveDb.transaction(config) {
        entries.insert(entry(c, "second", 5))
        entries.insert(entry(c, "first", 2))
      }
      LiveDb.transaction(config)(entries.list(c)).map(_.map(e => EntryId.value(e.id))) ==>
        Right(Vector("first", "second"))
      LiveDb.transaction(config)(entries.lockNext(c)) ==>
        Right(EntryStore.Next(TurnSeq(1), 6L))
    }

    test("an id already taken is DuplicateId; a seq already taken is a DatabaseError") {
      val c = LiveDb.conversation(config, Origin.Task("sql", "clash")).id
      LiveDb.transaction(config)(entries.insert(entry(c, "one", 0))) ==> Right(())
      LiveDb.transaction(config)(entries.insert(entry(c, "one", 1))) ==>
        Left(StoreError.DuplicateId(EntryId("one")))
      val seqTaken = LiveDb.transaction(config)(entries.insert(entry(c, "two", 0)))
      assert(seqTaken match {
        case Left(StoreError.DatabaseError(cause)) => cause.contains("entries_conversation_seq")
        case _ => false
      })
    }

    test("a DuplicateId leaves the transaction usable") {
      val c = LiveDb.conversation(config, Origin.Task("sql", "savepoint")).id
      val after = LiveDb.transaction(config) {
        entries.insert(entry(c, "dup", 0))
        val again = entries.insert(entry(c, "dup", 1))
        (again, entries.insert(entry(c, "after", 2)))
      }
      after ==> (Left(StoreError.DuplicateId(EntryId("dup"))), Right(()))
    }

    test("the ledger records a response once, with its exact cost") {
      val c = LiveDb.conversation(config, Origin.Task("sql", "ledger")).id
      val ledger = new SqlUsageLedger()
      val usage = Usage(Tokens(10), Tokens(5), Tokens(2), Some(BigDecimal("0.0000123")))
      val outcome = LiveDb.transaction(config) {
        entries.insert(entry(c, "costly", 0))
        val first = ledger.record(EntryId("costly"), WorkflowId("w"), "m", usage)
        (first, ledger.record(EntryId("costly"), WorkflowId("w"), "m", usage))
      }
      outcome ==> (Right(()), Left(StoreError.DuplicateId(EntryId("costly"))))
      LiveDb.ledger(config).filter(_._1 == "costly") ==>
        Vector(("costly", "m", Some(BigDecimal("0.0000123"))))
    }

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
      LiveDb.transaction(config)(entries.list(c)).map(_.size) ==> Right(2)
    }
  }
}
