package grit.dbos.engine

import java.time.Instant

import grit.core.id.{CloseRef, PeriodRef, PeriodSeq, PrincipalId, SourceId, TurnSeq}
import grit.core.message.Message
import grit.core.period.{CloseReason, PeriodState, TestClosings}
import grit.core.store.{Origin, Sealed}
import grit.dbos.sql.{LiveDb, SqlEntryStore, SqlPeriodStore, TestPostgres}

import utest.*

/** The inbox against a real Postgres. The stores' own contract is [[SqlStoreTests]]. */
object SqlInboxTests extends TestSuite {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_inbox")
    LiveEngine.open(c, "test").close()
    c
  }

  val tests = Tests {

    test("ingest: the same source is the same turn, a new one the next turn") {
      val origin = Origin.Task("sql", "ingest")
      val engine = LiveEngine.open(config, "test")
      val (first, again, second) =
        try {
          (
            engine.inbox.ingest(origin, SourceId("m1"), Message.User("one"), PrincipalId.Local),
            engine.inbox.ingest(
              origin,
              SourceId("m1"),
              Message.User("one, redelivered"),
              PrincipalId.Local
            ),
            engine.inbox.ingest(origin, SourceId("m2"), Message.User("two"), PrincipalId.Local)
          )
        } finally engine.close()
      again ==> first
      (first.map(_.turnSeq), second.map(_.turnSeq)) ==> (Right(TurnSeq(0)), Right(TurnSeq(1)))
      val c = LiveDb.conversation(config, origin).id
      LiveDb.transaction(config)(new SqlEntryStore().list(c)).map(_.size) ==> Right(2)
    }

    test("ingest opens a conversation's first period, and after a close the next, at its turn") {
      val origin = Origin.Task("sql", "periods")
      val periods = new SqlPeriodStore(new SqlEntryStore())
      val engine = LiveEngine.open(config, "test")
      try {
        val one =
          engine.inbox.ingest(origin, SourceId("p1"), Message.User("one"), PrincipalId.Local)
        val c = LiveDb.conversation(config, origin).id
        val p1 = PeriodRef(c, PeriodSeq.First)
        LiveDb.transaction(config)(periods.get(p1)).map(_.map(p => (p.first, p.state))) ==>
          Right(Some((TurnSeq(0), PeriodState.Open)))
        val closing =
          TestClosings.prose("one")
        LiveDb.transaction(config)(
          periods.seal(
            CloseRef(p1, TurnSeq(0), Instant.EPOCH),
            CloseReason.Lapsed,
            closing,
            Instant.now()
          )
        ) ==> Right(Sealed.Closed(p1.closingId))
        val two =
          engine.inbox.ingest(origin, SourceId("p2"), Message.User("two"), PrincipalId.Local)
        (one.map(_.turnSeq), two.map(_.turnSeq)) ==> (Right(TurnSeq(0)), Right(TurnSeq(1)))
        val p2 = PeriodRef(c, PeriodSeq.First.next)
        LiveDb.transaction(config)(periods.get(p2)).map(_.map(p => (p.first, p.state))) ==>
          Right(Some((TurnSeq(1), PeriodState.Open)))
      } finally engine.close()
    }

    test("an ingested message records who wrote it, and a redelivery by another keeps the first") {
      val origin = Origin.Task("sql", "authors")
      val engine = LiveEngine.open(config, "test")
      try {
        engine.inbox.ingest(origin, SourceId("a1"), Message.User("one"), PrincipalId.Local)
        engine.inbox.ingest(origin, SourceId("a1"), Message.User("one"), PrincipalId.Grit)
      } finally engine.close()
      val c = LiveDb.conversation(config, origin).id
      LiveDb.inbound(config, c) ==> Vector((SqlInbox.entryId(c, SourceId("a1")), "local"))
    }
  }
}
