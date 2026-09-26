package grit.dbos.engine

import java.time.Instant

import grit.core.id.{CloseRef, PeriodRef, PeriodSeq, SourceId, TurnSeq}
import grit.core.inbox.Signalled
import grit.core.message.Message
import grit.core.period.{CloseReason, Closing, PeriodState}
import grit.core.store.{Origin, Sealed}
import grit.dbos.sql.{LiveDb, SqlEntryStore, SqlPeriodStore, TestPostgres}

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

    test("ingest opens a conversation's first period, and after a close the next, at its turn") {
      val origin = Origin.Task("sql", "periods")
      val periods = new SqlPeriodStore(new SqlEntryStore())
      val engine = Engine.open(config, "test")
      try {
        val one = engine.inbox.ingest(origin, SourceId("p1"), Message.User("one"))
        val c = LiveDb.conversation(config, origin).id
        val p1 = PeriodRef(c, PeriodSeq.First)
        LiveDb.transaction(config)(periods.get(p1)).map(_.map(p => (p.first, p.state))) ==>
          Right(Some((TurnSeq(0), PeriodState.Open(None))))
        val closing =
          Closing
            .of("one", None, Vector(), Vector(), Vector(), Vector())
            .getOrElse(sys.error("closing"))
        LiveDb.transaction(config)(
          periods.seal(CloseRef(p1, TurnSeq(0)), CloseReason.Lapsed, closing, Instant.now())
        ) ==> Right(Sealed.Closed(p1.closingId))
        val two = engine.inbox.ingest(origin, SourceId("p2"), Message.User("two"))
        (one.map(_.turnSeq), two.map(_.turnSeq)) ==> (Right(TurnSeq(0)), Right(TurnSeq(1)))
        val p2 = PeriodRef(c, PeriodSeq.First.next)
        LiveDb.transaction(config)(periods.get(p2)).map(_.map(p => (p.first, p.state))) ==>
          Right(Some((TurnSeq(1), PeriodState.Open(None))))
      } finally engine.close()
    }

    test("a signal is recorded once until something happens; with nothing open, NothingOpen") {
      val origin = Origin.Task("sql", "signal")
      val periods = new SqlPeriodStore(new SqlEntryStore())
      val engine = Engine.open(config, "test")
      try {
        engine.inbox.signal(origin) ==> Right(Signalled.NothingOpen)
        engine.inbox.ingest(origin, SourceId("s1"), Message.User("one"))
        val p1 = PeriodRef(LiveDb.conversation(config, origin).id, PeriodSeq.First)
        def signalled = LiveDb.transaction(config)(periods.activity(p1)).map(_.flatMap(_.signalled))
        engine.inbox.signal(origin) ==> Right(Signalled.Closing)
        val first = signalled
        assert(first.exists(_.nonEmpty))
        engine.inbox.signal(origin) ==> Right(Signalled.Closing)
        signalled ==> first
      } finally engine.close()
    }
  }
}
