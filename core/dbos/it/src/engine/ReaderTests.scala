package grit.dbos.engine

import java.time.Instant
import java.util.concurrent.ConcurrentLinkedQueue

import scala.annotation.unused
import scala.concurrent.duration.*
import scala.util.Using

import grit.core.durable.Durable
import grit.core.id.{PeriodRef, PeriodSeq, PrincipalId, SourceId, TriageRef, TurnSeq, WorkflowId}
import grit.core.store.{Origin, StoreError, Tx}
import grit.core.triage.Tags
import grit.dbos.sql.{LiveDb, SqlEntryStore, TestPostgres}

import utest.*

/** A database read without an engine: what it reads, and that it cannot write. */
object ReaderTests extends TestSuite {

  private val nothing = (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id)

  private def eventually(done: => Boolean): Boolean = {
    val until = System.nanoTime() + 30.seconds.toNanos
    var held = done
    while (!held && System.nanoTime() < until) { Thread.sleep(50); held = done }
    held
  }

  private def right[A](e: Either[StoreError, A]): A =
    e.fold(err => throw new java.lang.AssertionError(s"store failed: $err"), identity)

  val tests = Tests {
    test("a reader reads what the engine wrote, beside it: entries, tags, a workflow, starts") {
      val config = TestPostgres.freshDatabase("reader_reads")
      val ran = new ConcurrentLinkedQueue[String]()
      def triage(id: WorkflowId)(using @unused d: Durable^): String = {
        ran.add(WorkflowId.value(id)); "triaged"
      }
      val engine = LiveEngine.open(config, "test")
      try {
        engine.launch(nothing, nothing, nothing, nothing, triage, Vector.empty)
        val here = Origin.Task("reader", "reads")
        engine.inbox.hear(
          here,
          SourceId("m1"),
          "standup moves to 10:00",
          PrincipalId.Local,
          Instant.now(),
          grit.core.speech.Reach.Nowhere
        ) ==> Right(())
        val c = LiveDb.conversation(config, here).id
        val ref = TriageRef(PeriodRef(c, PeriodSeq.First), TurnSeq(0))
        assert(eventually(!ran.isEmpty))
        val heard = right(LiveDb.transaction(config)(new SqlEntryStore().list(c)))
        val at = Instant.parse("2026-10-02T12:00:00Z")
        val tags = Tags.Unanswered("unavailable: test")
        heard.headOption.foreach { e =>
          LiveDb.transaction(config)(engine.triage.record(e.id, tags, at)) ==> Right(true)
        }
        val reader = Reader.open(config)
        try {
          reader.db.read(reader.entries.list(c)) ==> engine.db.read(engine.entries.list(c))
          reader.db
            .read(reader.triage.tagged(at, at.plusSeconds(1)))
            .map(_.map(t => (t.triage, t.tags))) ==>
            Right(Vector((ref, tags)))
          assert(eventually(reader.workflow(ref.workflowId).exists(_.status == "SUCCESS")))
          reader.workflow(ref.workflowId).map(_.epoch) ==> Some("test")
          reader.workflow(WorkflowId("never")) ==> None
          reader.starts().map(_.map(s => (s.epoch, s.build))) ==> Right(
            Vector(("test", Build.current))
          )
        } finally reader.close()
      } finally engine.close()
    }

    test("every session a reader opens is read-only in Postgres itself") {
      val config = TestPostgres.freshDatabase("reader_read_only")
      LiveEngine.open(config, "test").close()
      val reader = Reader.open(config)
      try {
        // Through a connection the driver was not told is read-only: only the server refuses.
        reader.db.read { (tx: Tx^) ?=>
          val conn: java.sql.Connection^{tx} = Tx.connection(tx)
          conn.setReadOnly(false)
          Using.resource(conn.createStatement()) { st =>
            Using.resource(st.executeQuery("SHOW transaction_read_only")) { rs =>
              Right(Option.when(rs.next())(rs.getString(1)))
            }
          }
        } ==> Right(Some("on"))
        reader.db.read { (tx: Tx^) ?=>
          val conn: java.sql.Connection^{tx} = Tx.connection(tx)
          Using.resource(conn.createStatement()) { st =>
            Right(
              st.executeUpdate(
                "INSERT INTO grit.engine_starts (started_at, machine, pid, epoch) VALUES (now(), 'm', 1, 'e')"
              )
            )
          }
        } match {
          case Left(StoreError.DatabaseError(why)) =>
            assert(why.contains("cannot execute INSERT in a read-only transaction"))
          case other => throw new java.lang.AssertionError(s"written: $other")
        }
      } finally reader.close()
    }
  }
}
