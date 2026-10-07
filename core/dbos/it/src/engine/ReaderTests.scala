package grit.dbos.engine

import java.time.Instant
import java.util.concurrent.ConcurrentLinkedQueue

import scala.annotation.unused
import scala.concurrent.duration.*
import scala.util.Using

import grit.core.durable.Durable
import grit.core.id.{PeriodRef, PeriodSeq, PrincipalId, SourceId, TriageRef, TurnSeq, WorkflowId}
import grit.core.message.Message
import grit.core.model.{Assignment, Catalog, ModelId, ModelRef, Policy}
import grit.core.store.{Origin, StoreError, Tx}
import grit.core.triage.Tags
import grit.core.visibility.{Subject, Visibility}
import grit.dbos.internal.Reader
import grit.dbos.sql.{LiveDb, SqlEntryStore, SqlModelProfileStore, TestPostgres}

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
    test(
      "a reader reads what the engine wrote, beside it: entries, periods, tags, a workflow, starts"
    ) {
      val config = TestPostgres.freshDatabase("reader_reads")
      val ran = new ConcurrentLinkedQueue[String]()
      def triage(id: WorkflowId)(using @unused d: Durable^): String = {
        ran.add(WorkflowId.value(id)); "triaged"
      }
      val engine = LiveEngine.open(config, "test")
      try {
        engine.launch(nothing, nothing, nothing, nothing, triage, LiveEngine.Unplaced, Vector.empty)
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
        val reader = Reader.open(config, Visibility.Shipped)
        try {
          reader.all.read(reader.entries.list(c)) ==> engine.db.read(Subject.Public)(
            engine.entries.list(c)
          )
          reader.all.read(reader.periods.all(c)).map(_.map(_.ref)) ==> Right(Vector(ref.period))
          reader.all
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

    test(
      "a reader lists the turn workflows created before a time, oldest first, and each one's steps with their outputs in the order run"
    ) {
      val config = TestPostgres.freshDatabase("reader_turns")
      def turn(@unused id: WorkflowId)(using d: Durable^): String = {
        val first = d.step("first")(() => "one")
        val _ = d.patch("a-patch")
        d.step("second")(() => s"$first and two")
      }
      val engine = LiveEngine.open(config, "test")
      try {
        engine.launch(turn, nothing, nothing, nothing, nothing, LiveEngine.Unplaced, Vector.empty)
        def ask(session: String): WorkflowId = {
          val ref = right(
            engine.inbox
              .ingest(
                Origin.Task("reader", session),
                SourceId("m"),
                Message.User("hi"),
                PrincipalId.Local
              )
              .left
              .map(e => StoreError.Invalid(e.toString))
          )
          engine.inbox.startTurn(ref) ==> Right(())
          val _ = engine.awaitTurn(ref)
          ref.workflowId
        }
        val asked = Instant.now().minusMillis(1)
        val older = ask("older")
        val answered = Instant.now().plusMillis(1)
        val newer = ask("newer")
        val reader = Reader.open(config, Visibility.Shipped)
        try {
          val until = Instant.now().plusSeconds(60)
          reader.turns(until).map(_.map(_._1)) ==> Right(Vector(older, newer))
          reader.turns(Instant.EPOCH) ==> Right(Vector.empty)
          val steps = right(reader.steps(older))
          steps.map(s => (s.name, s.output)) ==> Vector(
            ("first", Some("one")),
            ("DBOS.patch-a-patch", None),
            ("second", Some("one and two"))
          )
          // Each step's start, as DBOS journaled it: while the turn ran, in the order run.
          val started = steps.flatMap(_.started)
          started.size ==> 3
          started.filter(t => t.isBefore(asked) || t.isAfter(answered)) ==> Vector.empty
          started ==> started.sorted
          reader.steps(WorkflowId("never")) ==> Right(Vector.empty)
        } finally reader.close()
      } finally engine.close()
    }

    test("a reader reads the profile a turn was pinned to, by the turn and by its id") {
      val config = TestPostgres.freshDatabase("reader_profiles")
      val ref =
        ModelRef(ModelId.of("a/pinned").getOrElse(throw new java.lang.AssertionError()), None)
      val a = Assignment(ref, 100, None)
      val pinned = Catalog.of(Policy(a, a, a, a), Vector.empty).pin
      val engine = LiveEngine.open(config, "test")
      try {
        LiveDb.transaction(config)(
          new SqlModelProfileStore().pin(WorkflowId("w-pinned"), pinned)
        ) ==> Right(())
        val reader = Reader.open(config, Visibility.Shipped)
        try {
          reader.all.read(reader.profiles.of(WorkflowId("w-pinned"))) ==> Right(Some(pinned))
          reader.all.read(reader.profiles.get(pinned.id)) ==> Right(Some(pinned))
        } finally reader.close()
      } finally engine.close()
    }

    test("every session a reader opens is read-only in Postgres itself") {
      val config = TestPostgres.freshDatabase("reader_read_only")
      LiveEngine.open(config, "test").close()
      val reader = Reader.open(config, Visibility.Shipped)
      try {
        // Through a connection the driver was not told is read-only: only the server refuses.
        reader.all.read { (tx: Tx^) ?=>
          val conn: java.sql.Connection^{tx} = Tx.connection(tx)
          conn.setReadOnly(false)
          Using.resource(conn.createStatement()) { st =>
            Using.resource(st.executeQuery("SHOW transaction_read_only")) { rs =>
              Right(Option.when(rs.next())(rs.getString(1)))
            }
          }
        } ==> Right(Some("on"))
        reader.all.read { (tx: Tx^) ?=>
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
