package grit.dbos.engine

import java.time.Instant
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}

import scala.annotation.unused
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import grit.core.durable.Durable
import grit.core.id.{CloseRef, PeriodRef, PeriodSeq, SourceId, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.Message
import grit.core.period.{LifecycleSettings, Windows}
import grit.core.store.Origin
import grit.dbos.sql.{DbConfig, LiveDb, SqlLifecycleStore, TestPostgres}

import utest.*

/** The close workflow and the sweep that enqueues it, over DBOS against a real Postgres,
  * with stand-in bodies: what DBOS guarantees them, not what a close does.
  */
object CloseLiveTests extends TestSuite {

  /** A minute idle, a minute's grace. */
  private def minuteIdle(config: DbConfig): Unit = {
    val settings = Windows
      .of(1.minute, 1.minute, 1.day)
      .flatMap(LifecycleSettings.of(_, 3))
      .getOrElse(sys.error("settings"))
    LiveDb.transaction(config)(new SqlLifecycleStore().set(settings))
    ()
  }

  private def ingested(engine: Engine^, origin: Origin, text: String): TurnRef =
    engine.inbox
      .ingest(origin, SourceId(text), Message.User(text))
      .fold(e => sys.error(s"inbox: $e"), identity)

  /** Asks `done` until it holds, for up to 30 s, and whether it did. */
  private def eventually(done: => Boolean): Boolean = {
    val until = System.nanoTime() + 30.seconds.toNanos
    var held = done
    while (!held && System.nanoTime() < until) {
      Thread.sleep(50)
      held = done
    }
    held
  }

  val tests = Tests {
    test("a due close waits behind its conversation's running turn, and runs after it") {
      val config = TestPostgres.freshDatabase("close_waits")
      val events = new ConcurrentLinkedQueue[String]()
      val release = new CountDownLatch(1)
      def turn(id: WorkflowId)(using @unused d: Durable^): String = {
        events.add("turn started")
        release.await(30, TimeUnit.SECONDS)
        events.add("turn ended")
        WorkflowId.value(id)
      }
      def close(id: WorkflowId)(using @unused d: Durable^): String = {
        events.add("close ran")
        WorkflowId.value(id)
      }
      val engine = Engine.open(config, "test")
      try {
        engine.launch(
          turn,
          close,
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          Vector.empty
        )
        minuteIdle(config)
        val t0 = ingested(engine, Origin.Task("close", "waits"), "one")
        engine.inbox.startTurn(t0) ==> Right(())
        assert(eventually(events.contains("turn started")))
        val attempt = CloseRef(PeriodRef(t0.conversationId, PeriodSeq.First), TurnSeq(0))
        engine.sweep(Instant.now().plusSeconds(120)) ==> Right(Swept(Vector(attempt), Vector.empty))
        Thread.sleep(2000)
        events.asScala.toVector ==> Vector("turn started")
        release.countDown()
        assert(eventually(events.contains("close ran")))
        events.asScala.toVector ==> Vector("turn started", "turn ended", "close ran")
      } finally engine.close()
    }

    test("a sweep enqueues each due close once, and none for a period not yet due") {
      val config = TestPostgres.freshDatabase("close_once")
      val runs = new ConcurrentLinkedQueue[String]()
      def close(id: WorkflowId)(using @unused d: Durable^): String = {
        runs.add(WorkflowId.value(id)); "ran"
      }
      val engine = Engine.open(config, "test")
      try {
        engine.launch(
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          close,
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          Vector.empty
        )
        minuteIdle(config)
        val t0 = ingested(engine, Origin.Task("close", "once"), "one")
        val attempt = CloseRef(PeriodRef(t0.conversationId, PeriodSeq.First), TurnSeq(0))
        engine.sweep(Instant.now()) ==> Right(Swept.nothing)
        engine.sweep(Instant.now().plusSeconds(120)) ==> Right(Swept(Vector(attempt), Vector.empty))
        engine.sweep(Instant.now().plusSeconds(121)).map(_.enqueued) ==> Right(Vector.empty)
        assert(eventually(runs.size == 1))
      } finally engine.close()
    }

    test("an attempt that finished with its period still due is enqueued again, under its id") {
      val config = TestPostgres.freshDatabase("close_retried")
      val runs = new ConcurrentLinkedQueue[String]()
      // Never seals: each run leaves the period open and due, as an abandoned attempt does.
      def close(id: WorkflowId)(using @unused d: Durable^): String = {
        runs.add(WorkflowId.value(id)); "abandoned"
      }
      val engine = Engine.open(config, "test")
      try {
        engine.launch(
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          close,
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          Vector.empty
        )
        minuteIdle(config)
        val t0 = ingested(engine, Origin.Task("close", "retried"), "one")
        val attempt = CloseRef(PeriodRef(t0.conversationId, PeriodSeq.First), TurnSeq(0))
        val later = Instant.now().plusSeconds(120)
        engine.sweep(later) ==> Right(Swept(Vector(attempt), Vector.empty))
        assert(eventually(runs.size == 1))
        assert(eventually(engine.sweep(later) == Right(Swept(Vector.empty, Vector(attempt)))))
        assert(eventually(runs.size == 2))
        runs.asScala.toVector.distinct ==> Vector(WorkflowId.value(attempt.workflowId))
      } finally engine.close()
    }
  }
}
