package grit.dbos.engine

import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}

import scala.annotation.unused
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import grit.core.durable.Durable
import grit.core.id.{
  CloseRef,
  EntryId,
  PeriodRef,
  PeriodSeq,
  PrincipalId,
  SourceId,
  TurnRef,
  TurnSeq,
  WorkflowId
}
import grit.core.message.Message
import grit.core.period.{LifecycleSettings, Probability, Windows}
import grit.core.place.Locality
import grit.core.store.{Entry, Origin, Payload}
import grit.dbos.sql.{DbConfig, LiveDb, SqlLifecycleStore, TestPostgres}

import dev.dbos.transact.DBOSClient
import utest.*

/** The close workflow and the sweep that enqueues it, over DBOS against a real Postgres,
  * with stand-in bodies: what DBOS guarantees them, not what a close does.
  */
object CloseLiveTests extends TestSuite {

  /** A minute idle. */
  private def minuteIdle(config: DbConfig): Unit = {
    val settings = Windows
      .of(1.minute, 1.day, 1.day)
      .flatMap(LifecycleSettings.of(_, 4096, 30.seconds, Probability.One, 1, Locality.Default))
      .getOrElse(sys.error("settings"))
    LiveDb.transaction(config)(new SqlLifecycleStore().set(settings))
    ()
  }

  private def ingested(engine: Engine^, origin: Origin, text: String): TurnRef =
    engine.inbox
      .ingest(origin, SourceId(text), Message.User(text), PrincipalId.Local)
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

  /** Whether DBOS has `id` and it has ended. */
  private def ended(config: DbConfig, id: WorkflowId): Boolean = {
    val client = new DBOSClient(config.jdbcUrl, config.user, config.password)
    try
      Option(client.retrieveWorkflow[String, Exception](WorkflowId.value(id)).getStatus())
        .exists(s => !s.status().isActive())
    finally client.close()
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
      val engine = LiveEngine.open(config, "test")
      try {
        engine.launch(
          turn,
          close,
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          Vector.empty
        )
        minuteIdle(config)
        val t0 = ingested(engine, Origin.Task("close", "waits"), "one")
        engine.inbox.startTurn(t0) ==> Right(())
        assert(eventually(events.contains("turn started")))
        engine
          .sweep(Instant.now().plusSeconds(120))
          .map(_.enqueued.map(a => (a.period, a.last))) ==>
          Right(Vector((PeriodRef(t0.conversationId, PeriodSeq.First), TurnSeq(0))))
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
      val engine = LiveEngine.open(config, "test")
      try {
        engine.launch(
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          close,
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          Vector.empty
        )
        minuteIdle(config)
        val t0 = ingested(engine, Origin.Task("close", "once"), "one")
        engine.sweep(Instant.now()) ==> Right(Swept.nothing)
        engine
          .sweep(Instant.now().plusSeconds(120))
          .map(_.enqueued.map(a => (a.period, a.last))) ==>
          Right(Vector((PeriodRef(t0.conversationId, PeriodSeq.First), TurnSeq(0))))
        engine.sweep(Instant.now().plusSeconds(121)).map(_.enqueued) ==> Right(Vector.empty)
        assert(eventually(runs.size == 1))
      } finally engine.close()
    }

    test(
      "an attempt whose deadline a running turn's entries moved is followed by one under a new id, the first kept"
    ) {
      val config = TestPostgres.freshDatabase("close_moved")
      val release = new CountDownLatch(1)
      val closes = new ConcurrentLinkedQueue[String]()
      def turn(id: WorkflowId)(using @unused d: Durable^): String = {
        release.await(30, TimeUnit.SECONDS)
        WorkflowId.value(id)
      }
      // Never seals, as a close whose deadline moved does not.
      def close(id: WorkflowId)(using @unused d: Durable^): String = {
        closes.add(WorkflowId.value(id)); "abandoned: its deadline moved"
      }
      val engine = LiveEngine.open(config, "test")
      try {
        engine.launch(
          turn,
          close,
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          Vector.empty
        )
        minuteIdle(config)
        val t0 = ingested(engine, Origin.Task("close", "moved"), "one")
        engine.inbox.startTurn(t0) ==> Right(())
        val first = engine.sweep(Instant.now().plusSeconds(120)).map(_.enqueued)
        val replied = Instant.now().plusSeconds(60)
        assert(first.map(_.size) == Right(1))
        // The running turn writes its reply a minute on: the period's newest activity moves.
        engine.jot.write {
          for {
            next <- engine.entries.lockNext(t0.conversationId)
            _ <- engine.entries.insert(
              Entry(
                EntryId("reply"),
                t0.conversationId,
                t0.turnSeq,
                None,
                next.seq,
                Payload.Summary("replied"),
                replied
              )
            )
          } yield ()
        } ==> Right(())
        release.countDown()
        assert(eventually(closes.size == 1))
        val second = engine.sweep(Instant.now().plusSeconds(240)).map(_.enqueued)
        // The idle window after the reply, as Postgres keeps its time: to the microsecond.
        second ==> Right(
          Vector(
            CloseRef(
              PeriodRef(t0.conversationId, PeriodSeq.First),
              t0.turnSeq,
              replied.truncatedTo(ChronoUnit.MICROS).plusSeconds(60)
            )
          )
        )
        assert(eventually(closes.size == 2))
        closes.asScala.toVector.distinct.size ==> 2
        val client = new DBOSClient(config.jdbcUrl, config.user, config.password)
        try
          first.map(
            _.map(a =>
              client
                .retrieveWorkflow[String, Exception](WorkflowId.value(a.workflowId))
                .getStatus() != null
            )
          ) ==>
            Right(Vector(true))
        finally client.close()
      } finally engine.close()
    }

    test("an attempt that failed is not run again while its deadline stands, however many sweeps") {
      val config = TestPostgres.freshDatabase("close_failed")
      val runs = new ConcurrentLinkedQueue[String]()
      // Never seals, as a close whose seal fails does not.
      def close(id: WorkflowId)(using @unused d: Durable^): String = {
        runs.add(WorkflowId.value(id)); "failed: the database refused the seal"
      }
      val engine = LiveEngine.open(config, "test")
      try {
        engine.launch(
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          close,
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id),
          Vector.empty
        )
        minuteIdle(config)
        ingested(engine, Origin.Task("close", "failed"), "one")
        val later = Instant.now().plusSeconds(120)
        val failed = engine.sweep(later).map(_.enqueued)
        failed.map(_.size) ==> Right(1)
        assert(eventually(runs.size == 1))
        assert(eventually(failed.exists(_.forall(a => ended(config, a.workflowId)))))
        val attempt = engine.sweep(later).map(_.stuck)
        (1 to 3).map(n =>
          engine.sweep(later.plusSeconds(n.toLong)).map(s => (s.enqueued, s.stuck))
        ) ==>
          (1 to 3).map(_ => attempt.map((Vector.empty, _)))
        assert(attempt.map(_.size) == Right(1))
        Thread.sleep(1000)
        runs.size ==> 1
      } finally engine.close()
    }
  }
}
